library(shiny)
library(data.table)
library(igraph)
library(leaflet)
library(leaflet.extras)
library(chorddiag)
source("my_heatmap.R")
source("subset_records.R")

timelines <- fread('./data/author_timelines.csv', sep = ',', header = TRUE)
setkey(timelines, authorID)

# Provide colors for the heat map color palette.
clrs <- c("#0000FF", "#54B6FF", "#43FFBD", "#00FF00", "#CBFF00", "#FFC200", "#FF0000")
pal <- colorNumeric(clrs, 0:1)

chord_min_deg <- 0
chord_ntop <- 25
chord_theory_rank <- fread('./data/nranks_theory.txt', header = TRUE)

shinyServer(function(input, output, session) {
  
  # Heat map functions:
  
  output$map <- renderLeaflet({
    # Show leaflet map.
    leaflet(options = leafletOptions(minZoom = 0, maxZoom = 11)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -50, lat = 25.45, zoom = 2) 
  })
  
  observe({
    # Control heat map layer settings.
    dt <- instit_matches_filtered()
    
    if (nrow(dt) == 0){
      leafletProxy("map") %>%
        clearHeatmap2() %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls()
      return()
    }
    
    if (input$display_mode == 'Next'){
      pts <- dt[!is.na(LongitudeCityNext) & !is.na(LatitudeCityNext), 
                .(lng = LongitudeCityNext, lat = LatitudeCityNext, City = CityNext)]
    } 
    else if (input$display_mode == 'Previous') {
      pts <- dt[!is.na(LongitudeCityPrevious) & !is.na(LatitudeCityPrevious), 
                .(lng = LongitudeCityPrevious, lat = LatitudeCityPrevious, City = CityPrevious)]
    } 
    
    # Compute coordinate points with frequencies.
    pts <- pts[, 'freq' := .N, by=list(lng, lat)]
    
    pts_u <- unique(pts, by=c('lng', 'lat'))
    pts_u <- pts_u[order(-freq)]
    top10 <- pts_u[1:min(10, .N)]
      
    max_freq <- max(pts$freq, na.rm = TRUE)
    
    leafletProxy("map", data = pts) %>%
      clearHeatmap2() %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      addHeatmap2(lng = ~lng, lat = ~lat,
                  blur = 1, radius = 10, gradient=clrs) %>%
      addCircles(lng= ~lng, lat = ~lat, weight = 1,
                 radius=5000, color = ~pal(freq/max_freq)) %>%
      # Show label for top 10 most frequent matches.
      addLabelOnlyMarkers(data=top10, lng = ~lng, lat = ~lat, label = ~as.character(City),
                          labelOptions = labelOptions(noHide=T, textsize="8px", opacity=0.4, textOnly=T))
    
    # Display heat map legend.
    if (max_freq > 1){
    leafletProxy("map", data = pts) %>%
      addLegend("bottomright", pal=colorNumeric(clrs, 0:max_freq), 
                values= ~freq, title = 'count', bins = min(max_freq-1, 7),
                layerId="colorLegend")
    }
  })
  
  
  output$instit_control <- renderUI({
    # Generate selectInput filtering institution matches.
    instits <- unique(instit_matches()[, .(InstituteNameHEPold, City, CountryCode)])
    instits <- instits[order(InstituteNameHEPold)]
    instits_char <- instits[, paste(InstituteNameHEPold, City, City, sep=', ')]
    instits_char <- append(instits_char, 'All', 0)
    instits_names <- setNames(append(instits$InstituteNameHEPold, 'All selected', 0), instits_char)
    selectInput('instit_filter', 'Filter input institution:', instits_names, selectize = FALSE, multiple = TRUE, 
                size=5, selected = 'All selected')
  })
  
  instit_matches <- reactive({
    # Get institution matches from text search.
    return(search_instit(timelines, input$instit))
  })
  
  output$matches_table <- renderDataTable({
    # Generate data table with results underlying the heat map.
    dt <- instit_matches_filtered()
    
    dt <- date_ym_dt(dt)
    if (input$display_mode == 'Next') {
      dt <- dt[, .(InstituteNext, CityNext, CountryNext, Institute_0,
                   StartDate_0, EndDate_0, author)]
      dt <- dt[order(-EndDate_0)]
    }
    else {
      dt <- dt[, .(InstitutePrevious, CityPrevious, CountryPrevious, Institute_0,
                   StartDate_0, EndDate_0, author)]
      dt <- dt[order(-StartDate_0)]
    }

    return(dt)
  },
  options = list(pageLength = 10, scrollY=375)
  )
  
  instit_matches_filtered <- reactive({
    # Apply filters on matches.
    dt <- instit_matches()
    
    dt <- filter_instit(dt, input$instit_filter)
    dt <- filter_subject(dt, input$INSPIRE_subject_instit)
    dt <- filter_date(dt, input$date_range)
    
    if (input$display_mode == 'Next'){
      dt <- rename_cols_next(dt[!is.na(dt$recidNext)])
    }
    else if (input$display_mode == 'Previous'){
      dt <- rename_cols_prev(dt[!is.na(dt$recidPrevious)])
    }
    
    return(dt)
  })
  
  # Chord diagram functions:
  
  output$chorddiag <- renderChorddiag({
    # Generate chord diagram from timelines data. For this diagram, the nodes are ordered by total outflow.
    if (input$INSPIRE_subject_chord == "All") 
      inst_graph <- timelines[CityNext != "" & City != "", 
                            .(paste(City, CountryCode, sep=','), paste(CityNext,CountryCodeNext, sep=','))]
    else
      inst_graph <- timelines[CityNext != "" & City != "" & INSPIRESubjectTop == input$INSPIRE_subject_chord, 
                              .(paste(City, CountryCode, sep=','), paste(CityNext,CountryCodeNext, sep=','))]
    
    # Generate graph from timelines.
    g <- graph_from_data_frame(inst_graph)
    A <- get.adjacency(g, sparse=FALSE)
    v_names <- V(g)$name
    
    # Order nodes by outflow.
    tot_deg <- rowSums(A)
    tot_deg <- tot_deg[tot_deg >= chord_min_deg]
    v_names_red <- names(sort(tot_deg, decreasing = TRUE))
    A <- A[v_names_red, v_names_red]
    
    # Separate nodes into top 25 and lower.
    Ap <- A
    Ap <- rbind(Ap[1:chord_ntop,], colSums(Ap[-(1:chord_ntop),]))
    Ap <- cbind(Ap[,1:chord_ntop], rowSums(Ap[,-(1:chord_ntop)]))
    rownames(Ap)[chord_ntop+1] <- paste('Lower', nrow(A)-chord_ntop, 'locations')
    colnames(Ap)[chord_ntop+1] <- paste('Lower', nrow(A)-chord_ntop, 'locations')
    
    chorddiag(Ap, showTicks = FALSE, groupnameFontsize = 11, type=input$display_mode_chord)
  })
  
  output$chorddiag_theory <- renderChorddiag({
    # Generate chord diagram from timelines data. For this diagram, the nodes are ordered by minimum violations
    # ranking.
    inst_graph <- timelines[CityNext != "" & City != "" & INSPIRESubjectTop == 'Theory-HEP', 
                              .(paste(City, CountryCode, sep=','), paste(CityNext,CountryCodeNext, sep=','))]
    
    # Generate graph from timelines.
    g <- graph_from_data_frame(inst_graph)
    A <- get.adjacency(g, sparse=FALSE)
    v_names <- V(g)$name
    
    # Order nodes by minimum violations ranking.
    v_names_red <- chord_theory_rank$Var1
    A <- A[v_names_red, v_names_red]
    
    # Separate nodes into top 25 and lower.
    Ap <- A
    Ap <- rbind(Ap[1:chord_ntop,], colSums(Ap[-(1:chord_ntop),]))
    Ap <- cbind(Ap[,1:chord_ntop], rowSums(Ap[,-(1:chord_ntop)]))
    rownames(Ap)[chord_ntop+1] <- paste('Lower', nrow(A)-chord_ntop, 'locations')
    colnames(Ap)[chord_ntop+1] <- paste('Lower', nrow(A)-chord_ntop, 'locations')
    
    chorddiag(Ap, showTicks = FALSE, groupnameFontsize = 11, type=input$display_mode_chord_theory)
  })
  
})