library(shinythemes)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(chorddiag)

INSPIRE_subs <- c(
  "All",
  "Theory-HEP",
  "Math and Math Physics",
  "Phenomenology-HEP",
  "Gravitation and Cosmology",
  "Astrophysics",
  "Lattice",
  "Theory-Nucl",
  "Experiment-HEP",
  "Experiment-Nucl",
  "Accelerators",
  "Instrumentation",
  "General Physics",
  "Computing",
  "Other"
)

navbarPage(
  theme = shinythemes::shinytheme("sandstone"),
  "HEP Hiring Networks",
  tabPanel(
    "Heat Map",
    div(class="outer",
        
        tags$head(
          includeCSS("styles.css")
        ),
        
        leafletOutput("map", width="100%", height="100%"),
        
        absolutePanel(id = "search", class = "panel panel-default", fixed = TRUE,
                      top = 80, left = 45, right = "auto", bottom = "auto",
                      width = 330, height = 20,
                      textInput('instit', label=NULL, 
                                placeholder = 'Search institution by city or name (e.g. Harvard)', width = '100%')
        ),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      top = 135, left = 45, right = "auto", bottom = "auto",
                      width = 330, height = "auto", draggable = TRUE,
                      radioButtons('display_mode', 'Output mode:', 
                                   c("Outflow" = "Next", "Inflow" = "Previous"),
                                   inline = TRUE
                      ),
                      sliderInput("date_range", "Date range:", min = 1930, max = as.integer(format(Sys.Date(), "%Y")),
                                  value = c(1960, as.integer(format(Sys.Date(), "%Y"))), step = 1, round = TRUE, sep = ""
                      ),
                      uiOutput('instit_control'),
                      selectInput('INSPIRE_subject_instit', 'Filter by Subject:', INSPIRE_subs, multiple = TRUE,
                                  selectize = FALSE, selected = "All", size = 5),
                      hr(),
                      checkboxInput('show_table', 'Show table with results'),
                      checkboxInput('show_help', 'Show help')
        ),
        conditionalPanel("input.show_table", 
                         absolutePanel(id = "output_table", class = "panel panel-default", fixed = TRUE,
                                       top = 135, left = 380, right = "auto", bottom = "auto",
                                       width = 1000, height = "auto", draggable = TRUE,
                                       div(
                                       dataTableOutput('matches_table'),
                                       style = "font-size: 80%; width: 100%"
                                       )
                          )
        ),
        conditionalPanel("input.show_help", 
                         absolutePanel(id = "help", class = "panel panel-default", fixed = TRUE,
                                       top = 200, left = 640, right = "auto", bottom = "auto",
                                       width = 431, height = "auto", draggable = TRUE,
                                       h3('Help'),
                                       div(
                                       p(HTML(paste('Start by',
                                          tags$b('searching'), 
                                         'the city
                                         or institution you are interested in. The map can display either the', 
                                         tags$b('outflow,'),
                                         'showing where scientists working at the institution went next in their career,
                                         or the',
                                         tags$b('inflow,'),
                                         'showing where they worked previously.'))),
                                       p('You can', tags$b('filter'), 'the results by Date and Subject area, and refine the
                                         input institution by selecting from a list. Select multiple options
                                         by holding down the Cmd/Ctrl key. Search accepts the', tags$b('OR') ,
                                          'operator, e.g. "Harvard OR Berkeley".
                                         '),
                                       p('The output is
                                         a', tags$b('heat map,'), 'with warmer-colored circles marking locations with larger
                                         number of results (see legend in the bottom-right corner). The',
                                         tags$b('table'), 'with results contains the data underlying the heat map.
                                         ')
                                       )
                         )
        )
    )
    
  ),
  tabPanel(
    "Chord diagrams",
    
    fluidRow(
      column(6, align = 'center',
        h1('Chord diagrams'),
        div(
        p('A nice compact way to visualize the flows of scientists between the different institutions 
           is to use chord diagrams. In these diagrams, the chord\'s width at a given node is proportional 
           to the outflow from the node. One can visualize either the outflow only, or, by including each node
           twice, both the inflow and outflow.
          ')
        ),
        offset = 3
      )
    ),
    fluidRow(
      column(6,
             h2('Top 25 places in Theory-HEP vs rest'),
             p('This diagram shows the faculty hiring network in Theory-HEP.* A flow from A to B
                represents researchers with PhD from A who became faculty at B. The places are
                ordered by Minimum Violations Ranking in clockwise direction starting at the top
                (see "About" tab for more on the ranking). 
               '),
             offset=3
      )
    ),
    fluidRow(
      column(3,
             radioButtons('display_mode_chord_theory', 'Show:', 
                          c("Outflow only" = "directional", "Inflow/Outflow" = "bipartite"),
                          inline = TRUE
             ),
             offset=3
      )
    ),
    fluidRow(
      chorddiagOutput('chorddiag_theory', height = '800px')
    ),
    fluidRow(
      column(6,
             p('The diagram reveals, for instance, that the top 25 places (20% of all) generate
               50% of the faculty in Theory-HEP (corresponding to how much of the circle perimeter they span).
               Looking at the individual locations also shows sizeable differences. For example
               61% of PhDs from Pasadena, US (Caltech) become faculty at one of the top 25 places,
               while only 19% do so from La Plata, AR. About 31% of PhDs from the lower ranked locations
               become professors at the top 25.
               '),
             
               p('*Note: We infer the PhD and faculty institutions only indirectly, based on 
                  publication affiliation data. We define the PhD institution to be the affiliation of the
                  first publications, and the faculty institution that of the last publications. Furthermore,
                  we consider only researchers who have a publication record of at least twelve years.
                 ', style='font-size:11px'),
             
             offset=3
             )
      ),
    fluidRow(
      column(6,
             h2('Top 25 places vs rest'),
             p('Here we show the diagram for all subject areas and without restricting the
                flows to PhD -> faculty as above. The places are now ordered by decreasing outflow.
               '),
             offset= 3
             )
    ),
    fluidRow(
      column(3,
             selectInput(
               'INSPIRE_subject_chord',
               'INSPIRE Subject:',
               INSPIRE_subs
             ),
             offset = 3
      ),
      column(3,
             radioButtons('display_mode_chord', 'Show:', 
                          c("Outflow only" = "directional", "Inflow/Outflow" = "bipartite"),
                          inline = TRUE
             )
      )
    ),
    fluidRow(
      chorddiagOutput('chorddiag', height = '800px')
    ),
    fluidRow(
      column(6,
             p('We can again observe significant differences between the different locations. For example, 
                considering all subject areas together, we see that while 51% of the researchers who worked 
                in New York, US had their next career stage among the top 25 places, in Beijing, CN this
                was merely 13%. Of course, these relative numbers, and the ordering of locations, vary
                by subject area.
               '),
             offset=3
            )
    )
  ),
  tabPanel(
    "About",
    column(6,
      h2('In short'),
      p('This interactive app helps one understand the hiring networks and researchers\' mobility in the 
         field of High Energy Physics (HEP). It is based on publication metadata from the open database',
        a("INSPIRE-HEP.", href="http://inspirehep.net" ,target="_blank")
      ),
      p('Based on the data, we have determined the career paths of around 55k researchers, 
         which we use for the study of mobility patterns between different institutions. 
         The data processing scripts in Python and this app in R are available at',
        (a("github.", href="https://github.com/daniel-pl", target="_blank"))
        ),
      h2('Career paths'),
      p('Scientists tend to change their workplace frequently, especially in early years of their careers.
         In many fields, including HEP, a PhD graduate typically goes through a series of
         postdoctoral positions before landing a permanent faculty position. Each position is usually taken at a different 
         institution to promote scientific exchange and exposure to different research environments.
        '),
      p('For young researchers planning their careers it is natural to ask if a PhD/postdoc position at
         a given institution will advance their career and increase their chances of eventually becoming
         full professors. Unfortunately, apart from subjective experiences and
         projections, there are not enough studies of patterns in researchers\' mobility and hiring
         that would enable one to assess such questions on solid grounds.
        '),
      h2('Finding data'),
      p('The main challenge in studying the issue is the shortage of suitable data. Some of the few existing works
        on the subject have used either hand-curated data from university databases', a('[1],', href="#ref1"), 
        a('[2],', href="#ref2"), 'which are by default not publicly available, or user-provided CV data from the 
        open ORCID database', a('[3],', href="#ref3"), 'which are available and up-to-date only for active users.'
      ),
        
      p('This work takes another approach by making use of the metadata that is already available in research publications.
         The open literature database INSPIRE-HEP is particularly well-suited for the study, as it lists
         the vast majority of publications in HEP from the last 50 years, and provides easy access to the data. 
         To the best of our knowledge, this is the first study using such an approach.
        '),
      h2('Analysis'),
      p('We acquired the metadata from the monthly', a('INSPIRE-HEP dump,', href="http://inspirehep.net/dumps/inspire-dump.html",
                                                        target="_blank"),
        'containing more than 1 million publication records. An average publication has 11 authors, resulting in
         11 million unique author signatures. From each we extracted the relevant information, most importantly
         the author, publication date, affiliation, physical address, and subject area. A fraction of the signatures had
         to be discarded because of missing affiliation data or author ID, leaving 7 million usable signatures.
        '),
      p('For each author we thus obtained a series of affiliations with a timestamp. Based on this we built a continuous
         model of each author\'s career path. There are several challenges in doing this, for example, some authors 
         having multiple affiliations at a time, different names for (virtually) the same institution, short stays, and 
         occasional errors in data entries. To deal with these problems, we first clustered the authors\' affiliations 
         by geographic location* and date**, so that each cluster represents an extended stay in a given location. Each 
         publication date is then associated with one or several clusters (stays), and using multinomial logistic regression, 
         we could estimate the time period of each stay.
        '),
      p('Knowing the career path of all researchers allows us to study their flows between the different locations.
         The heat map in this app allows one to visualize the inflow and outflow for every institution in the
         database. The chord diagrams offer another view on the data, making the interconnections between the institutions
         better visible.
        '),
      h2('Rankings'),
      p('It is natural to expect that some hierarchy of institutions will emerge from the flow
         patterns, with institutional prestige playing a significant role in shaping ones career path. This issue 
         was recently studied in', a('[1],', href="#ref1"),' discovering a steep hierarchy in faculty hiring 
         networks among US universities, with the prestige of a researcher\'s PhD institution strongly influencing 
         at which university they become full professors. To determine the hierarchy, the authors used a minimum 
         violations ranking, which is a ranking that minimizes the flow up the hierarchy. 
        '),
      p('There exists several methods to compute minimum violations rankings', a('[4],', href="#ref4"), 
         a('[5].', href="#ref5"), 'We adopted the method of Markov Chain Monte Carlo sampling
         from', a('[1],', href="#ref1"), 'and the authors\' implementation provided in', a('[6].', href="#ref6"),'
         Using this method we determined the institutional rankings in faculty hiring, and as in', a('[1]', href="#ref1"),
        'we generated the resulting chord diagrams displaying the hiring networks between the institutions. 
        '),
      h2('Outlook'),
      p('The data at hand offer many more opportunities for exploration. Among the topics we wish to 
         study further are: community detection among institutions, markovian character of career paths
         (to what extent only the last institution determines the next), flows on regional and continental levels,
         and general statistics on mobility in the field. It would be also interesting to extend this study
         to other fields of research, or to job markets outside of academia (for example by making use
         of data available on career websites like Linkedin).
        '),
      p('Author: Daniel Plencner
        '),
      h2('Notes'),
      p('*We acquired the geographic locations from the', a("Google Maps API", href = "https://developers.google.com/maps/",
                                                            target="_blank"), 
        'based on city name.
        ', style='font-size:11px'),
      p('**For the clustering we used the DBSCAN algorithm, clustering together institutions less 
         than 50km away from each other, and less than 1.5 years apart in the author\'s affiliation history.
        ', style='font-size:11px'),
      h2('References'),
      p('[1] A. Clauset, S. Arbesman, D. Larremore,', tags$i('\"Systematic inequality and hierarchy 
         in faculty hiring networks\",'), a('Science Advances 1 (2015), e1400005.', href = "http://dx.doi.org/10.1126/sciadv.1400005",
                                            target="_blank"), 
        id="ref1"),
      p('[2] ', tags$i('\"Academics without borders\",'), a('MIT News.', href = "http://news.mit.edu/2017/academics-without-borders-mit-world-0614",
                                                     target="_blank"),
        id="ref2"),
      p('[3] J. Bohannon,', tags$i('\"Vast set of public CVs reveals the world’s most migratory scientists\",'), 
        a('Science May 18, 2017.', href = "http://dx.doi.org/10.1126/science.aal1189",
          target="_blank"),
        id="ref3"),
      p('[4] I. Ali, W. Cook, M. Kress,', tags$i('\"On the Minimum Violations Ranking of a Tournament\",'), 
        a('Management Science 32 (1986) 660.', href = "http://dx.doi.org/10.1287/mnsc.32.6.660",
          target="_blank"),
        id="ref4"),
      p('[5] K. Pedings, A. Langville, Y. Yamamoto,', tags$i('\"A minimum violations ranking method\",'), 
        a('Optimization and Engineering 13 (2012) 349.', href = "http://dx.doi.org/10.1007/s11081-011-9135-5",
          target="_blank"),
        id="ref5"),
      p('[6]',
        a('http://tuvalu.santafe.edu/~aaronc/facultyhiring/', href = "http://tuvalu.santafe.edu/~aaronc/facultyhiring/",
          target="_blank"),
        id="ref6"),
      offset = 3
    )
  )
)