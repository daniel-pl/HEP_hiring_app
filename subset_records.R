library(stringr)

quotemeta <- function(string) {
  # Make all strings fixed (not regular expressions).
  str_replace_all(string, "(\\W)", "\\\\\\1")
}

date_ym <- function(d){
  # Change date to "%Y-%m" format. 
  format(as.Date(d), "%Y-%m")
}

date_ym_dt <- function(dt){
  # Change date to "%Y-%m" format in a dataframe.
  date_cols <- c('StartDate_0', 'EndDate_0')
  dt <- dt[, (date_cols) := lapply(.SD, date_ym), .SDcols=date_cols]
  
  return(dt)
}

search_instit <- function(df, instit_name){
  # Search institution by name.
  if(nchar(instit_name) == 0) return(df[FALSE])
  
  if (suppressWarnings(any(!is.na(as.numeric(instit_name)))))
    return(df[as.numeric(df$recid) == as.numeric(instit_name), ])
  
  # Split institutions joined by OR operator.
  instit_name <- strsplit(instit_name, split = " OR ", fixed = TRUE)[[1]]
  if (Reduce("|", nchar(instit_name) < 3)) return(df[FALSE])
  
  # Search timelines for institution name.
  if (length(instit_name) == 1){
    instit_name <- tolower(instit_name)
    return(df[grepl(instit_name, tolower(df$InstituteNameHEPold), fixed = TRUE) |
                      grepl(instit_name, tolower(df$InstituteNameHEPnew), fixed = TRUE) |
                      grepl(instit_name, tolower(df$City), fixed = TRUE)]
    )
  }
  else {
    instit_name <- paste(quotemeta(instit_name), collapse='|')
    instit_name <- tolower(instit_name)
    return(df[grepl(instit_name, tolower(df$InstituteNameHEPold)) |
                    grepl(instit_name, tolower(df$InstituteNameHEPnew)) |
                    grepl(instit_name, tolower(df$City)), ]
    )
  }
}

filter_date <- function(dt, date_range){
  # Apply date range filter.
  from_date <- as.Date(paste(date_range[1],'-01-01', sep=""), '%Y-%m-%d')
  to_date <- as.Date(paste(date_range[2],'-12-31', sep=""), '%Y-%m-%d')
  
  dt <- dt[as.Date(dt$affiliationEndDate, '%Y-%m-%d') >= from_date & 
             as.Date(dt$affiliationStartDate, '%Y-%m-%d') <= to_date, ]
  
  return(dt)
}

filter_instit <- function(dt, instit_names){
  # Apply institution filter.
  if (!('All selected' %in% instit_names)) dt <- dt[dt$InstituteNameHEPold %in% instit_names, ]
  
  return(dt)
}


filter_subject <- function(dt, subject){
  # Apply subject filter.
  if (!("All" %in% subject)) dt <- dt[dt$INSPIRESubjectTop %in% subject, ]
  
  return(dt)
}

rename_cols_next <- function(dt){
  # Rename column names for next affiliation.
  dt <- dt[, .(InstituteNameHEPoldNext, CityNext, CountryCodeNext,
               LatitudeCityNext, LongitudeCityNext, InstituteNameHEPold,
               affiliationStartDate, affiliationEndDate, author, authorID)]
  
  setnames(dt, c('InstituteNext','CityNext', 'CountryNext', 'LatitudeCityNext', 'LongitudeCityNext', 
                 'Institute_0', 'StartDate_0', 'EndDate_0', 'author', 'authorID'))
  return(dt)
}

rename_cols_prev <- function(dt){
  # Rename column names for previous affiliation.
  dt <- dt[, .(InstituteNameHEPoldPrevious, CityPrevious, CountryCodePrevious,
               LatitudeCityPrevious, LongitudeCityPrevious, InstituteNameHEPold,
               affiliationStartDate, affiliationEndDate, author, authorID)]
  
  setnames(dt, c('InstitutePrevious','CityPrevious', 'CountryPrevious', 'LatitudeCityPrevious', 'LongitudeCityPrevious', 
                 'Institute_0', 'StartDate_0', 'EndDate_0', 'author', 'authorID'))
  return(dt)
}