#' Check if value is empty
#' 
#' @description Function checks if provided value is NA or empty
#' @return Logical value TRUE or FALSE; if input value is vector then function
#' returns vector of logical values
#' 
#' @param x - vector of values for check
is_empty <- function(x) {
  is.na(x) | x == ""
}

#' Replace empty value with alternative
#' 
#' @description Function checks if provided value is NA and replaces it 
#' with alternative value
#' @return Either main value (if it is not NA) or alternative value
#' 
#' @param x - vector of main values
#' @param y - vector of alternative values (if main value is empty)
is_null <- function(x, y) {
  ifelse(is.na(x), y, x)
}


#' Trim whitespaces
#' 
#' @description Trim leading and trailing whitespaces from text value
#' @return character trimmed string
#' 
#' @param x - character string
trim <- function(x) trimws(x, which = "both")


#' Test vector for duplicates
#' @description Function tests if vector of values and returns another vector of
#' logical values of the same length showing if element is unique
#' @return vector of logical values, 
#'   TRUE - element present more than once, 
#'   FALSE - element is unique
#' 
#' @param x - input vector of values
dup.test<- function(x){
  x %in% x[duplicated(x)]
}

#' convert NAICS -> Sector Classification
#' 
#' @description Function takes list of NAICS codes and returns vector of 
#' corresponding Sector Classifications. Additionally dataframe with conversion
#' rules in \link{data.naics2sc.rules} format can be provided.
#' 
#' @export
#' @param naics - 6-digit NAICS Industry code
#' @param rules - dataframe with conversion rules (example: \link{data.naics2sc.rules})
convert.naics2sc <- function(naics, rules = twodii4loans::data.naics2sc.rules) {
  # prepare dataframe for use in merge operation
  x <- data.frame(
    Sector    = substr(naics, 1, 2),
    Subsector = substr(naics, 1, 3),
    Industry  = substr(naics, 1, 6), 
    stringsAsFactors = F
  )
  # find Sector Classification for specific level
  find.sc4lev <- function(level) {
    r <- subset(rules, Level == level) %>% subset(!duplicated(Code))
    m <- match(
      x = x[[level]], 
      table = r[["Code"]]
    )
    r$Sector.Classification[m]
  }
  # get sectors (3 vectors) based on Industry, Subsector, Sector levels
  matches <- Map(f = find.sc4lev, c("Industry", "Subsector", "Sector"))
  # chose best value: match by Industry is stronger than Subsector, etc.
  Reduce(f = is_null, x = matches)
}
