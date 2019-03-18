#' Perfect match score
#' 
#' @description Perfect score - it is used for splitting match results 
#'   into perfect and thise which require verification
#' 
#' @export
match.score.perfect <- 1

#' Match threshold
#' 
#' @description threshold score - it is used for filtering out all match results
#'   which are below thresold level. 
#' 
#' @export
match.score.threshold <- .8


#' Match loanbook and asset-level data
#' 
#' @description function that matches loanbook data (prepared for matching) 
#'   and asset level data (prepared)
#' @return function returns bridge file produced by \link{matchNamesByBlocks}
#'   function
#' 
#' @export
#' @param lbk4match data frame prepared with \link{process.prepLBK4match} 
#'   function
#' @param ald4match data frame prepared with \link{process.prepALD4match}
#'   function
process.matchLBKandALD <- function(lbk4match, ald4match) {
  matchNamesByBlocks(
    names      = lbk4match$Match.Value, 
    nblocks    = lbk4match$Sector.Classification,
    dictionary = ald4match$Match.Value, 
    dblocks    = ald4match$Sector.Classification, 
    threshold = .8
  )
}

#' Flag perfect matches
#' 
#' @description Function takes match scores and returns logical vector of 
#'   perfect matches which can be used further without manual verification
#' 
#' @export
#' @param scores vector with match scores produced by \link{process.matchLBKandALD}
#'   function (which contains \code{Score} column)
process.flagPerfectMatches <- function(scores) {
  scores == 1
}

#' Extract non-perfect matches
#' 
#' @description function takes all matches produced by \link{process.matchLBKandALD} 
#'   function and flags only matches which require verification
#' 
#' @export
#' @param names vector of names (\code{Names} column in match results data frame)
#' @param sectors vector of sectors (\code{Segment} column)
#' @param scores vector of scores (\code{Score} column)
process.flagNonPerfectMatches <- function(names, sectors, scores) {
  pft <- process.flagPerfectMatches(scores)
  
  filterVec <- `%in%`(
    paste0(names, '#', sectors),
    paste0(names, '#', sectors)[pft]
  )
  # invert vector
  filterVec == FALSE
}
