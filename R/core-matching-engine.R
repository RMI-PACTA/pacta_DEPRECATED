#' String similarity
#' @description Function identifies similarity between provided string and
#'   vector of strings called dictionary.
#' @return Vector of numeric values between 0 and 1 where 0 - lowest similarity
#'   and 1 - highest. Vector length is the same as dictionary length. Similarity
#'   scores ordered in the same order as dictionary elements.
#'
#' @param string - value to compare against dictionary
#' @param dictionary - set of plausible values to chose from
stringSim <- function(string, dictionary) {
  stringdist::stringsim(string[1], dictionary, method = "jw", p = 0.1)
}

#' Find cloasest matches 
#' @description Function finds closest matches in a dictionary for the names provided
#' 
#' @return vector of indexes of dictionary entries
#' 
#' @param names - vector of names to find in disctionary
#' @param dictionary - dictionary
closestMatches <- function(names, dictionary) {
  stringdist::amatch(names, dictionary, method = "jw", p = 0.1, maxDist = 1)
}

# empty match result data frame
initMatchRes <- function() {
  data.frame(
    Name       = character(),
    Dictionary = character(),
    Score      = double(),
    Segment    = character(),
    stringsAsFactors = F
  )
}

# internal function that processes 1 name for matchNames
match1Name <- function(name, dictionary, threshold) {
  scores <- stringSim(name, dictionary)

  df <- data.frame(
    Name       = as.character(replicate(length(dictionary), name)),
    Dictionary = as.character(dictionary),
    Score      = as.double(scores),
    stringsAsFactors = F 
  )
  
  subset(df, df$Score >= threshold)
}

#' Find similar names in dictionary
#' @description Function compares given name with elements of dictionary,
#'   callculates simiarity scores and returns result in data.frame format.
#'   Matching results might be optionally filtered according to threshold value
#' @return Function returns dataframe with columns \code{Name},
#'   \code{Dictionary}, \code{Score}. Number of rows in dataframe depends on
#'   number of names and number of mathces that have a score higher than the
#'   threshold
#'
#' @export
#' @param names - name to compare against dictionary
#' @param dictionary - set of plausible values to chose from
#' @param threshold - threshold value, minimum score to filter out results with
#'   lower score
#' @param return - what values to return from c("closest", "all"). Return type 
#'   "closest" works much faster and returns only 1 match per name with highest 
#'   score.
matchNames <- function(names, dictionary, threshold = .95, return = "closest") {
  # function that matches 1 name
  matchName <- function(x) {
    matches <- match1Name(x, dictionary, threshold)
  }
  df <- if (return == "all") {
    # apply matchName to all names
    matches <- Map(f = matchName, names)
    # combine all mathes into one dataframe
    df <- Reduce(
      f = rbind,
      x = matches,
      init = initMatchRes()[, c("Name", "Dictionary", "Score")]
    )  
    df
  } else if (return == "closest") {
    mv <- closestMatches(names, dictionary)
    hit.name  <- as.character(names[!is.na(mv)])
    hit.dict  <- as.character(dictionary[mv[!is.na(mv)]])
    hit.score <- if(length(hit.name) == 0) numeric()
                 else mapply(stringSim, hit.name, hit.dict)
    df<- data.frame(
      Name       = hit.name,
      Dictionary = hit.dict,
      Score      = hit.score,
      stringsAsFactors = F
    )
    rbind(
      initMatchRes()[, c("Name", "Dictionary", "Score")], 
      subset(df, Score >= threshold)
    )
  }
  
  df
}

#' Match names using blocking
#' @description split names and dictionary values into blocks and make matching
#'   within corresponding blocks
#' @return dataframe of structure described as result of \link{matchNames}
#'   function, extended with \code{Segment} column that represents block.
#' 
#' @export
#' @param names - name to compare against dictionary
#' @param nblocks - vector that specifies blocks for names; should have the same
#'   length as names
#' @param dictionary - set of plausible values to chose from
#' @param dblocks - vector that specifies blocks for dictionary elements; should
#'   have the same length as names
#' @param threshold - threshold value, minimum score to filter out results with
#' @param return - what values to return from c("closest", "all"). Return type 
#'   "closest" works much faster and returns only 1 match per name with highest 
#'   score.
matchNamesByBlocks <- function(
  names, nblocks, dictionary, dblocks, 
  threshold = 0.95, return = "closest"
) {
  blocks <- unique(nblocks)
  blocks <- blocks[blocks %in% unique(dblocks)]
  # takes 1 segment and runs matchig for names and dictionary filtered by segment
  match1Segm <- function(block) {
    ns <- names[nblocks == block]
    ds <- dictionary[dblocks == block]
    df <- matchNames(ns, ds, threshold, return)
    df$Segment <- as.character(replicate(nrow(df), block))
    df
  } 
  # apply matches by segments
  matches <- Map(match1Segm, blocks)
  # folds result of matching names and dictionaries by segment into 1 dataframe
  df <- Reduce(
    f = rbind,
    x = matches,
    init = initMatchRes()[, c("Name", "Dictionary", "Score", "Segment")]
  )
}
