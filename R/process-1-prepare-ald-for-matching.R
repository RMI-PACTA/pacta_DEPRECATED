#' Prepare ALD data for matching
#' 
#' @description function that prepares asset level data for matching. It 
#' takes assets from ald database and produces dataframe of universal 
#' structure for matching.
#' 
#' @return data.frame with column set: 
#' Level, ID, OriginalValue, MatchValue, SectorClassification
#' 
#' @export
#' @param ald - asset level data according to structure returned by
#'   \link{empty.ALD4mathcing} function
process.prepALD4match <- function(ald) {
  # assets and companies
  all <- ald %>%
    subset(
      select = c("ALD.link.Level", "ALD.Name", "Sector")
    ) %>%
    unique()
  
  addALD4matching(
    levels     = all$ALD.link.Level,
    ids        = all$ALD.Name, 
    values     = all$ALD.Name, 
    sectors    = all$Sector,
    simplify = T
  )

}

#' Prepare dataset for matching
#' 
#' @param levels - Level at which matching is performed (e.g. "Direct Owner", "Ultimate Economic Parent" etc.)
#' @param ids - vector with cusomer ID
#' @param values - vector with values to be matched
#' @param sectors - vector with sector sclassification 
#' @param simplify - logical flag if names simplification should be applied (use only for names)
addALD4matching <- function(
  levels, ids, values, sectors, 
  simplify = FALSE
) {
  match.values <- if (simplify == FALSE) {
    values
  } else {
    simplifyName(values)
  }
  df <- data.frame(
    Level = levels,
    ID = ids,
    Original.Value = values,
    Match.Value = match.values,
    Sector.Classification = sectors,
    stringsAsFactors = F
  )
  unique(df)
}

#' Empty asset-level data frame for mathcing
#' 
#' @description Function defines data frame structure for asset-level data used in 
#' matching.
#' 
#' @export
empty.ALD4mathcing <- function() {
  data.frame(
    Source                = character(),
    Level                 = character(),
    ID                    = character(),
    Original.Value        = character(),
    Match.Value           = character(),
    Sector.Classification = character(),
    stringsAsFactors = F
  )
  
}
#------------------------------------------------------------------------------#
