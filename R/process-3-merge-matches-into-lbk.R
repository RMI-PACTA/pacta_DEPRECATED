#' Merge matching results into Loanbook
#' 
#' @description Link each record of Loanbook to ALD data based on matches
#' produced by matching egine. Merging is done on several levels: 
#' Assets, Borrowers, Ultimate Parents.
#' Loanbook assets can be matched to Assets, Owners, Parents (considering priority). 
#' Loanbook loan-takers (Borrowers) can be matched to Owners or Parents. 
#' Loanbook Ultimate parents can be matched to ALD parents only.
#' During data merge function follows priority of ALD levels: 
#' 1) Assets, 2) Owners 3) Parents.
#' 
#' @return Original loanbook dataframe plus 3 groups of columns related to 
#'   Asset, Borrower, Ultimate Parent level from ALD side. 
#' Each group of columns consist of: ID, Level, Name. Columns of each group have
#' their own prefixes: Asset.link.to; DO.link.to; UP.link.to
#' 
#' @export
#' @param loanbook - dataframe with loanbook data in stadard format 
#'   see \link{data.dataset.req.attrs} requirements in part of Loanbook
#' @param matches - dataframe with match results
process.mergeMatches2LBK <- function(loanbook, matches) {
  # pick matches for LBK Assets
  match.at <- pickBestMatches(
    matches,
    local.level = "Asset", 
    ald.levels  = c("Asset", "Company"),
    first.dupl  = F
  )
  colnames(match.at) <- c(
    "Asset.ID", 
    "AT.link.to.Level", 
    "Borrower.Sector",
    "AT.link.to.ID",
    "AT.link.to.Name"
  )
  # pick matches for LBK Borrowers
  match.do <- pickBestMatches(
    matches, 
    local.level = "Company", 
    ald.levels  = "Company"
  )
  colnames(match.do) <- c(
    "Borrower.ID", 
    "BR.link.to.Level", 
    "Borrower.Sector",
    "BR.link.to.ID",
    "BR.link.to.Name"
  )

  # pick matches for LBK Economic Ultimate Parents
  match.up <- pickBestMatches(
    matches, 
    local.level = "Company", 
    ald.levels  = "Company"
  )
  colnames(match.up) <- c(
    "Ultimate.Parent.ID", 
    "UP.link.to.Level", 
    "Borrower.Sector",
    "UP.link.to.ID",
    "UP.link.to.Name"
  )
  
  # merge resuts
  merged <- 
    loanbook %>%
    merge(
      y  = match.at,
      by = c("Asset.ID","Borrower.Sector"), 
      all.x = T
    ) %>%
    merge(
      y  = match.do,
      by = c("Borrower.ID","Borrower.Sector"), 
      all.x = T
    ) %>%
    merge(
      y  = match.up,
      by = c("Ultimate.Parent.ID","Borrower.Sector"), 
      all.x = T
    ) 
  
  merged
}

#' Pick best matches
#' 
#' @description Function pickes best matches from matches data frame provided.
#' "Best" is defined based on order of "Level.global" values provided in 
#' `ald.levels` vector. 
#' 
#' @return dataframe with columns 
#' "ID.local", "Level.global", "ID.global", "Original.Value.global"
#' 
#' @export
#' @param matches - data frame with match results
#' @param local.level - which local level of loanbook data is gouing to be merged
#' @param ald.levels - vector of ALD data level in order of their priority
#' @param first.dupl - how to treat first duplicate: 
#'   TRUE - keep it; FALSE - exclude duplicates completely
pickBestMatches <- function(
  matches, 
  local.level, 
  ald.levels,
  first.dupl = T
) {
  # filter matches by local level
  onelev <- subset(
    matches, 
    Level.local == local.level,
    colnames(empty.bestMatches())
  )
  # decode how to treat duplicates
  duplicate.fun <- if (first.dupl) duplicated else dup.test
  # pick match resutls in the order defined by ald.levels, deduplicate per level
  ordered <- 
    Map(
      f = function(x) {
        onelev %>% 
          subset(Level.global == x & Sector.Classification.global == Sector.Classification.local) %>% 
          subset(!duplicate.fun(paste(ID.local, Sector.Classification.local, sep = "#")))
      },
      ald.levels
    )
  # combine results into one data frame and remove low-prio duplicates
  df <-
    Reduce(
      f = rbind, 
      x = ordered, 
      init = empty.bestMatches()
    ) %>%
    unique() %>%
    subset(
      !duplicated(paste(ID.local, Sector.Classification.local, sep = "#"))
    ) %>%
    select(
      ID.local,
      Level.global,
      Sector.Classification.local,
      ID.global,
      Original.Value.global
    )
  df
}

empty.bestMatches <- function() {
  data.frame(
    ID.local = character(),
    Level.global = character(),
    Sector.Classification.local = character(),
    Sector.Classification.global = character(),
    ID.global = character(),
    Original.Value.global = character(),
    stringsAsFactors = F
  )
}
