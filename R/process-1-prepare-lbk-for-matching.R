#' Prepare loanbook data for matching
#' 
#' @description function that prepares local loanbook data for matching. It 
#' takes loanbook and override rules and produces dataframe of universal 
#' structure for matching.
#' 
#' @return data.frame with column set: CustomerID, Level, Relation, ValueType, 
#' OriginalValue, MatchValue, SectorClassification
#' 
#' @export
#' @param loanbook - local loanbook with minimum set of required fields (see 
#' loanbook.example)
#' @param selections - list of named selections from loanbook, each selection in 
#'   list must contain following attributes: 
#'   `level` (level of mathcing); 
#'   `ids` (Column name with IDs); 
#'   `values` (Column name with values used for mathcing). 
#'   Besides this, selection may contain following optional attributes:
#'   `value.type` (type of values provided)
#'   `simplify` (flag that runs name simplification of values provided - 
#'     see \link{simplifyName} function for details)
#'   `cut.ownership` (flag that instructs simplify algorithm to cut ownership type)
#' @param override - local override rules for matching: dataframe 
process.prepLBK4match <- function(loanbook, selections, override) {
  # selections <- list(
  # ast = list(level = "Asset", ids = "Asset.ID", values = "Asset.Name", cut.ownership = T),
  # brr = list(level = "Borrower", ids = "Borrower.ID", values = "Borrower.Name"),
  # up = list(level = "Ultimate Parent", ids = "Ultimate.Parent.ID", values = "Ultimate.Parent.Name"))
  
  sel <- if (missing(selections)) {
    list(
      ast = list(level = "Asset", ids = "Asset.ID", values = "Asset.Name",  cut.ownership = T),
      brr = list(level = "Borrower", ids = "Borrower.ID", values = "Borrower.Name"),
      up = list(level = "Ultimate Parent", ids = "Ultimate.Parent.ID", values = "Ultimate.Parent.Name")
    )
  } else {
    selections
  }
  # process one element of list of selections
  mapfun <- function(x) {
    elems <- names(x)
    # checks
    if (!"level"  %in% elems) stop("\"level\" must be defined for all elements of selection")
    if (!"ids"    %in% elems) stop("\"ids\" must be defined for all elements of selection")
    if (!"values" %in% elems) stop("\"values\" must be defined for all elements of selection")
    # default values
    # x$value.type <- ifelse(!"value.type" %in% elems, "Legal Name", x$value.type)
    x$simplify <- ifelse(!"simplify" %in% elems, T, x$simplify)
    x$cut.ownership <- ifelse(!"cut.ownership" %in% elems, F, x$cut.ownership)
    src <- if("Source" %in% names(loanbook)) loanbook$Source else "Loanbook"
    # retrieve data for matching
    lbk.x <- addLBK4matching(
      source        = src,
      level         = x$level,
      ids           = loanbook[[x$ids]],
      values        = loanbook[[x$values]], 
      sectors       = loanbook[["Sector.Classification"]],
      # value.type    = x$value.type, 
      simplify      = x$simplify,
      cut.ownership = x$cut.ownership
    )
    # return result
    lbk.x
  }
  # go through list of selections and get data
  data.by.levels <- Map(f = mapfun, sel)
  # add override values
  overrides.by.levels <- if (missing(override)) {
    empty.df()
  } else {
    Map(
      f = function(x) {
        ovr.lev <- subset(override, Level = x)
        src <- if ("Source" %in% names(ovr.lev)) ovr.lev$Source else "Manual linkage"
        addLBK4matching(
          source     = src,
          level      = x,
          ids        = ovr.lev$ID,
          names      = ovr.lev$Name,
          # relation   = ovr.lev$Relation,
          values     = ovr.lev$Original.Value,
          sectors    = ovr.lev$Sector.Classification,
          # value.type = ovr.lev$Value.Type,
          simplify   = TRUE
        )
      },
      unique(override$Level)
    )
  }
  # combined & filtered & unique
  lbk <- 
    Reduce(
      f = rbind, 
      x = c(data.by.levels, overrides.by.levels),
      init = empty.LBK4mathcing()
    ) %>% 
    subset(
      !is_empty(Sector.Classification) & 
      !is_empty(Match.Value)
    ) %>%
    unique()
  # return values
  lbk
}

#------------------------------------------------------------------------------#
#' Extract necessary data from loanbook and format it
#' 
#' @description Function takes vectors of values and products dataframe that can 
#' be used for matching
#' 
#' @param level - mathicng level
#' @param ids - vector of IDs, for example customer Borrower IDs
#' @param names - vector of names, that characterize ID (for example Borrower Name)
#' @param values - vector of names, for example Borrower Name
#' @param sectors - vector of sector sclassification 
#' @param relation - describes how CustomerID related to OriginalValue in the output;
#'   see section "Relation types" for more details
#' @param value.type - describes what type of data OriginalValue contains; 
#'   see section "Value types" for more details
#' @param source - describes source of information; 
#'   it can be loanbook or manial client linkage file
#' @param simplify - defines if matching should be performed using simplified 
#'   values
#' @param cut.ownership - cuts-off ownership type
#' 
#' @section Relation types:
#'   
#' Relation type describes relation between Value for matching and client ID. 
#' For example if we use client's name, then relation will be `Id`;
#' if we use names of SPVs that belong to client, then we will use code
#' `Legal child`.
#'   
#' 
#' @section Value types:
#' 
#' In general value just characterizes company. It can be any unformation useful
#' for matching. Value type describes what kind of value we use. 
#' It can be Company name (`Name`) in a standard case,
#' but it can also be security ISIN code issued by company (`ISIN`) or
#' IMO of a ship owned by company (`IMO`).
addLBK4matching <- function(
  level, ids, names, values, sectors, 
  relation = "Id", value.type = "Legal Name", source = "Loanbook",
  simplify = FALSE, cut.ownership = FALSE
) {
  match.values <- if (simplify == FALSE) {
    values
  } else {
    simplifyName(values, cut.ownership)
  }
  id.names <- if(missing(names)) {
    values
  } else {
    names
  }
  df <- 
    data.frame(
      Source = source,
      Level = replicate(length(ids), level),
      ID = ids,
      Name = id.names,
      # Relation = relation,
      # Value.Type = value.type,
      Original.Value = values,
      Match.Value = match.values,
      Sector.Classification = sectors,
      stringsAsFactors = F
    )
  df
}

#' Empty loanbook data frame for mathcing
#' 
#' @description Function defines data frame structure for loanbook data used in 
#' matching.
#' 
#' @export
empty.LBK4mathcing <- function() {
  data.frame(
    Source                = character(),
    Level                 = character(),
    ID                    = character(),
    Name                  = character(),
    # Relation              = character(),
    # Value.Type            = character(),
    Original.Value        = character(),
    Match.Value           = character(),
    Sector.Classification = character(),
    stringsAsFactors = F
  )
  
}



#' Empty manual matches data frame
#' 
#' @description Function defines data frame structure for the manual matches data 
#' added to the automatically generated matches
#' 
#' @export
empty.ManualMatches4matching <- function() {
  data.frame(
    Loanbook.ID      = character(),
    Loanbook.Name    = character(),
    ALD.Name         = character(),
    Sector           = character(),
    stringsAsFactors = F
  )
  
}
