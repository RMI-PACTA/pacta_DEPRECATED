#' Check completeness of dataset attributes
#'
#' @description Procedure returns NULL if dataset complete or raises error if
#'   required columns are missing
#'
#' @export
#'
#' @param ds_name dataset name
#' @param ds_col_names vector of columns from dataset that needs to be checked for
#'   completeness
#'   
check.ds_cols <- function(ds_name, ds_col_names) {
  missing <- get.ds_missing_cols(ds_name, ds_col_names)
  if (length(missing) > 0) {
    collapsed <- paste(missing, collapse = ",")
    stop(
      paste("Dataset", ds_name, "has incomplete. Missing columns:", collapsed)
    )
  }
}

#' Find list of missing columns
#'
#' @description Function returns vector of missing columns (among given columns
#'   as an input parameter) in a given dataset. If all columns are present then
#'   function returns empty vector. Requirements are taken from packages internal dataset \link{data.dataset.req.attrs}
#'
#' @export
#'
#' @param ds_name Dataset Name
#' @param ds_col_names vector of Dataset columns
get.ds_missing_cols <- function(ds_name, ds_col_names) {
  requirements <- subset(
    pacta::dataset.req.attrs,
    Dataset == ds_name
  )
  req.attr <- requirements$Attribute
  # required attributes which are not in subject to check
  req.attr[!req.attr %in% ds_col_names]
}

#' Substitute parts of string
#' 
#' @param str string to modify
#' @param subs list of substitutions in format \code{list(name = value)}
#'   where \code{name} is pattern and \code{value} is replacement.
susbtitute.string <- function(str, subs) {
  new_val <- Reduce(
    f = function(z, x) {
      stringr::str_replace(z, stringr::fixed(paste0("{", x, "}")), subs[[x]])
    },
    x = names(substitutions),
    init
  )
  # recursive call
  if (string == new_val) {
    new_val
  } else {
    substitute.string(new_val, substitutions)
  }
  
}

#' List of files for each dataset from requirements
#'
#' @param ds_name dataset name
#' @param files vector of file names
#' @param substitutions list of substitutions (list(keyword = value)), so
#'   {keyword} entries can be replaced in vector of filenames
get.ds_file_names <- function(ds_name, files, substitutions) {
  NULL
  # use dirname(), basename()
}

#' Number of files for each dataset from requirements
#'
#' Function checks vector of given files (with full path) and returns number of
#' files that provide data for specified dataset.
#'
#' @param ds_name Dataset name
#' @param files list of files to check if they serve as a source for dataset.
#'   
get.ds_file_count <- function(ds_name, files) {
  NULL
}
