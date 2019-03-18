#' read input and return to caller (generic method to read from KV store)
#'
#' Function takes another function that performs read and location that tells
#' what exactly needs to be read. 
#' 
#' @param f function that reads value
#' @param k key that describes where value stored 
io.file.read <- function(f, k) f(k)

#' write output (generic method to write into KV store)
#'
#' Procedure takes data (value), function that performs write operation and location that tells
#' what exactly needs to be written 
#' 
#' @param v value to be written
#' @param f function that writes value f(v, k)
#' @param k key that describes where value stored 
io.file.write <- function(v, f, k) {
  l <- dirname(path = k)
  if (!dir.exists(paths = l)) {dir.create(path = l, recursive = T)}
  f(v, k)
}

#' Data import
#'
#' @description Function imports data from Key-Value store by given key. By
#'   default file system is used as KV store. Serialization format is CSV
#'   (american centric).
#'
#' @export
#'
#' @param file path to data; by default it is path to file (without extension)
#'   where data is stored
#' @param ... parameters of \code{read.csv} function
io.import.csv <- function(file, ...) {
  k <- file
  f <- function(k) {
    read.csv(
      file = k, stringsAsFactors = F,
      header = T, sep = ",", quote = "\"", dec = ".", na.strings = c(""), ...
    )
  }
  io.file.read(f, k)
}

#' Data export
#'
#' @description Function exports data into Key-Value store by given key. By
#'   default file system is used as KV store. Serialization format is CSV
#'   (american centric).
#'
#' @export
#'
#' @param data data to be exported
#' @param file path to value; by default it is path to file (without extension)
#'   where data is stored
io.export.csv <- function(data, file) {
  k <- file
  v <- data
  f <- function(v, k) {
    write.csv(
      x = v, file = k, append = F,
      header = T, sep = ",", quote = "\"", dec = ".", row.names = F
    )
  }
  io.file.write(v, f, k)
}

#' Load data object from dump
#'
#' @description Fast using internal serialization format.
#'   Works best for temporary / intermediate data.
#'
#' @export
#'
#' @param key location of value
io.load <- function(key) {
  k <- paste0(key, ".rds")
  f <- function(k) readRDS(k)
  io.file.read(f, k)
}

#' Save data object from dump
#'
#' @description Fast save using internal serialization format.
#'   Works best for temporary / intermediate data.
#'
#' @export
#'
#' @param value value to be stored
#' @param key location of value
io.save <- function(value, key) {
  k <- paste0(key, ".rds")
  f <- function(v, k) saveRDS(v, k)
  io.file.write(value, f, k)
}

#' Get folder name
#'
#' @description Supplementary function that calculates dynamic path to file
#'   based on preserved names. It supposed to work together with
#'   link{data.dataset.req.files} where Location is passed into \code{location}
#'   parameter.
#'
#' @export
#'
#' @param location input parameterized path, for example "Location" attribute 
#'   from \link{data.dataset.req.files} data frame.
#' @param config configuration from yml file, returned by \code{config::get} method
io.getPath <- function(location, config) {
  datestr <- as.Date(config$workflow$date) %>% strftime(format = "%Y-%m-%d")
  data.input <- ifelse(
    is.null(config$location$`data-input`), 
    "./DataInput", 
    config$location$`data-input`
  ) %>% paste0("/", datestr)
  data.temp <- ifelse(
    is.null(config$location$`data-input`), "./Data", 
    config$location$`data-temp`
  )
  data.result <- ifelse(
    is.null(config$location$`data-result`), 
    "./Results", 
    config$location$`data-result`
  ) %>% paste0("/", datestr)
  
  Reduce(
    f = function(z, x) {gsub(pattern = x[1], replacement = x[2], x = z, fixed = T)},
    x = list(
      c("{data-input}", data.input),
      c("{data-temp}", data.temp),
      c("{result}", data.result)
    ),
    init = location
  )
}

#' Count files in folder
#'
#' @description function counts files by given file-name pattern in specific
#'   folder.
#'
#' @export
#'
#' @param path folder where to count files
#' @param pattern file-name pattern
io.countFiles <- function(path, pattern) {
  mapply(
    FUN = function(x, y) {list.files(x, y) %>% length},
    path,
    pattern,
    SIMPLIFY = T
  )
    
}

#' Get list of missing attributes of files
#'
#' @description Function returns vector of missing attributes in a file which 
#'   are necessary for processing
#'
#' @export
#'
#' @param file CSV file to check, contains full path to file (not only file name)
#' @param ds_name dataset name from \link{data.dataset.req.attrs} to retrieve
#'   requirements
io.getMissingFileAttr <- function(file, ds_name) {
  cols <- io.import.csv(file, nrows = 0) %>% colNames 
  get.ds_missing_cols(ds_name, cols)
}
