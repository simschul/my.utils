#' Title
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
read_EB3_A <- function(path) {
  data.table::fread(path, skip = 3, drop = c(1,2)) %>% 
    as.matrix  
}

#' Title
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
read_EB3_Y <- function(path) {
  data.table::fread(path, skip = 3, drop = c(1,2)) %>% 
    as.matrix  
}
#' Title
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
read_EB3_va <- function(path) {
  fread(file.path(path), 
        skip = 2, header = FALSE, drop = 1)[1:9] %>% 
    as.matrix
}

#' Title
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
read_EB3_S <- function(path) {
  fread(file.path(path), 
        skip = 25, header = FALSE, drop = 1) %>% 
    as.matrix
}