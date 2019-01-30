

#' Title
#'
#' @param raster a raster object
#' @param na.rm boolean, should cells with NA values be removed? Default is TRUE
#' @param xy boolean, should x- and y- coordinates be listed (as separated columns) in the data.table? Default is TRUE
#'
#' @return a data.table
#' @export
#'
#' @examples
#' 
convert_raster2dt <- function(raster, na.rm = TRUE, xy = TRUE) {
  raster::as.data.frame(raster, xy = xy, na.rm = na.rm) %>%
    as.data.table
}