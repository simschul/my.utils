#' Set the row order to a specific given new order
#' copied from: https://stackoverflow.com/questions/37878620/reorder-rows-in-data-table-in-a-specific-order
#' @param x 
#' @param neworder 
#'
#' @return
#' @export
#'
#' @examples

setroworder <- function(x, neworder) {
  setorderv(x[, eval(call(":=", as.name(".r"), call("order", neworder)))], ".r")[, ".r" := NULL]
} 