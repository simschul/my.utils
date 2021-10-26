#' Checks if a data.tree is coherent, i.e. all child nodes sum correctly to their parent nodes
#'
#' @param tree a data.tree object 
#' @param attribute the attribute to test for coherency (needs to be numeric!)
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
is_coherent_tree <- function(tree, attribute, tol = 1E-3) {
  if (!is.numeric(tree$Get('values'))) {
    stop('The attribute must be of type numeric. ')
  } 
  
  # sum up child nodes
  tree$Do(function(node) {
    node$temp <- Aggregate(node, attribute = attribute, 
                           aggFun = function(x)  { 
                             x <- sum(x, na.rm = TRUE)
                             x <- ifelse(x == 0, NA, x)
                             return(x)
                           })}, 
    traversal = "post-order")
  # compare to original values
  dt <- data.table(
    original = tree$Get(attribute),
    aggregated = tree$Get('temp'), 
    name = tree$Get('name')
  )
  dt[, reldif := (aggregated - original) / original]
  mean_reldif <- dt[, mean(reldif, na.rm = TRUE)]
  max_reldif <- dt[, max(reldif, na.rm = TRUE)]
  
  # print warning message if the maximum difference found is larger than the given tolerance 
  if (max_reldif > tol) {
    message(paste0('Discrepency found for node ', 
                   dt[reldif == max_reldif]$name, 
                   ': \nSum of its child nodes (', 
                   tree$Climb(name = dt[reldif == max_reldif]$name)$children %>%
                     sapply(function(x) x$Get('name')) %>% 
                     `names<-`(NULL) %>% 
                     paste(collapse = ', '),
                   ') show a relative variability of ', 
                   max_reldif, 
                   ' compared to the original value given for the node'))
    return(FALSE)
  } else {
    return(TRUE)
  }
}




