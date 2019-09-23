

#' Title
#'
#' @param names a vector (character or numeric). Numeric is converted to character.  
#'
#' @return a list of the same length as the names vector
#' @export
#'
#' @examples
#' 
create_named_list <- function(names) {
  vector(mode = "list", length(names)) %>% 
    setNames(names)
  
}



#' If you want to search for two (or more) words within a string in any
#' possible order
#'
#' @param words a character vector containing the words you want to look for
#'
#' @return an reg exp string
#' @export
#'
#' @examples
combine_words_regexp <- function(words) {
  regexp <- ""
  for(i in 1:length(words)) {
    regexp <- paste0(regexp, "(?=.*", words[i], ")")
  }
  return(regexp)
}



#' Title
#'
#' @param x 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5, na.rm = TRUE) {
  if (length(x) == 1) return(TRUE)
  abs(max(x) - min(x)) < tol
}

