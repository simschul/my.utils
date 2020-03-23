

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
#' see https://www.rdocumentation.org/packages/scales/versions/0.4.1/topics/zero_range
#' @param x 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5, na.rm = TRUE) {
  if (length(x) == 1) return(TRUE)
  if (sum(is.na(x)) == length(x)) return(NA)
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) < tol
}


#' Calculates the coefficient of variance 
#' 
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
coef_var <- function(x) {
  sqrt(var(x)) / mean(x)
}




#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples

rescale <- function(x) {
  atts <- attributes(x)
  center <- "scaled:center" %in% names(atts)
  scale <- "scaled:scale" %in% names(atts)
  if (isFALSE(center) & isFALSE(scale)) {
    stop("Argument x must have attribute `scaled:center` and/or `scaled:scale`")
  } 
  if (isTRUE(center) & isTRUE(scale)) {
    x_unscaled <- x * atts$`scaled:scale` + atts$`scaled:center` 
  } else if (isFALSE(center)) {
    x_unscaled <- x * atts$`scaled:scale`
  } else {
    x_unscaled <- x + atts$`scaled:center`    
  }
  attr(x_unscaled, "scaled:scale") <- NULL
  attr(x_unscaled, "scaled:center") <- NULL
  return(x_unscaled)
}


#' Title
#'
#' @param x vector
#' @param y vector
#' @param mode either 1: looking for common elements (default), or -1: looking for non-common elements
#'
#' @return
#' @export
#'
#' @examples
common_elements <- function(x, y, mode = 1) {
  x <- unique(x)
  y <- unique(y)
  if (mode == -1) res <- y[y %in% x]
  else res <- x[x %in% y]
  return(res)
}

#' Title
#'
#' @param x 
#' @param y 
#' @param mode 
#'
#' @return
#' @export
#'
#' @examples
#' 
non_common_elements <- function(x, y, mode = 1) {
  x <- unique(x)
  y <- unique(y)
  if (mode == -1) res <- y[!(y %in% x)]
  else res <- x[!(x %in% y)]
  return(res)
}

