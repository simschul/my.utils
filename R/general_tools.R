

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
#' @return a reg exp string
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

rescale <- function(x, scaling_attributes) {
  if (missing(scaling_attributes)) scaling_attributes <- attributes(x)
  center <- "scaled:center" %in% names(scaling_attributes)
  scale <- "scaled:scale" %in% names(scaling_attributes)
  if (isFALSE(center) & isFALSE(scale)) {
    warning("Argument x should have attribute `scaled:center` and/or `scaled:scale`")
    return(x)
  } 
  if (isTRUE(center) & isTRUE(scale)) {
    x_unscaled <- x * scaling_attributes$`scaled:scale` + scaling_attributes$`scaled:center` 
  } else if (isFALSE(center)) {
    x_unscaled <- x * scaling_attributes$`scaled:scale`
  } else {
    x_unscaled <- x + scaling_attributes$`scaled:center`    
  }
  attr(x_unscaled, "scaled:scale") <- NULL
  attr(x_unscaled, "scaled:center") <- NULL
  attr(x_unscaled, "dim") <- NULL
  return(x_unscaled)
}






#' Normalize vector so that the data has a range from 0 to 1
#'
#' @param x a numeric vector
#'
#' @return the normalized vector and the sum of the original vector as attribute (to back-transform later)
#' @export
#'
#' @examples

normalize <- function(x) {
  sumx <- sum(x)
  x <- x / sumx
  attr(x, "normalized:sum") <- sumx
  return(x)
}

renormalize <- function(x) {
  x <- x * attributes(x)[["normalized:sum"]]
  attr(x, "normalized:sum") <- NULL
  return(x)
}


#' Transform a data. Here 'transform' is used in a wider meaning including standardization and normalization
#' This function can be used to 
#' (i) transform `x` using the function given by `fun`
#' (ii) scale `x` using R's `scale` function (=standardization)
#' (iii) normalize `x` so that its values have a range from 0 to 1
#' 
#' When combining (i) and (ii) or (i) and (iii) the data is first transformed then scaled/normalized
#' Note: Combining (ii) and (iii) does not make any sense.
#' @param x a numeric vector
#' @param fun a function to transform x
#' @param fun_inverse the inverse of it
#' @param scale boolean. shall x also be scaled? default = FALSE
#' @param normalize boolean. shall x also be normalized? default = FALSE 
#' @param ... other parameters passed `scale` (at the moment only support `center`)
#'
#' @return
#' @export
#'
#' @examples
transform <- function(x, fun, fun_inverse,
                      scale = FALSE, normalize = FALSE, ...) {
  if (!missing(fun)) {
    x <- fun(x)
    attr(x, "transformed:function") <- fun 
    if (missing(fun_inverse)) attr(x, "transformed:inverse_function") <- NULL
    else {
      if (is_inverse_function(fun, fun_inverse)) {
        attr(x, "transformed:inverse_function") <- fun_inverse
      } else stop("fun and fun_inverse are not inverse to each other!")
    }
  }
  if (isTRUE(scale) & isTRUE(normalize)) stop("scaling and normalization together does not make any sense. Please set either scale=FALSE or normalize=FALSE")
  if (isTRUE(scale)) {
    atts <- attributes(x)
    x <- scale(x, ...)
    attr(x, "dim") <- NULL
    attributes(x) <- c(atts, attributes(x))
  }
  if (isTRUE(normalize)) x <- normalize(x)  
  return(x)
}


retransform <- function(x, inverse_fun) {
  # 1. rescale data
  if (has_attribute(x, "scaled:center") | has_attribute(x, "scaled:scale")) {
    x <- rescale(x)
  }
  # 2. re-normalize data
  if (has_attribute(x, "normalized:sum")) {
    x <- renormalize(x)
  }
  # 3. retransform
  if (has_attribute(x, "transformed:function")) {
    if (!missing(inverse_fun)) {
      if (!is_inverse_function(attr(x, "transformed:function"), inverse_fun)) {
        stop("fun and fun_inverse are not inverse to each other!")
      }
      x <- inverse_fun(x)
    } else if (has_attribute(x, "transformed:inverse_function")) {
      x <- attr(x, "transformed:inverse_function")(x)
    } else {
      stop("No inverse function found! Please provide either `inverse_fun` or give x an attribute named `transformed:inverse_function`")
    }
  }
  attr(x, "transformed:function") <- NULL
  attr(x, "transformed:inverse_function") <- NULL
  return(x)
}


has_attribute <- function(x, attribute) {
  return(attribute %in% names(attributes(x)))
}


#' Tests if two function are inverse to each other, thus if f(g(x)) == x applies
#'
#' @param fun1 
#' @param fun2 
#'
#' @return boolean
#' @export
#'
#' @examples

is_inverse_function <- function(fun1, fun2) {
  x <- runif(1, min = 0.5)
  return(dplyr::near(fun1(fun2(x)),x))
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
common_elements <- function(x, y, mode = 1, ignore.case = FALSE) {
  x <- unique(x)
  y <- unique(y)
  if (isTRUE(ignore.case)) {
    x <- tolower(x)
    y <- tolower(y)
  }
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
non_common_elements <- function(x, y, mode = 1, ignore.case = FALSE) {
  x <- unique(x)
  y <- unique(y)
  if (isTRUE(ignore.case)) {
    x <- tolower(x)
    y <- tolower(y)
  }
  if (mode == -1) res <- y[!(y %in% x)]
  else res <- x[!(x %in% y)]
  return(res)
}


#' Title
#'
#' @param x 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
substr_right <- function(x, n){
  nch <- nchar(x)
  substr(x, nch-n+1, nch)
}
