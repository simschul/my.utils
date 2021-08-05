

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



#' The machine epsilon is the difference between 1.0 and the next number that can be represented by the machine. By default, this function uses epsilon * 1000 as the tolerance. First it scales the values so that they have a mean of 1, and then it checks if the difference between them is larger than the tolerance. 
#' Just added na.rm argument to allow for NA in x.
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

coef_var <- function(x, na.rm = FALSE) {
  sqrt(var(x, na.rm = na.rm)) / mean(x, na.rm = na.rm)
}


#' Title
#' from:   https://rdrr.io/github/hadley/bigvis/man/weighted.var.html
#' @param x 
#' @param w 
#' @param na.rm 
#'
#' @return
#' @export
#'
#' @examples

weighted.var <- function (x, w = NULL, na.rm = FALSE){
  # from:   https://rdrr.io/github/hadley/bigvis/man/weighted.var.html
  if (na.rm) {
    na <- is.na(x) | is.na(w)
    x <- x[!na]
    w <- w[!na]
  }
  sum(w * (x - weighted.mean(x, w))^2)/(sum(w) - 1)
}

#' Title
#'
#' @param x a numeric vector or one-column matrix. Optionally: with attributes "scaled:center" and "scaled:scale" (as returned by the base function scale)
#' @param scaling_attributes optional. 
#'
#' @return a numeric vector. (also in case x is given as a 1-col matrix)
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

normalize <- function(x, na.rm = TRUE) {
  sumx <- sum(x, na.rm)
  x <- x / sumx
  attr(x, "normalized:sum") <- sumx
  return(x)
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples

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
#' @param fun a function to transform x. Or character vector: at the moment only 'logap' (log(x+a)) is implemented
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
                      scale = FALSE, normalize = FALSE, eps = 0.001, ...) {
  
  
  if (!missing(fun)) {
    if (is.character(fun) && fun == "logap") {
      a <- max(0,-min(x, na.rm = TRUE) + eps, na.rm = TRUE)
      fun <- eval(parse(text = whisker::whisker.render(template = 
                                                         ("function(x) logap(x, a = {{a}})"),
                                                       data = list("a" = a))))
      fun_inverse <- eval(parse(text = whisker::whisker.render(template = 
                                                                 ("function(x) exp(x) - {{a}}"),
                                                               data = list("a" = a))))
    }
    
    x <- fun(x)
    attr(x, "transformed:function") <- fun 
    if (missing(fun_inverse)) {
      attr(x, "transformed:inverse_function") <- NULL
    } else {
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



#' Title
#'
#' @param x 
#' @param inverse_fun 
#'
#' @return
#' @export
#'
#' @examples
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


#' see ?bestNormalize::log_x()
#'
#' @param x 
#' @param a 
#' @param eps 
#'
#' @return
#' @export
#'
#' @examples
logap <- function(x, a = max(0,-min(x, na.rm = TRUE) + eps, na.rm = TRUE), 
                  eps = 0.001) {
  xt <- log(x + a)
  attributes(xt) <- list("transformed:function" = logap, 
                         "logap:a" = a)
  return(xt)
}



#' Find the "best" a for a log(x+a) transformation when x contains values <= 0
#' From bestNormalize::log_x
#'
#' @param x 
#' @param eps 
#'
#' @return
#' @export
#'
#' @examples

find_a <- function(x, eps = 0.001) {
  return(max(0,-min(x, na.rm = TRUE) + eps), na.rm = TRUE)
}




#' Title
#'
#' @param x 
#' @param attribute 
#'
#' @return
#' @export
#'
#' @examples
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
#' @param model 
#'
#' @return
#' @export
#'
#' @examples
get_predictor <- function(model) {
  return(all.vars(formula(model$formula))[1])
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


#' Extracts the last n characters of a character string x
#'
#' @param x a character string
#' @param n the last n elements to extract
#'
#' @return a character string of length n
#' @export
#'
#' @examples
substr_right <- function(x, n){
  if (length(x) > n) warning(paste0('You want to extract the last ', n, 
                                    ' characters of a string of length ', nchar(x), 
                                    '. All characters of x a returned.'))
  nch <- nchar(x)
  substr(x, nch-n+1, nch)
}


#' Title
#'
#' @param restart 
#' @param gc 
#'
#' @return
#' @export
#'
#' @examples
#' 
clean_session <- function(detach_packages = TRUE, 
                          restart = FALSE, gc = TRUE) {
  clean_workspace(gc = gc)
  message('✓ Workspace cleaned ')
  if (isTRUE(detach_packages)) {
    detach_packages()
    message('✓ Packages detached ')  
  }
  if (isTRUE(restart)) {
    restartR()
    message('✓ R session restarted')
    }
}

#' Title
#'
#' @param gc 
#'
#' @return
#' @export
#'
#' @examples
clean_workspace <- function(gc = TRUE) {
  rm(list = ls(all = TRUE, envir = globalenv()),
     envir = globalenv())
  if (isTRUE(gc)) gc()
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
detach_packages <- function() {
  pkgs <- names(sessionInfo()$otherPkgs)
  if (!is.null(pkgs)) {
    invisible(lapply(paste0('package:', pkgs), 
                     detach, character.only=TRUE, unload=TRUE))
  }
  return(NULL)
} 


#' Title
#'
#' @return
#' @export
#'
#' @examples
restartR <- function() {
  .rs.restartR()
}


#' Returns the upper left corner of a large matrix. Extends the utils::head.matrix 
#' function by not only return the first n rows but also the first m cols. 
#'
#' @param x a matrix or data.frame
#' @param n number of rows to return
#' @param m number of cols to return
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples

big_head <- function (x, n = 6L, m = 10L, ...) {
  stopifnot(length(n) == 1L)
  n <- if (n < 0L) 
    max(nrow(x) + n, 0L)
  else min(n, nrow(x))
  m <- min(m, ncol(x))
  x[seq_len(n), seq_len(m) , drop = FALSE]
}




























