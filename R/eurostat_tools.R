


#' Get definitions of all Eurostat codes for all variables (=columns) of the data set. 
#'
#' @param data A data.frame from get_eurostat(..., type = 'code')
#'
#' @return a list of dictionaries (=data.tables) for each variable of the supplied data (except time and value)
#' @export
#'
#' @examples

get_my_eurostat_dic <- function(data) {
  variables <- colnames(data)
  variables <- variables[which(!(variables %in% c('time', 'values')))]
  list <- create_named_list(variables)
  for (i in variables) {
    list[[i]] <- get_eurostat_dic(i) %>% 
      as.data.table %>%
      .[code_name %in% unique(data[[i]])]
    
  }
  return(list)
}


#' Checks which elements of a character vector are capital letters from A to U
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
is_NACE_level1 <- function(y) {
  y %in% LETTERS[1:21]
}

#' Checks which elements of a character vector have two digits as their last two characters
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
ends_with_2_numbers <- function(y) {
  !suppressWarnings(is.na(as.numeric(substr_right(y, 2))))
  
}

#' Checks which elements of a character vector have a capital letter from A to U as their first character
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
starts_with_NACE_letter <- function(y) {
  is_NACE_level1(substr(y, 1, 1))
}

#' Checks which elements of a character vector are of form [Capital Letter][Digit][Digit], such as
#' A03, C10, U99
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
is_NACE_level2 <- function(y) {
  return(starts_with_NACE_letter(y) & ends_with_2_numbers(y) & nchar(y) == 3)
}

#' Checks which elements of a character vector are combinations of NACErev2 level-2-sectors, such as: 
#' C10-12 or C10_C11
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
is_combination <- function(y) {
  return(is_combination_underscore(y) | is_combination_bar(y))
}


#' Title
#'
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
is_combination_underscore <- function(y) {
  z <- stringr::str_split(y, '_') 
  z <- lapply(z, function(x) {
    is_nace <- is_NACE_level2(x)
    sum(is_nace) == 2 | (isTRUE(is_nace[1]) & !is.nan(is.numeric(x[2])))
    }) 
  return(unlist(z) & grepl('_', y))
}

#' Title
#'
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
is_combination_bar <- function(y) {
  # 1. first 3 characters are nace rev 2 code
  # 2. contains "-"
  # 3. 5th and 6th characters are numbers
  is_NACE_level2(substr(y, 1,3)) & substr(y, 4,4) == '-'& ends_with_2_numbers(y)
}

#' Checks which elements of a character vector are valid NACE rev2 codes. 
#' Currently includes: 
#' - Level 1: A, B, ..., U
#' - Level 2: A01, C10, U99, ...
#' - Combinations: C10-12, C10_C11
#' 
#'  Not included: Level 3 and 4 codes, such as A01.01 or A01.01.11  
#'
#' @param y a vector
#'
#' @return vector of booleans
#' @export
#'
#' @examples
is_NACErev2_code <- function(y) {
  # currentyl only until level 2
  is_NACE_level1(y) | is_NACE_level2(y) | is_combination(y)
}



#' Converts a data.frame with NACE rev2 classification to a hierarchical data.tree. 
#' Currently only supports until 2nd level (e.g. A03, C11, ...)
#'
#' @param x a data.frame
#' @param code character: column name with NACE rev2 codes (need to be in the form A, B, ..., A01, C13, C10-12, C10_C11, ...)
#'
#' @return a data.tree object with all columns of x as attributes
#' @export
#'
#' @examples
convert_NACErev2_to_tree <- function(x, code) {
  # currently only works until 2nd level (eg. A03, C11, ...)
  level1_vec <- x[get(code) %in% LETTERS][[code]]
  for (i in level1_vec) {
    x[grepl(i, x[[code]]), code_tree := i]
  }
  x[nchar(get(code)) > 1, code_tree := paste0(code_tree, '.', get(code))]
  x[, code_tree := paste0('TOTAL.', code_tree)]
  tree <- as.Node(x, pathName = 'code_tree', pathDelimiter = '.')
  return(tree)
}



#' Title
#'
#' @param x 
#' @param code_name 
#' @param attribute 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
is_coherent_NACErev2 <- function(x, 
                                 code_name = 'code', 
                                 attribute = 'value', 
                                 tol = 1E-3) {
  tree <- convert_NACErev2_to_tree(x, code = code_name)
  return(is_coherent_tree(tree, attribute = attribute, tol = tol))
}


