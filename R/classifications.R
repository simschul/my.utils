#' Checks if x is of type matrix and has rownames and column names
#'
#' @param x 
#'
#' @return boolean
#' @export
#'
#' @examples

is_concordance_matrix <- function(x) {
  return(is.matrix(x) & is.numeric(x) & !is.null(colnames(x)) & !is.null(rownames(x)))
}


#' Checks if a given concordance matrix or lookup table contains ONLY n-to-1 
#' concordances (i.e. only aggregation needed, no disaggregation)
#'
#' @param x either concordance matrix (check if is_concordance_matrix(x) delivers TRUE) or a look-up table with 2 columns
#' @param origin if x is a concordance-matrix: either 1 (if the rows indicate the original classification), or 2 (if it is the columns). If x is a look-up table: the argument indicates the column number (or name)
#'
#' @return boolean
#' @export
#'
#' @examples

is_Nto1_concordance <- function(x, origin = 1) {
  if (is_concordance_matrix(x)) {
    x <- transform_concordance_matrix_to_lookup_table(x)
  } 
  return(sum(duplicated(x[[origin]])) == 0)
}




#' Just adds rownames and column names to any matrix. 
#' rownames with leading i, colnames with leading j
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
create_concordance_matrix <- function(x) {
  if (!is.matrix(x)) stop("x needs to be of type 'matrix'!")
  rownames(x) <- paste0("i", formatC(1:nrow(x), width = floor(log10(nrow(x)))+1, 
                                     flag = "0")) # see https://stackoverflow.com/questions/47190693/count-the-number-of-integer-digits
  colnames(x) <- paste0("j", formatC(1:ncol(x), width = floor(log10(ncol(x)))+1, 
                                     flag = "0"))
  return(x)
}

#' Concordance matrix = matrix which has unique row- and colnames
#' Lookup table = 2-column data.frame/table with row-wise unique concordances 
#'
#' @param concordance_matrix 
#'
#' @return
#' @export
#'
#' @examples
transform_concordance_matrix_to_lookup_table <- function (concordance_matrix) {
  dt <- melt(concordance_matrix, 
             varnames = c("origin_id", "dest_id")) %>% 
    as.data.table %>% 
    .[value == 1] %>% 
    .[, value := NULL] %>% .[]  
  return(dt)
}

#' Currently only works for N-to-1 concordances
#'
#' @param origin_id vector of length N with the ids of the original classification
#' @param origin_value vector of length N with the values of the original classification 
#' @param origin_etc vector of length N OR data.frame with nrow = N with additional attributes (such as Year, Country, ...)
#' @param concordance either matrix with given col- and row-names (=concordance matrix) or data.frame. Expressing the way how the original classification shall be mapped to the 'new' classification. If it is a matrix the rows represent the original ids, the columns the new ids. If it is data.frame the first row represents the original ids, the second the new ones.
#'
#' @return a data.frame / data.table (currrently)
#' @export
#'
#' @examples

map_to_classification <- function(origin_id, origin_value, origin_etc, concordance) {
  # check if the additional attributes are a single vector or a data.frame
  if (is.vector(origin_etc)) {
    origin_etc <- data.table(Dim1 = origin_etc)
  }
  
  dt <- data.table(origin_id, origin_value, origin_etc)
  # check if input is of type concordance matrix
  if (is_concordance_matrix(concordance)) {
    concordance <- transform_concordance_matrix_to_lookup_table(concordance)
  } else if (is.data.frame(concordance)) {
    message(paste0("column '", colnames(concordance)[1], "' taken as origin_id and '", colnames(concordance)[2],"' as dest_id. If you wish the other way around change the order of the columns in the 'concordance' data.frame"))
    colnames(concordance) <- c("origin_id", "dest_id")
  }
  # check if it is a N to 1 concordance
  if (!is_Nto1_concordance(concordance)) {
    stop("Function currently only works with N-to-1 concordances.")
  } else {
    dest <- concordance %>%
      merge(dt, ., by = "origin_id") %>% 
      .[, sum(origin_value, na.rm = TRUE), by = c("dest_id", names(origin_etc))]
    setnames(dest, c("V1"), c("value"))
    return(dest)    
  }
}



.find_most_aggregated_classification <- function(concordance_matrix) {
  concordance_matrix[concordance_matrix == 0] <- NA
  results <- list()
  while (sum(concordance_matrix, na.rm = TRUE) > 0) {
    imax <- rowSums(concordance_matrix, na.rm = TRUE) %>%
      ifelse(. > 0, ., NA) %>% 
      which.max          
    concordance_matrix <- concordance_matrix[-imax,]
    jmax <- colSums(concordance_matrix, na.rm = TRUE) %>%
      ifelse(. > 0, ., NA) %>% 
      which.max
    concordance_matrix <- concordance_matrix[,-jmax]
    results[[length(results)+1]] <- c(names(imax), names(jmax))
  }
  return(unlist(results))
}





#' Takes two classifications where concordances are known and finds the most 
#' aggregated classifiaction (i.e. the classification where you do not have to
#' disaggregate, thus no further knowledge is needed).
#'
#' @param concordance_matrix 
#'
#' @return list with two look-up tables: i2k (concordance of rows to new classifaction), j2k (concordance of cols to new classification) 
#' @export
#'
#' @examples
get_most_aggregated_concordances <- function(concordance_matrix) {
  temp <- concordance_matrix %>% as.data.table(keep.rownames = "id_i") %>% 
    melt(id.var = "id_i", variable.name = "id_j", 
         variable.factor = FALSE) %>% 
    .[value > 0] %>% 
    .[, "sum_i" := sum(value), by = id_i] %>% 
    .[, "sum_j" := sum(value), by = id_j] %>% 
    .[, "id_k" := ifelse(sum_i > sum_j, 
                         id_i, id_j)] %>% 
    .[, c("value", "sum_i", "sum_j") := NULL] %>% 
    .[]
  ik_dt <- temp[, c("id_i", "id_k")] %>% unique 
  jk_dt <- temp[, c("id_j", "id_k")] %>% unique 
  return(list("i2k" = ik_dt, "j2k" = jk_dt))
}

