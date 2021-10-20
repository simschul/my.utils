


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
