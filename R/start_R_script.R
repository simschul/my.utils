#' Title
#'
#' @param dir 
#' @param filename 
#' @param author 
#' @param packages 
#'
#' @return
#' @export
#'
#' @examples


start_R_script <- function(filename = "Untitled.R",
                           dir = ".", 
                           author = "Simon Schulte", 
                           packages = TRUE) { 
  
  path2template <- system.file(file.path("extdata", "template.R"), package = "my.utils")
  if (substr_right(filename,2) != ".R") filename <- paste0(filename, ".R") 
  new.path <- file.path(dir, filename)
  # write to template
  template_out <- whisker::whisker.render(template = readLines(path2template),
                                          data = list("date" = Sys.time(), 
                                                      "author" = author))
  
  writeLines(template_out, new.path)
  file.edit(new.path)
  
  
}


