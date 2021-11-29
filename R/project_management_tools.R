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




#' Run this function when starting a new projects. 
#' 1. Creates a new directory with all relevant folders (code, tex, data, figures, ...)
#' 2. Optionally creates an Rproject within the Code folder
#' 3. Optinally with Git/Github
#'
#' @param name the name of the project/the folder supposed to be created
#' @param root_dir the root directory where the project should be stored
#' @param rproject defaults to `name`, can be given any character string. Set to NULL if you wish not to have a rproject be created
#' @param git TRUE if you want to create a local git repo. You will be asked to make an initial commit
#' @param github TRUE if you wish to create an remote repo on github
#'
#' @return
#' @export
#'
#' @examples

start_new_project <- function(name,
                              root_dir = '/home/simon/Documents/Projects',
                              rproject = name, # character or NULL
                              git = TRUE, 
                              github = TRUE
) {
  project_dir <- file.path(root_dir, name)
  
  if (dir.exists(project_dir)) {
    stop('Project directory already exists.')
  }
  
  code_dir <- file.path(project_dir, 'code')
  dir.create(code_dir, recursive = TRUE)
  dir.create(file.path(project_dir, 'tex'))
  dir.create(file.path(project_dir, 'data'))
  dir.create(file.path(project_dir, 'figures'))
  dir.create(file.path(project_dir, 'documentation'))
  dir.create(file.path(project_dir, 'misc'))
  dir.create(file.path(project_dir, 'tmp'))
  dir.create(file.path(project_dir, 'spreadsheets'))
  
  if (!is.null(rproject) & !isFALSE(rproject)) {
    create_project(file.path(code_dir, rproject), open = FALSE)
    if (git) {
      proj_set(file.path(code_dir, rproject))
      use_git()
      if (github) {
        use_github(private = TRUE)
      }
    }
  }
  
}








