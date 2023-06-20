

############################################################################## # 
##### functions #################################################################
############################################################################## # 

#' Title
#'
#' @param file 
#' @param dir 
#'
#' @return
#' @export
#'
#' @examples
ssh_open_file <- function(file, dir = '.') {
  scp_download(session, file, to = dir, verbose = FALSE)
  rstudioapi::navigateToFile(file.path(dir, file))
}

#' Title
#'
#' @param session 
#' @param pkgs 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
ssh_install_packages <- function(session, pkgs, ...) {
  ssh_exec_wait(session, 
                command = c("module load math/R/3.6.3", 
                            paste0("R -e 'install.packages(\"",pkgs ,
                                   "\" , repos=\"http://cran.us.r-project.org\")'"))) 
}

#' Title
#'
#' @param session 
#' @param project_name 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
ssh_new_project <- function(session, project_name, ...) {
  ssh_exec_wait(session, 
                command = paste0('mkdir ', project_name))
}

#ssh_new_project(session, 'rio_indonesia')



#' Title
#'
#' @param session 
#' @param file_to_execute 
#' @param files_etc 
#' @param project_name 
#' @param n_nodes 
#' @param n_cores 
#' @param time_minutes 
#' @param RAM_per_node 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
use_bwunicluster <- function(ssh_address = "fr_ss1625@bwunicluster.scc.kit.edu", 
                             file_to_execute, 
                             files_etc = NULL, 
                             project_name,
                             n_nodes = NA, 
                             n_cores = NA, 
                             time_minutes = NA, 
                             RAM_per_node = NA, 
                             ...) {
  # connect to bwunicluster
  session <- ssh_connect(ssh_address, verbose = TRUE)
  
  # create directory on server
  ssh_new_project(session, project_name = project_name)
  
  # create bash script
  path2template_bash <- system.file(file.path("extdata", 
                                         "bwunicluster_bash_template.sh"), 
                               package = "my.utils")
  # write to template
  template_bash_out <- whisker::whisker.render(template = readLines(path2template_bash),
                                          data = list(
                                            jobname = project_name, 
                                            n_nodes = n_nodes, 
                                            n_cores = n_cores,
                                            time_minutes = time_minutes,
                                            RAM_per_node = RAM_per_node, 
                                            file_to_execute = file_to_execute
                                          ))
  # save to temp dir
  temp_dir <- file.path(tempdir(), paste0(project_name, '.sh'))
  writeLines(template_bash_out, temp_dir)
  
  # upload all files to server
  scp_upload(session, files = c(file_to_execute, 
                                files_etc, 
                                temp_dir), 
             to = file.path(project_name))
  
  # create workflow script
  path2template_workflow <- system.file(file.path("extdata", 
                                              "bwunicluster_workflow_template.R"), 
                                    package = "my.utils")
  
  template_workflow_out <- whisker::whisker.render(template = readLines(path2template_workflow),
                                          data = list(
                                            project_name = project_name, 
                                            file_to_execute = file_to_execute, 
                                            files_etc = files_etc 
                                          ))
  # save to temp dir
  writeLines(template_workflow_out, file.path('bwunicluster_workflow.R'))
  
  
  
  
}

# use_bwunicluster(session, file_to_execute = './R/eurostat_tools.R',
#                  files_etc = c('./R/general_tools.R','./R/plot_tools.R'),
#                  project_name = 'test3')


# check_progress <- function(session, dir = 'trade_split/results', 
#                            regex = "product_fp"){
#   tmp <- tempfile(fileext = '.txt')
#   ssh_exec_wait(session, command = c(paste0('cd ', dir), 'ls'), 
#                 std_out = tmp) 
#   read.csv(tmp, header = FALSE) %>% 
#     .[nrow(.), 1] %>% 
#     ssh_exec_wait(session=session, command = c(paste0('cd ', dir, '/', .), 'ls'), 
#                   std_out = tmp) 
#   
#   return(read.csv(tmp, header = FALSE) %>% 
#            unlist %>% 
#            grepl(regex, .) %>% 
#            sum)
# }
# 
# download_last_results <- function(session, dir = 'trade_split/results', to = './results') {
#   tmp <- tempfile(fileext = '.txt')
#   ssh_exec_wait(session, command = c(paste0('cd ', dir), 'ls'), 
#                 std_out = tmp) 
#   
#   read.csv(tmp, header = FALSE) %>% 
#     .[nrow(.), 1] %>%
#     as.character() %>% 
#     #paste0(dir, "/", .)
#     scp_download(session=session, files = paste0(dir, "/", .))
# }
# 
# remove_last_results <- function(session, dir) {
#   tmp <- tempfile(fileext = '.txt')
#   ssh_exec_wait(session, command = c(paste0('cd ', dir), 'ls'), 
#                 std_out = tmp) 
#   read.csv(tmp, header = FALSE) %>% 
#     .[nrow(.), 1] %>%
#     as.character() %>% 
#     ssh_exec_internal(session=session, command = c(paste0('rm -Rf ', dir, '/', .)))
# }

