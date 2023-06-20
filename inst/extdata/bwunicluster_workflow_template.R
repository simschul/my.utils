library(ssh)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
ssh_address <- "fr_ss1625@bwunicluster.scc.kit.edu"
file_to_execute <- "{{file_to_execute}}" 
files_etc <- "{{files_etc}}"
batch_job <- paste0({{project_name}}, '.sh')

############################################################################## # 
##### Workflow: Submit batch job to BWunicluster ###############################
############################################################################## # 

# # 0. Connect to server -----------------------------------------------------
(session <- ssh_connect(ssh_address, verbose = TRUE))

# 1. Update file on server -----------------------------------------------------
scp_upload(session, files = files_local, to = file.path({{project_name}}))


# 2. Check batch job file --------------------------------------------------
ssh_open_file(batch_job)
# save changes
scp_upload(session, files = file.path(batch_job))

# 3. Submit batch job ----------------------------------------------------------
ssh_exec_wait(session,
              command = paste("sbatch -p single", batch_job),
              std_out = 'current_batch_job_ID.txt')

batchID <- readLines('current_batch_job_ID.txt') %>%
  gsub("[^0-9.]", "",  .) %>%
  as.numeric()

# 4. List batch job queued ----------------------------------------------------------
ssh_exec_wait(session, command = 'squeue')

# 5. Cancel batch job ----------------------------------------------------------
ssh_exec_wait(session,
              command = paste("scancel", <<insert Batch ID here>>))

# 6. Check R output --------------------------------------------------------------
ssh_open_file(paste0(file_to_execute, '.Rout')) 
ssh_open_file(paste0('slurm-', batchID, '.out'))

# 7. Check progress ------------------------------------------------------------
check_progress(session)

# 8. Download results ----------------------------------------------------------
download_last_results(session)
scp_download(session=session,
             files = <<insert directory on server>>,
             to = './results')

