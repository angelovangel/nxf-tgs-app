
bin_on_path = function(bin) {
  exit_code = suppressWarnings(system2("command", args = c("-v", bin), stdout = FALSE))
  return(exit_code == 0)
}

# Helper to check if pipeline is finished, based on the tmux_sessions() df
pipeline_finished <- function(id, df) {
  status_val <- df[df$session_id == id, ]$status
  if (length(status_val) > 0 && !is.na(status_val) && str_detect(status_val, 'OK')) {
    TRUE
  } else {
    FALSE
  }
  #file.exists(file.path("output", session_id, "00-sample-status-summary.html"))
}


# instead of imvoking heavy nextflow log, just read .nextflow/history
# write_nxf_status <- function(file) {
#   
#   log <- system2('nextflow', args = 'log', stdout = T, stderr = T)
#   if (length(log) < 2) {
#     write.csv(data.frame(COMMAND = NA, DURATION = NA, STATUS = NA), file = file) 
#   } else {
#     logt <- read.table(text = log, header = T, sep = "\t")
#     write.csv(logt, file = file)  
#   }
# }

# return nextflow log table for a nextflow log in a specific folder
# DO NOT invoke nextflow log, just read .nextflow/history
nxf_log <- function(path) {
  
  cols <- c("TIMESTAMP", "DURATION", "RUN.NAME", "STATUS", "REVISION.ID", "SESSION.ID", "COMMAND")
  #log <- processx::run('nextflow', 'log', wd = path, error_on_status = F)
  log <- processx::run('cat', args = fs::path(path, '.nextflow', 'history'), error_on_status = F)
  if (log$status == 0) {
    #read.table(text = log$stdout, header = T, sep = '\t')
    t <- read.table(text = log$stdout, header = F, sep = '\t')
    colnames(t) <- cols
    t
  } else {
    data_list <- setNames(rep(list(NA), length(cols)), cols)
    as.data.frame(data_list, stringsAsFactors = FALSE)
  }
  
}

