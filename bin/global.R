
bin_on_path = function(bin) {
  exit_code = suppressWarnings(system2("command", args = c("-v", bin), stdout = FALSE))
  return(exit_code == 0)
}

# Helper to check if pipeline is finished, based on the tmux_sessions() df
pipeline_finished <- function(id, df) {
  if (df[df$session_id == id, ]$status == 'OK') {
    TRUE
  } else {
    FALSE
  }
  #file.exists(file.path("output", session_id, "00-sample-status-summary.html"))
}



write_nxf_status <- function(file) {
  
  log <- system2('nextflow', args = 'log', stdout = T, stderr = T)
  if (length(log) < 2) {
    write.csv(data.frame(COMMAND = NA, DURATION = NA, STATUS = NA), file = file) 
  } else {
    logt <- read.table(text = log, header = T, sep = "\t")
    write.csv(logt, file = file)  
  }
}
