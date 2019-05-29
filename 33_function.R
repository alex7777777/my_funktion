#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: run all r-scripts

my_run_rscripts <- function(my_r_scripts) {
  
  if(!file.exists("r_objects")) {
    dir.create("r_objects")
    log_file <- data.frame(step = NA,
                           time_execute = as.POSIXct(NA),
                           to_dos = as.character(NA),
                           module = as.character(NA))
    log_file <- log_file[-1, ]
    print("The DF <log_file> was created and saved")
    save(log_file, file = "r_objects/log_file.RData")
  } else if(!file.exists("r_objects/log_file.RData")) {
    log_file <- data.frame(step = NA,
                           time_execute = as.POSIXct(NA),
                           to_dos = as.character(NA),
                           module = as.character(NA))
    log_file <- log_file[-1, ]
    print("The DF <log_file> was created and saved")
    save(log_file, file = "r_objects/log_file.RData")
  } else {
    load("r_objects/log_file.RData")
    print("The DF <log_file> was loaded")
  }
  
  for(i in 1:length(my_r_scripts)) {
    log_file2 <- data.frame(i, Sys.time(), "start", my_r_scripts[i])
    names(log_file2) <- c("step", "time_execute", "to_dos", "module")
    print(paste0("Executed script: ", my_r_scripts[i]))
    source(my_r_scripts[i])
    log_file3 <- data.frame(i, Sys.time(), "end", my_r_scripts[i])
    names(log_file3) <- c("step", "time_execute", "to_dos", "module")
    log_file <- rbind(log_file, log_file2, log_file3)
    save(log_file, file = "r_objects/log_file.RData")
  }
  
  return(log_file)
}
