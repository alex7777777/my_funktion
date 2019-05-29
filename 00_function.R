#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: read raw data

my_read_rawdata_csv <- function(folder_for_raw_data, 
                                SAVE_RAW_DATA=F, 
                                folder_for_save_raw_data_r="r_objects") {
  pfad_for_raw_data <- paste0("./", folder_for_raw_data)
  to_read_files <- dir(pfad_for_raw_data)
  names_for_save_read_files <- gsub("\\..*","",to_read_files)
  rawdata <- list()
  for(i in 1:(length(to_read_files))) rawdata[[i]] <- data.frame()
  names(rawdata) <- names_for_save_read_files
  for(i in 1:(length(to_read_files))) {
    print(paste0("Reading: <", to_read_files[i], ">..."))
    rawdata[[i]] <- read.csv(paste0(folder_for_raw_data, "/", to_read_files[i]),
                              header = T, sep = ";", dec= ",", fileEncoding="UTF-8")
    print(paste0("The file <", to_read_files[i], "> has been read"))
  }
  if(SAVE_RAW_DATA) {
    save(rawdata, file = paste0(folder_for_save_raw_data_r, "/rawdata.RData"))
  }
  return(rawdata)
}
