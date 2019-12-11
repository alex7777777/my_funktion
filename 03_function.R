#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: save table function

my_save_tab_function <- function(r_object_to_csv, name_for_saving, folder_name="csv_tab", file_extention="csv") {
  date_file_saving <- substr(as.character(Sys.time()), 1, 10)
  # check if the folder exists
  if (!file.exists(folder_name)){
    dir.create(file.path(getwd(), folder_name))
  }
  
  path_for_save <- paste0(folder_name, "/", date_file_saving, "_", name_for_saving, ".", file_extention)
  
  suppressWarnings(write.table(r_object_to_csv,
                               file = path_for_save,
                               append = TRUE,
                               sep = ";",
                               na = "",
                               dec = ",",
                               row.names = FALSE,
                               col.names=TRUE,
                               qmethod = "double"))
  
  func_message <- as.character(paste0("The file <", name_for_saving, ".csv> was saved successfully"))
  
  return(func_message)
}
