#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: save text function

my_save_txt_function <- function(txt_for_saving, name_for_saving, folder_name="tmp", file_extention="txt") {
  date_file_saving <- substr(as.character(Sys.time()), 1, 10)
  # check if the folder exists
  if (!file.exists(folder_name)){
    dir.create(file.path(getwd(), folder_name))
  }
  my_file = paste0(folder_name, "/", date_file_saving, "_", name_for_saving, ".", file_extention)
  write(txt_for_saving, file = my_file, append = TRUE, sep = ";")
  
  func_message <- as.character(paste0("The file <", name_for_saving, ".csv> was saved successfully"))
  
  return(func_message)
}
