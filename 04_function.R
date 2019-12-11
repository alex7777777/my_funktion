#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: save image function

my_save_img_function <- function(r_img_to_png, name_for_saving, folder_name="images") {
  if(!dir.exists(folder_name)) { dir.create(folder_name) }
  date_file_saving <- substr(as.character(Sys.time()), 1, 10)
  dev.copy(png,paste0(folder_name, "/", date_file_saving, "_", name_for_saving, ".png"))
  dev.off()
  func_message <- as.character(paste0("The file <", name_for_saving, ".png> was saved successfully"))
  
  return(func_message)
}
