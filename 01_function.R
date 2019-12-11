#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: creation of help directories

my_create_help_dir  <- function(create_dir_names) {
  for(i in 1:(length(create_dir_names))) {
    if (!file.exists(create_dir_names[i])){
      dir.create(file.path(getwd(), create_dir_names[i]))
    } else {
      print(paste0("The directory <", create_dir_names[i], "> already exists"))
    }
  }
}
