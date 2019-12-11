# ######################################################################## #
# 2019-06-01 - by Alex Gorbach                                             #
# Url-string processing and *.csv-file output for creating pivot tables    # 
# ######################################################################## #

library(stringr)

my_url_changeling <- function(url_string_vector) {
 
  get_all_my_function  <- function(funct_name) {
    url_my_function <- "https://raw.githubusercontent.com/alex7777777/my_funktion/master/"
    source(url(paste0(url_my_function, funct_name)))
    closeAllConnections()
  }
  
  options(warn=-1)
  # url_string_vector <- sapply(url_string_vector, function(x) str_replace_all(x, "\"", "|"))
  url_string_vector <- sapply(url_string_vector, function(x) URLdecode(x))
  # warnings()
  options(warn=0)
  
  # clean " "; "_nn"
  # Update 2019-08-27: "nn"  should remain in strings(!)
  get_all_my_function("49_function.R")
  # source("my_function/49_function.R")
  url_string_vector <- gsub(" ", "", url_string_vector, F) # as substring, not as matching
  # url_string_vector <- gsub("_nn", "", url_string_vector, F)
  url_string_vector <- gsub("%2C", "", url_string_vector, F)
  
  # Add new variables between delimiter from urs-strings to the DF
  get_all_my_function("48_function.R")
  # get_all_my_function("17_function.R")
  # source("my_function/48_function.R")
  # source("my_function/17_function.R")
  tree_vector <- cbind(data.frame(url_string_vector), my_split_save(url_string_vector, "_"))

  return(tree_vector[,2:ncol(tree_vector)])
}
