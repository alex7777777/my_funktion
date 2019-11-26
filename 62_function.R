#######################################################
# 2019-11-26 - by Alex Gorbach
#######################################################

# Function: running time for R code

my_r_code_running_time <- function(running_time_list, save_as_csv=T) {

  running_time_save <- data.frame("steps"=paste0("step ", 1:length(running_time_list)), 
                                  "running_time"=running_time_list)
  running_time_save$steps <- as.character(running_time_save$steps)
  running_time_save[nrow(running_time_save),1] <- "<end>"
  
  if(save_as_csv) {
    get_all_my_function  <- function(funct_name) {
      url_my_function <- "https://raw.githubusercontent.com/alex7777777/my_funktion/master/"
      source(url(paste0(url_my_function, funct_name)))
      closeAllConnections()
    }
    get_all_my_function("03_function.R")
    my_save_tab_function(running_time_save, "running_time_of_R_code")
    cat("The file <running_time_of_R_code.csv> was saved successfully")
  }

  knitr::kable(running_time_save)

}
