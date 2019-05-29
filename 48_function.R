# ######################################################################## #
# 2019-05-21 - by Alex Gorbach                                             #
# Splitting HTTP-Strings with delimiters as "_" into separate parts and    # 
# saving them into separate variables                                      #
# ######################################################################## #

my_split_save <- function(col_to_split, delimiter="_") {
  
  library(stringr)
  library(rlang)
  library(tidyr)
  
  max_numb_pos <- max(str_count(col_to_split, delimiter))+1
  new_string_var <- data.frame(1:length(col_to_split))
  
  # create dummy variables
  source("my_function/17_function.R")
  new_string_var <- my_add_new_var(new_string_var,
                           max_numb_pos,
                           "pos_",
                           fill_values="")
  
  # splitting
  channel_split <- str_split(col_to_split, delimiter)
  
  # filling of the new variables:
  for(j in 1:length(col_to_split)) {
    for(i in 1:length(channel_split[[j]])) {
      new_string_var[j, i+1] <- channel_split[[j]][i]
    }
  }

  return(new_string_var[ , 2:ncol(new_string_var)])
}
