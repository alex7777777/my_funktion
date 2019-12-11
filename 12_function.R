#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: automatic overwriting of time differences in the selected time classes

my_time_classes_recode <- function(ids, 
                                   time_difference_variable, 
                                   time_class_select_for_join) {
  
  
  my_df <- data.frame(ids = ids,
                      delta_t = time_difference_variable)
  
  library(dplyr)
  my_df <- left_join(my_df,
                     time_class_select_for_join,
                     all.x = TRUE)
  
  return(my_df[,ncol(my_df)])
}

