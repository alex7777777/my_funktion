#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Adding new numbered variables with NA
my_add_new_var <- function(my_df, numb_new_var, var_labels, fill_values=NA) {
  q_labels =  paste0(var_labels, 1:(numb_new_var))
  for(i in 1:numb_new_var) { my_df[,q_labels[i]] <- fill_values }
  return(my_df)
}
