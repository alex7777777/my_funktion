#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Select of the best predictors

my_best_predictors_output <- function(boruta_output, SELECT_PREDICTORS=T) {
  
  # Collect Confirmed and Tentative variables
  boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])
  boruta_signif <- gsub("`", "", boruta_signif)
  print(boruta_signif)
  
  select_var_list_nr <- c()
  
  if(SELECT_PREDICTORS) {
    for(i in 1:length(boruta_signif)) {
      select_var_list_nr <- c(select_var_list_nr, grep(paste0("^", boruta_signif[i], "$"), colnames(df_train)))
    }
    print("Selecting variables: ")
    print(paste0(select_var_list_nr, ". ", boruta_signif, collapse=cat("\n")))
  } else {
    select_var_list_nr_tmp <- select_var_list_nr
    for(i in 1:length(boruta_signif)) {
      select_var_list_nr <- c(select_var_list_nr, grep(paste0("^", boruta_signif[i], "$"), colnames(df_train)))
    }
    print("Selecting variables: ")
    print(paste0(select_var_list_nr, ". ", boruta_signif, collapse=cat("\n")))
    select_var_list_nr <- select_var_list_nr_tmp
  }
  
  return(select_var_list_nr)
}
