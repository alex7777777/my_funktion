#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Saving the best predictors

source("my_function/03_function.R")
my_best_predictors_boruta_save <- function(boruta_output, 
                                           file_name="boruta", 
                                           fold_name="csv_tab", 
                                           file_ext="csv") {
  predictor_select_boruta <- attStats(boruta_output)
  predictor_select_boruta$VarNr   <- c(1:nrow(predictor_select_boruta))
  predictor_select_boruta$VarName <- rownames(predictor_select_boruta)
  rownames(predictor_select_boruta) <- NULL
  predictor_select_boruta <- predictor_select_boruta[ , c(7,8,1:6)]
  predictor_select_boruta$VarName <- gsub("`", "", predictor_select_boruta$VarName)
  head(predictor_select_boruta)
  table(predictor_select_boruta$decision)
  
  my_save_tab_function(predictor_select_boruta, 
                       file_name, 
                       fold_name, 
                       file_ext)
}
