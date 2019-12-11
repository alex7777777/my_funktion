#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Preparation of independent variable sets for the modeling

my_independ_var_set <- function(my_df, dep_var, set_number) {
  predict_df <- data.frame(VarNr = seq(ncol(my_df)),
                           VarName = names(my_df),
                           SET1 = -1,
                           SET2 = -1,
                           SET3 = -1)
  
  # Independent variables
  # SET-1: one-event substrings
  
  # library(stringr)
  predict_df[(str_count(predict_df$VarName, " ")==0) &
               as.integer(rownames(predict_df)) != 1, ]$SET1 <- 1
  # SET-2: two- and three-events substrings
  predict_df[str_count(predict_df$VarName, " ") >0, ]$SET2 <- 1
  
  # SET-3: all predictors
  predict_df[(predict_df$SET1==1)|(predict_df$SET2==1), ]$SET3 <- 1
  
  # Dependent variables:
  predict_df[predict_df$VarName == dep_var, ]$SET1 <- 0
  predict_df[predict_df$VarName == dep_var, ]$SET2 <- 0
  predict_df[predict_df$VarName == dep_var, ]$SET3 <- 0
  
  # Predictor SETs als list
  predictor_select <- list()
  predictor_select$SET1 <- predict_df[predict_df$SET1==1, ]$VarNr
  predictor_select$SET2 <- predict_df[predict_df$SET2==1, ]$VarNr
  predictor_select$SET3 <- predict_df[predict_df$SET3==1, ]$VarNr
  
  
  predict_df[ predict_df$VarNr %in% predictor_select$SET3, ]
  
  return(predictor_select[[set_number]])
}
