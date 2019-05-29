#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Regression diagnostic & GLM

my_glm <- function(my_df,
                   TARGET_VAR_LIST,
                   INDEPEND_VAR_LIST,
                   SAMPLING=1000) {
  
  # REGRESSION DIAGNOSTICS
  # https://www.statmethods.net/stats/rdiagnostics.html
  
  # check the alias problem
  source("my_function/28_function.R")
  INDEPEND_VAR_LIST
  # my_check_alias(TARGET_VAR_LIST, my_df, INDEPEND_VAR_LIST, F)
  INDEPEND_VAR_LIST <- my_check_alias(TARGET_VAR_LIST, my_df, INDEPEND_VAR_LIST)
  INDEPEND_VAR_LIST
  # rm(my_check_alias)
  
  # Multicollinearity problem
  source("my_function/23_function.R")
  print(paste0("Length of the independed variable list before cleaning: ", length(INDEPEND_VAR_LIST)))
  # my_multicoll_rm(TARGET_VAR_LIST, my_df, INDEPEND_VAR_LIST, 3.9, T)
  INDEPEND_VAR_LIST <- my_multicoll_rm(TARGET_VAR_LIST, my_df, INDEPEND_VAR_LIST)
  print(paste0("Length of the independed variable list after cleaning: ", length(INDEPEND_VAR_LIST)))
  # rm(my_multicoll_rm)
  
  # Predictors distribution
  source("my_function/02_function.R")
  source("my_function/03_function.R")
  source("my_function/26_function.R")
  
  #INDEPEND_VAR_LIST
  for(i in 1:length(INDEPEND_VAR_LIST) ) {
    my_dist <- my_distrib(my_df[,INDEPEND_VAR_LIST[i]])
    text_f_output <- paste0("Var. Nummer ", INDEPEND_VAR_LIST[i], ". Distribution for <", names(my_df)[INDEPEND_VAR_LIST[i]], ">")
    my_save_txt_function(text_f_output, "predictor_distribution", "tmp", "csv")
    my_save_tab_function(my_dist, "predictor_distribution", "tmp", "csv")
  }
  # rm(my_distrib, my_save_tab_function, my_save_txt_function)
  
  # Logistic regression
  # x <- my_df[ , INDEPEND_VAR_LIST]
  # y <- my_df[ , TARGET_VAR_LIST]
  
  glm_output <- glm(my_df[ , TARGET_VAR_LIST] ~ ., family=binomial, data=my_df[ , INDEPEND_VAR_LIST])
  # summary(glm_output)
  # glm(formula = my_df[ , TARGET_VAR_LIST] ~ ., family=binomial, data=my_df[ , INDEPEND_VAR_LIST])
  
  # p.glm <- ifelse(predict(glm_output, my_df[ , INDEPEND_VAR_LIST], type='response')> 0.5, 1,0)
  # table(real = my_df[ , TARGET_VAR_LIST], predict = p.glm )
  # 
  # Acc <- sum(my_df[ , TARGET_VAR_LIST] == p.glm, na.rm = T)/(nrow(my_df[ , INDEPEND_VAR_LIST]) - sum(is.na(p.glm)))
  # paste0("Accuracy=", round(100*Acc, 1), "%") # 89.2%
  # 
  # # Pseudo r-squared
  # library(pscl)
  # pR2_r2CU <- data.frame(pR2(glm_output))[6,]
  # paste0("Cragg and Uhler`s pseudo r-squared: ", round(100*pR2_r2CU, 2), "%") # 51.51%
  
  return(glm_output)
}
