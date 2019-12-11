#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: GLM for select a time class
# Selecting only training my_rawdata

# Get all my functions
get_all_my_function  <- function(funct_name) {
  url_my_function <- "https://raw.githubusercontent.com/alex7777777/my_funktion/master/"
  source(url(paste0(url_my_function, funct_name)))
  closeAllConnections()
}

my_glm_for_time_class <- function(my_rawdata,
                                  SELECTED_TIME_CLASS,
                                  NUMBERTIMECLASSES=20,
                                  DEADLINE=as.Date("2017-01-01"),
                                  TARGET_VAR="rent_2017") {
  
  # length(unique(my_rawdata$ids)) #before: 20.273
  
  # selecting only training data
  load("r_objects/ORIG_ID.RData")
  training_ids <- unique(ORIG_ID[ORIG_ID$training_test == "training", ]$ids)
  my_rawdata <- my_rawdata[my_rawdata$ids %in% training_ids, ]
  rm(ORIG_ID, training_ids)
  
  # length(unique(my_rawdata$ids)) #after: 19.289

  # attach(my_rawdata)
  # if(exists("delta_t")) {
  #   my_rawdata$delta_t <- NULL
  # }
  
  # Time difference function
  get_all_my_function("05_function.R")
  my_rawdata$delta_t <- my_time_diff(my_rawdata$ids, my_rawdata$Datum)

  # Automatical generate Time Classes
  # Time distribution
  get_all_my_function("11_function.R")
  time_class_objects <- my_time_classes_generator(my_rawdata$delta_t, NUMBERTIMECLASSES)
  
  time_class_list <- time_class_objects$time_class_list
  
  # head(time_class_objects$time_class_list)
  # head(time_class_objects$TimeClassParam)
  
  # Recode time difference to time classes
  cat(paste0("Recode time difference to time classes.\n", 
             "Current select time class number: ", SELECTED_TIME_CLASS, sep = "\n"))
  
  get_all_my_function("12_function.R")
  my_rawdata$delta_t <- my_time_classes_recode(my_rawdata$ids, 
                                               my_rawdata$delta_t, 
                                               time_class_list[[SELECTED_TIME_CLASS]])
  # table(my_rawdata$delta_t)
  
  # # cj_df_07 <<< cj_df
  
  # get_all_my_function("18_function.R")
  # my_rawdata_dm <- my_help_var(my_rawdata[,1:3])
  # head(my_rawdata_dm)
  # table(my_rawdata_dm$rent_2017)
  
  my_rawdata_result <- my_rawdata[ my_rawdata$Datum >= as.POSIXct(DEADLINE), ]
  length(unique(my_rawdata_result$ids)) #3.102
  
  my_target_var <- data.frame(ids = unique(my_rawdata$ids),
                              rent_2017 = 1)
  my_target_var$rent_2017 <- ifelse(my_target_var$ids %in% my_rawdata_result$ids, 0, 1) # "0" - rent a car vs. "1" - don't rent a car
  table(my_target_var$rent_2017)
  
  # Pattern data frame for the time interval 01.01.2014-31.12.2016
  raw_data <- my_rawdata[my_rawdata$Datum < as.POSIXct(DEADLINE), ]
  
  # Sequencing raw data
  get_all_my_function("06_function.R")
  data_seq <- my_data_sequencing(raw_data, 1) # 1=LENGTHFROM
  
  get_all_my_function("13_function.R")
  sq_df <- my_pattern_df(data_seq)
  
  length(unique(sq_df$ids))
  
  library(plyr)
  my_rawdata_dm <- join(my_target_var, sq_df, type = "inner")
  
  # This step is not necessary if the distribution of the dependent
  # variable with the analyzed value is not less than 5%.
  #
  # The problem: "Imbalanced Data"
  # Logistic Regression for Rare Events"
  # https://statisticalhorizons.com/logistic-regression-for-rare-events
  # Using Random Forest to Learn Imbalanced Data
  # http://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf
  
  if(min(table(my_rawdata_dm[ , TARGET_VAR])) / sum(table(my_rawdata_dm[ , TARGET_VAR])) <= 0.05) {
    # Balanced sample 50% vs. 50%
    get_all_my_function("21_function.R")
    my_rawdata_dm <- my_balanced_sample(TARGET_VAR, my_rawdata_dm)
    print("The sample was balanced")
  } else {
    print("The sample was not balanced")
  }
  
  # Preparation of independent variable sets
  get_all_my_function("22_function.R")
  library(stringr)
  independ_var_list <- my_independ_var_set(my_rawdata_dm, TARGET_VAR, 3) # "3": SET3 for all predictors
  
  # REGRESSION DIAGNOSTICS
  # https://www.statmethods.net/stats/rdiagnostics.html
  
  # check the alias problem
  get_all_my_function("28_function.R")
  independ_var_list
  # my_check_alias(TARGET_VAR, my_rawdata_dm, independ_var_list, F)
  independ_var_list <- my_check_alias(TARGET_VAR, my_rawdata_dm, independ_var_list)
  
  # Multicollinearity problem
  get_all_my_function("23_function.R")
  print(paste0("Length of the independed variable list before cleaning: ", length(independ_var_list)))
  # my_multicoll_rm(TARGET_VAR, my_rawdata_dm, independ_var_list, 3.9, T)
  independ_var_list <- my_multicoll_rm(TARGET_VAR, my_rawdata_dm, independ_var_list)
  print(paste0("Length of the independed variable list after cleaning: ", length(independ_var_list)))
  
  # Logistic regression
  x <- my_rawdata_dm[ , independ_var_list]
  y <- my_rawdata_dm[ , TARGET_VAR]
  
  fit.glm <- glm(y ~ ., family=binomial, data=x)
  # Pseudo r-squared
  # library(pscl)
  # pseudo_r_squared <- data.frame(pR2(fit.glm))[6,]
  # paste0("Cragg and Uhler`s pseudo r-squared: ", round(100*pseudo_r_squared, 2), "%") # 52.8%
  
  p.glm <- ifelse(predict(fit.glm, x, type='response')> 0.5, 1,0)
  # table(real = y, predict = p.glm )
  
  # Acc <- sum(y == p.glm, na.rm = T)/(nrow(x) - sum(is.na(p.glm)))
  # paste0("Accuracy=", round(100*Acc, 1), "%") # 86.5%
  
  # Modeling quality assessment
  # table(p.glm)
  TP <- table(y, p.glm )[1,1]
  FP <- table(y, p.glm )[1,2]
  FN <- table(y, p.glm )[2,1]
  TN <- table(y, p.glm )[2,2]
  
  # F_measure <- round((2*TP/(TP+FP)*TP/(TP+FN))/(TP/(TP+FP)+TP/(TP+FN)),4)
  # F_measure
  
  return(round((2*TP/(TP+FP)*TP/(TP+FN))/(TP/(TP+FP)+TP/(TP+FN)),4))
}


# my_glm_for_time_class <- function(my_rawdata, 
#                                   SELECTED_TIME_CLASS,
#                                   NUMBERTIMECLASSES=20,
#                                   DEADLINE=as.Date("2017-01-01"),
#                                   TARGET_VAR="rent_2017",
#                                   my_rawdata_dm) {
#   attach(my_rawdata)
#   if(exists("delta_t")) {
#     my_rawdata$delta_t <- NULL
#   }
#   
#   # Time difference function
#   get_all_my_function("05_function.R")
#   my_rawdata$delta_t <- my_time_diff(my_rawdata$ids, my_rawdata$Datum)
#   
#   # Automatical generate Time Classes
#   # Time distribution
#   get_all_my_function("11_function.R")
#   time_class_objects <- my_time_classes_generator(my_rawdata$delta_t, NUMBERTIMECLASSES)
#   
#   time_class_list <- time_class_objects$time_class_list
#   
#   # Recode time difference to time classes
#   cat(paste0("Recode time difference to time classes.\n", 
#              "Current select time class number: ", SELECTED_TIME_CLASS, sep = "\n"))
#   
#   get_all_my_function("12_function.R")
#   my_rawdata$delta_t <- my_time_classes_recode(my_rawdata$ids, 
#                                                my_rawdata$delta_t, 
#                                                time_class_list[[SELECTED_TIME_CLASS]])
#   table(my_rawdata$delta_t)
#   
#   # cj_df_07 <<< cj_df
#   # my_rawdata_dm <- data.frame()
#   
#   get_all_my_function("18_function.R")
#   my_rawdata_dm <- my_help_var(my_rawdata[,1:3])
#   
#   # Pattern data frame for the time interval 01.01.2014-31.12.2016
#   raw_data <- my_rawdata[my_rawdata$Datum < as.POSIXct(DEADLINE), ]
#   
#   # Sequencing raw data
#   get_all_my_function("06_function.R")
#   data_seq <- my_data_sequencing(raw_data, 1) # 1=LENGTHFROM
#   
#   get_all_my_function("13_function.R")
#   sq_df <- my_pattern_df(data_seq)
#   
#   library(plyr)
#   my_rawdata_dm <- join(my_rawdata_dm, sq_df, type = "inner")
#   
#   # This step is not necessary if the distribution of the dependent
#   # variable with the analyzed value is not less than 5%.
#   #
#   # The problem: "Imbalanced Data"
#   # Logistic Regression for Rare Events"
#   # https://statisticalhorizons.com/logistic-regression-for-rare-events
#   # Using Random Forest to Learn Imbalanced Data
#   # http://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf
#   
#   if(min(table(my_rawdata_dm[ , TARGET_VAR])) / sum(table(my_rawdata_dm[ , TARGET_VAR])) <= 0.05) {
#     # Balanced sample 50% vs. 50%
#     get_all_my_function("21_function.R")
#     my_rawdata_dm <- my_balanced_sample(TARGET_VAR, my_rawdata_dm)
#     print("The sample was balanced")
#   } else {
#     print("The sample was not balanced")
#   }
#   
#   # Preparation of independent variable sets
#   get_all_my_function("22_function.R")
#   library(stringr)
#   indep_var_list <- my_independ_var_set(my_rawdata_dm, TARGET_VAR, 3) # "3": SET3 for all predictors
#   
#   # REGRESSION DIAGNOSTICS
#   # https://www.statmethods.net/stats/rdiagnostics.html
#   
#   # check the alias problem
#   get_all_my_function("28_function.R")
#   print(indep_var_list)
#   # my_check_alias(TARGET_VAR, my_rawdata_dm, indep_var_list, F)
#   indep_var_list <- my_check_alias(TARGET_VAR, my_rawdata_dm, indep_var_list)
#   print(indep_var_list)
#   
#   # Multicollinearity problem
#   get_all_my_function("23_function.R")
#   print(paste0("Length of the independed variable list before cleaning: ", length(indep_var_list)))
#   # my_multicoll_rm(TARGET_VAR, my_rawdata_dm, indep_var_list, 3.9, T)
#   indep_var_list <- my_multicoll_rm(TARGET_VAR, my_rawdata_dm, indep_var_list)
#   print(paste0("Length of the independed variable list after cleaning: ", length(indep_var_list)))
#   
#   # Logistic regression
#   # x <- my_rawdata_dm[ , indep_var_list]
#   # y <- my_rawdata_dm[ , TARGET_VAR]
#   
#   fit.glm <- glm(my_rawdata_dm[ , TARGET_VAR] ~ ., family=binomial, data=my_rawdata_dm[ , indep_var_list])
#   # Pseudo r-squared
#   library(pscl)
#   pseudo_r_squared <- data.frame(pR2(fit.glm))[6,]
#   paste0("Cragg and Uhler`s pseudo r-squared: ", round(100*pseudo_r_squared, 2), "%") # 52.8%
#   
#   return(pseudo_r_squared)
# }
