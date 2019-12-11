# ######################################################################## #
# 2019-03-10 - by Alex Gorbach                                             #
# It makes a lot of binary variables from the nominal or factor variables  #
# including empty and missing values                                      #
# ######################################################################## #

my_nominal_to_binary <- function(nominal_feature_as_list, 
                                 prefix_for_new_variables = "") {
  
  if(!(class(nominal_feature_as_list) == "character" || class(nominal_feature_as_list) == "factor")) {
    stop("Error. The variable is not nominal. Execution terminated.")
  } else {
    
    library(stringr)
    
    # factor -> caracter
    if(class(nominal_feature_as_list)=="factor") { nominal_feature_as_list <- as.character(nominal_feature_as_list) }
    
    new_var_names <- as.character(data.frame(table(nominal_feature_as_list, exclude=NULL))[ , 1])
    new_var_names <- ifelse(is.na(new_var_names), "missing", new_var_names)
    new_var_names <- ifelse(new_var_names=="", "empty", new_var_names)

    nominal_feature_as_list <- data.frame(nominal_feature_as_list)
    
    for(i in 1:length(new_var_names)) {
      nominal_feature_as_list$t <- 0
      if(new_var_names[i]=="empty") {
        nominal_feature_as_list$t <- ifelse(nominal_feature_as_list[ , 1]=="", 1, nominal_feature_as_list$t)
      } else if(new_var_names[i]=="missing") {
        nominal_feature_as_list$t <- ifelse(is.na(nominal_feature_as_list[ , 1]), 1, nominal_feature_as_list$t)
      } else {
        nominal_feature_as_list$t <- ifelse(nominal_feature_as_list[ , 1]==new_var_names[i], 1, nominal_feature_as_list$t)
      }
      nominal_feature_as_list$t <- replace(nominal_feature_as_list$t,is.na(nominal_feature_as_list$t),0)
      colnames(nominal_feature_as_list)[colnames(nominal_feature_as_list)=="t"] <- paste0(prefix_for_new_variables, "_", str_sub(gsub(' ', '_', new_var_names)[i], 1, 10), "_v", i)
    }
  }
    
    return(nominal_feature_as_list[ , -1])
  }
  
  
  
#   new_var_names <- as.character(data.frame(table(nominal_feature_as_list))[ , 1])
#   
#   # clean ' '
#   new_var_names <- gsub(' ', '_', new_var_names)
#   
#   nominal_feature_as_df <- data.frame(nominal_feature_as_list)
#   
#   for(i in 1:length(new_var_names)) {
#     nominal_feature_as_df$t <- 0
#     colnames(nominal_feature_as_df)[colnames(nominal_feature_as_df)=="t"] <- paste0(prefix_for_new_variables, str_sub(new_var_names[i], 1, 5))
#   }
#   
#   for(i in (ncol(nominal_feature_as_df) - length(new_var_names) + 1):ncol(nominal_feature_as_df)) {
#     nominal_feature_as_df[ , i] <- ifelse(nominal_feature_as_df==gsub('_', ' ', new_var_names[i-(ncol(nominal_feature_as_df)) - length(new_var_names)]), 1, 0)
#   }
#   
#   return(nominal_feature_as_df)
# }
# 
# 
# 
# # testing
# x <- c("A B", "CB", NA, "A B", "A B", "CB", "A B", "", "A B", "DB", "", "A B", NA, "A B")
# if(class(x)=="factor") { x <- as.character(x) }
# 
# new_var_names <- as.character(data.frame(table(x, exclude=NULL))[ , 1])
# new_var_names <- ifelse(is.na(new_var_names), "missing", new_var_names)
# new_var_names <- ifelse(new_var_names=="", "empty", new_var_names)
# # new_var_names <- gsub(' ', '_', new_var_names)
# x <- data.frame(x)
# for(i in 1:length(new_var_names)) {
#   x$t <- 0
#   if(new_var_names[i]=="empty") {
#     x$t <- ifelse(x[ , 1]=="", 1, x$t)
#   } else if(new_var_names[i]=="missing") {
#     x$t <- ifelse(is.na(x[ , 1]), 1, x$t)
#   } else {
#     x$t <- ifelse(x[ , 1]==new_var_names[i], 1, x$t)
#   }
#   x$t <- replace(x$t,is.na(x$t),0)
#   colnames(x)[colnames(x)=="t"] <- paste0("var_", str_sub(gsub(' ', '_', new_var_names)[i], 1, 7), "_", i)
# }
# x[ , -1]
