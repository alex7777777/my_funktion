#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Сhecking and deleting aliased variables

my_check_alias <- function(dep_var_char, my_df, indep_var_list, ALIASED_REMOVE=T) {

  # my_df_alias <- alias(lm(my_df[ , dep_var_char]~ . ,
  #                            data=my_df[ , indep_var_list] ))
  
  # Remove alias predictors
  mod_alias_check <- lm(unlist(my_df[ , dep_var_char])~ . ,
                        data=my_df[ , indep_var_list])
  mod_alias_check_out <- data.frame(is.na(coef(mod_alias_check)))
  mod_alias_check_out$aliased_var <- rownames(mod_alias_check_out)
  rownames(mod_alias_check_out) <- NULL
  head(mod_alias_check_out)
  names(mod_alias_check_out) <- c("aliased_y_n", "aliased_var")
  mod_alias_check_out[mod_alias_check_out$aliased_y_n==T, ]
  remove_var_list <- mod_alias_check_out[mod_alias_check_out$aliased_y_n==T, ]$aliased_var
  remove_var_list <- gsub("`", "", remove_var_list)
  remove_var_list_nr <- c()
  for(i in 1:length(remove_var_list)) {
    remove_var_list_nr <- c(remove_var_list_nr, grep(remove_var_list[i], colnames(my_df)))
  }
  
  print("Deleting variables: ")
  print(paste0(remove_var_list_nr, ". ", remove_var_list, collapse=cat("\n")))
  
  # Remove aliased variables
  if(ALIASED_REMOVE) {
    indep_var_list <- indep_var_list[!(indep_var_list %in% remove_var_list_nr)]
  }
  
  return(indep_var_list)
}
