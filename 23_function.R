#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Multicollinearity problem

my_multicoll_rm <- function(dep_var_char, my_df, ind_var_list, break_value=5, MULTICOLL_REMOVE=T)  {
  
  tmp <- ind_var_list
  
  print(paste0("Multi-collinearity: variance inflation factors. The break value: ", break_value))
  # print(paste0("Length of the input independet variables vector:", length(ind_var_list)))
  dep_var <- my_df[ , dep_var_char]
    
  ## Multi-collinearity problem
  library(car)
  vif_value <- vif(lm(dep_var~ . , data=my_df[ , ind_var_list] ))
  vif_value <- data.frame(vif_value)
  vif_value$var_names <- rownames(vif_value)
  rownames(vif_value) <- NULL
  vif_value <- vif_value[,c("var_names", "vif_value")]
  vif_value_df <- data.frame(orig_df_var_nr = ind_var_list,
                             cj_predictors = vif_value$var_names,
                             vif_value = vif_value$vif_value )
  vif_value_df <- vif_value_df[order(-vif_value_df$vif_value) , ]
  rownames(vif_value_df) <- NULL
  
  print(vif_value_df)

  nr_t <- 1
  if(vif_value_df[1,3] > break_value) {
    repeat
    {
      vif_value <- vif(lm(dep_var~ . , data=my_df[ , ind_var_list] ));
      vif_value <- data.frame(vif_value);
      vif_value$var_names <- rownames(vif_value);
      rownames(vif_value) <- NULL;
      vif_value <- vif_value[,c("var_names", "vif_value")];
      vif_value_df <- data.frame(orig_df_var_nr = ind_var_list,
                                 cj_predictors = vif_value$var_names,
                                 vif_value = vif_value$vif_value );
      vif_value_df <- vif_value_df[order(-vif_value_df$vif_value) , ];
      # print(vif_value_df);
      cat(paste0("Removal the multicollinearity variable (", nr_t, "): ", "\n<",
                       vif_value_df[1,]$cj_predictors, ">\n",
                       "Variance inflation factors (vif): ",
                       round(vif_value_df[1, ]$vif_value,1), "\n", sep = "\n"));
      vif_value_df <- vif_value_df[ 2:nrow(vif_value_df), ];
      ind_var_list <- sort(vif_value_df$orig_df_var_nr);
      nr_t <- nr_t+1;
      if(vif_value_df$vif_value[1] <= break_value) break
    }
  } else {
    print(paste0("vif_value = ", vif_value_df[1,3], " < ", break_value, ": To do nothing!"))
  }
  
  if(MULTICOLL_REMOVE) {
    ind_var_list <- sort(vif_value_df$orig_df_var_nr)
  } else {
    ind_var_list <- tmp
  }
  
  return(ind_var_list)
}
