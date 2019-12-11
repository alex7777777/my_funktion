#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: create substring analysis (support) from the pattern db

my_create_support <- function(pattern_db) {
  library(stringr)
  my_df <- pattern_db[ , -(1:2)]
  substrings <- names(my_df)
  cj_df_sum  <- 0
  cj_df_sum2 <- 0
  for(i in 1:(ncol(my_df))){
    cj_df_sum[i]  <- sum(my_df[,i])
    cj_df_sum2[i] <- nrow(my_df[my_df[,i]>0,])
  }
  substr_support <- data.frame(substrings, cj_df_sum, cj_df_sum2)
  substr_support$support <- substr_support$cj_df_sum2/nrow(my_df)
  substr_support$cj_df_sum2 <- NULL
  substr_support$event_count <- str_count(substr_support$substrings, " ")/2+1
  substr_support$substrings <- as.character(substr_support$substrings)
  rownames(substr_support) <- NULL
  
  return(substr_support)
} 
