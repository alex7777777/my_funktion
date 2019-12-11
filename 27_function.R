#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Check missings in df

my_check_missings <- function(my_df, MISSINGS_REMOVE=T) {
  
  TOTALMISSINGS <- 0
  for(i in 1:ncol(my_df)) {
    print(paste0("Percentage Missings in [", i, "] ",
                 colnames(my_df)[i], 
                 ": ", 
                 round(100*sum(is.na(my_df[,i]))/nrow(my_df),1), "%"))
    TOTALMISSINGS <- TOTALMISSINGS + sum(is.na(my_df[,i]))
  }
  
  if((TOTALMISSINGS!=0) & MISSINGS_REMOVE) {
    print(paste0("Total missings: ", TOTALMISSINGS, ". Missings will be deleted..."))
    cj_df <- na.omit(cj_df)
  } else if((TOTALMISSINGS!=0) & (!MISSINGS_REMOVE)) {
    print(paste0("Total missings: ", TOTALMISSINGS))
  } else {
  }

  return(TOTALMISSINGS)
}
