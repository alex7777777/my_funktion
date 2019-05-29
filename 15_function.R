#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: without time difference

my_without_time_diff <- function(df_ids, df_date) {
  my_df <- data.frame(ids = df_ids,
                      Datum = df_date)
  my_df$delta_t <- 1
  my_df$ids2 <- 0
  my_df$ids2[1:(nrow(my_df)-1)] <- my_df$ids[2:nrow(my_df)] # displacement
  my_df$ids2[nrow(my_df)] <- NA
  my_df$delta_t = ifelse(my_df$ids==my_df$ids2,my_df$delta_t,NA) # between sequences "NA"
  
  return(my_df$delta_t)
}
