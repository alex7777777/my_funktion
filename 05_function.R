#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: time difference between events (days)

my_time_diff <- function(df_ids, df_date, wihtout_time=F) {
  if(wihtout_time) {
    my_df <- data.frame(ids = df_ids,
                        Datum = df_date)
    my_df$delta_t <- 1
    my_df$ids2 <- 0
    my_df$ids2[1:(nrow(my_df)-1)] <- my_df$ids[2:nrow(my_df)] # displacement
    my_df$ids2[nrow(my_df)] <- NA
    my_df$delta_t = ifelse(my_df$ids==my_df$ids2,my_df$delta_t,NA) # between sequences "NA"
  } else {
    my_df <- data.frame(ids = df_ids,
                        Datum = df_date)
    my_df$Datum <- as.Date(my_df$Datum)
    my_df$t2 <- ""
    my_df$t2 <- as.Date(strptime(my_df$t2, "%Y-%m-%d %H:%M:%S"))
    my_df$t2[1:(nrow(my_df)-1)] <- my_df$Datum[2:nrow(my_df)] # displacement
    # my_df$delta_t <- as.integer(round(my_df$t2 - my_df$Datum))    # in days!!!
    my_df$delta_t <- as.integer(round((my_df$t2 - my_df$Datum)*24)) # in hours!!!
    my_df$ids2 <- 0
    my_df$ids2[1:(nrow(my_df)-1)] <- my_df$ids[2:nrow(my_df)] # displacement
    my_df$ids2[nrow(my_df)] <- NA
    my_df$delta_t = ifelse(my_df$ids==my_df$ids2,my_df$delta_t,NA) # between sequences "NA"
  }

  return(my_df$delta_t)
}
