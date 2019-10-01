#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: time difference between events
# Coding for the time difference, parameter 'scaling_time':
# "" - DEFAULT: dummy time difference ("1" between all events)
# S  - sec.
# M  - min.
# H  - hour
# d  - days (default)
# w  - week
# m  - month
# q  - quarter
# y  - year

my_time_diff <- function(df_ids, df_date, scaling_time="") {
  my_df <- data.frame(ids = df_ids,
                      Datum = df_date)
  my_df$Datum <- as.POSIXlt(my_df$Datum, tryFormats = "%Y-%m-%d %H:%M:%S")
  my_df$t2 <- ""
  my_df$t2 <- as.POSIXlt(strptime(my_df$t2, "%Y-%m-%d %H:%M:%S"))
  my_df$t2[1:(nrow(my_df)-1)] <- my_df$Datum[2:nrow(my_df)] # displacement
  if(scaling_time == "S") {
    my_df$delta_t <- as.numeric(round((my_df$t2 - my_df$Datum)))                               # sec
  } else if(scaling_time == "M") {
    my_df$delta_t <- as.numeric(round((my_df$t2 - my_df$Datum)/60))                            # min
  } else if(scaling_time == "H") {
    my_df$delta_t <- as.numeric(round((my_df$t2 - my_df$Datum)/(60*60), digits = 1))           # hour
  } else if(scaling_time == "d") {
    my_df$delta_t <- as.numeric(round((my_df$t2 - my_df$Datum)/(24*60*60), digits = 1))        # days
  } else if(scaling_time == "w") {
    my_df$delta_t <- as.numeric(round((my_df$t2 - my_df$Datum)/(24*60*60*7), digits = 1))      # week
  } else if(scaling_time == "m") {
    my_df$delta_t <- as.numeric(round((my_df$t2 - my_df$Datum)/(24*60*60*30.5), digits = 1))   # month
  } else if(scaling_time == "q") {
    my_df$delta_t <- as.numeric(round((my_df$t2 - my_df$Datum)/(24*60*60*3*30.5), digits = 1)) # quarter
  } else if(scaling_time == "y") {
    my_df$delta_t <- as.numeric(round((my_df$t2 - my_df$Datum)/(24*60*60*365.25), digits = 1)) # year
  } else {
    my_df$delta_t <- 1
    print("The time difference for other formats is not defined here: dummy time")
  }
  my_df$ids2 <- 0
  my_df$ids2[1:(nrow(my_df)-1)] <- my_df$ids[2:nrow(my_df)] # displacement
  my_df$ids2[nrow(my_df)] <- NA
  my_df$delta_t = ifelse(my_df$ids==my_df$ids2,my_df$delta_t,NA) # between sequences "NA"

  return(my_df$delta_t)
}
