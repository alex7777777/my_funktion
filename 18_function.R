#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Do help variables
my_help_var <- function(my_df, 
                        deadline=as.Date("2017-01-01"),
                        numb_new_var=12, 
                        var_labels="QNr", 
                        fill_values=NA) 
  {
  
  my_df$Datum <- as.Date(my_df$Datum)
  
  my_df_training <- my_df[ my_df$Datum <  deadline, ]
  my_df_result   <- my_df[ my_df$Datum >= deadline, ]

  # Data Frame "data_users"
  users <- sort(unique(my_df_training$ids))
  data_users <- data.frame(ids = users)

  # Removal of excess levels of factors
  t <- my_df_training$ids[drop = T,]
  my_df_training$ids <- t

  # Additional predictor: number of days from the last carsharing
  my_df_training$day_for <- as.numeric(max(my_df_training$Datum) - my_df_training$Datum)

  # How many times did the customer rent a car
  data_users$count <- as.numeric(table(my_df_training$ids))
  
  # Dependent variable from my_df_result (NEW: Rent "0" vs. not Rent "1")
  data_users$rent_2017 <- 1 # 0
  data_users$rent_2017 <- ifelse(data_users$ids %in% my_df_result$ids, 0, 1) # 1, 0) 
  
  print(paste0("Percentage of cars leased in the last half of the year: ", 
               round(100*table(data_users$rent_2017)[1]/nrow(data_users),1), "%")) # [2]
  
  data_users$day_for  <- tapply(my_df_training$day_for, my_df_training$ids, min)
  
  source("my_function/17_function.R")
  data_users <- my_add_new_var(data_users, numb_new_var, var_labels, 0)
  
  ids_rent_2014_q1 <- unique(my_df_training[ my_df_training$Datum > "2013-12-31" & my_df_training$Datum <= "2014-03-31",]$ids)
  ids_rent_2014_q2 <- unique(my_df_training[ my_df_training$Datum > "2014-03-31" & my_df_training$Datum <= "2014-06-30",]$ids)
  ids_rent_2014_q3 <- unique(my_df_training[ my_df_training$Datum > "2014-06-30" & my_df_training$Datum <= "2014-09-30",]$ids)
  ids_rent_2014_q4 <- unique(my_df_training[ my_df_training$Datum > "2014-09-30" & my_df_training$Datum <= "2014-12-31",]$ids)
  ids_rent_2015_q1 <- unique(my_df_training[ my_df_training$Datum > "2014-12-31" & my_df_training$Datum <= "2015-03-31",]$ids)
  ids_rent_2015_q2 <- unique(my_df_training[ my_df_training$Datum > "2015-03-31" & my_df_training$Datum <= "2015-06-30",]$ids)
  ids_rent_2015_q3 <- unique(my_df_training[ my_df_training$Datum > "2015-06-30" & my_df_training$Datum <= "2015-09-30",]$ids)
  ids_rent_2015_q4 <- unique(my_df_training[ my_df_training$Datum > "2015-09-30" & my_df_training$Datum <= "2015-12-31",]$ids)
  ids_rent_2016_q1 <- unique(my_df_training[ my_df_training$Datum > "2015-12-31" & my_df_training$Datum <= "2016-03-31",]$ids)
  ids_rent_2016_q2 <- unique(my_df_training[ my_df_training$Datum > "2016-03-31" & my_df_training$Datum <= "2016-06-30",]$ids)
  ids_rent_2016_q3 <- unique(my_df_training[ my_df_training$Datum > "2016-06-30" & my_df_training$Datum <= "2016-09-30",]$ids)
  ids_rent_2016_q4 <- unique(my_df_training[ my_df_training$Datum > "2016-09-30" & my_df_training$Datum <= "2016-12-31",]$ids)
  
  ids_rent_q <- list()

  ids_rent_q$ids_rent_2014_q1 <- ids_rent_2014_q1
  ids_rent_q$ids_rent_2014_q2 <- ids_rent_2014_q2
  ids_rent_q$ids_rent_2014_q3 <- ids_rent_2014_q3
  ids_rent_q$ids_rent_2014_q4 <- ids_rent_2014_q4
  
  ids_rent_q$ids_rent_2015_q1 <- ids_rent_2015_q1
  ids_rent_q$ids_rent_2015_q2 <- ids_rent_2015_q2
  ids_rent_q$ids_rent_2015_q3 <- ids_rent_2015_q3
  ids_rent_q$ids_rent_2015_q4 <- ids_rent_2015_q4
  
  ids_rent_q$ids_rent_2016_q1 <- ids_rent_2016_q1
  ids_rent_q$ids_rent_2016_q2 <- ids_rent_2016_q2
  ids_rent_q$ids_rent_2016_q3 <- ids_rent_2016_q3
  ids_rent_q$ids_rent_2016_q4 <- ids_rent_2016_q4
  
  for(i in (ncol(data_users)-numb_new_var+1):ncol(data_users)) {
    data_users[,i] <- ifelse(data_users$ids %in% ids_rent_q[[i-(ncol(data_users)-numb_new_var)]], 1, 0)
  }
  
  # Quarterly statistics
  for(i in 5:ncol(data_users)) {
    print(paste0(i-4, ". Quarterly statistics for the ", names(data_users)[i], ": ", 
                 round(100*table(data_users[,i])[2]/nrow(data_users), 2), "% were rented")) # [2]
  }
  
  return(data_users)
}
