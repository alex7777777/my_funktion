#######################################################
# 2018-01-29 - by Alex Gorbach / Svens Korrectur 26-02
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: one event during the day ()
# RULE 1. If many same events occurred on the same day, 
#         only one event should be left

my_one_event_day_rule <- function(my_df) {
  source("my_function/05_function.R")
  my_df$delta_t <- my_time_diff(my_df$ids, my_df$Datum, F)
  line_befor <- paste0("The number of lines befor remove: ", nrow(my_df))
  
  # removal of double-rend in one day
  my_df <- my_df[my_df$delta_t != 0, ]
  my_df <- na.omit(my_df)
  
  print(line_befor)
  print(paste0("The number of lines after deletion: ", nrow(my_df)))
  
  return(my_df)
}

svens_one_event_day_rule <- function(data, 
                                     differentEventsAllowed=F) {
  # About:
  #   - Corrected and easier version of Alex function. Sort out same events that occur for one ID on the same day. 
  # Arguments:
  #   - data: data frame that has the "cj_df format" with variables ids, Datum and event in that order 
  #   - differentEventsAllowed: Option to keep more events per ID and day, as long as they are distinct, 
  #                             i.e. every event that is kept represents a different car choice
  
  # START
  
  print(paste0("The number of lines before deletion: ", nrow(data)))
  
  #create and insert the variable date without time information
  data <- cbind(data, date=as.Date(data[,2])) 
  
  if(differentEventsAllowed==F) 
  {
    #Remove duplicate combinations of ids and date / output only the standard variables
    data <- data[!duplicated(data[,c("ids","date")]), c("ids", "Datum","event")]
  }else 
  {
    #Remove duplicate combinations of ids, date and event / output only the standard variables
    data <- data[!duplicated(data[,c("ids","date","event")]), c("ids", "Datum","event")]
  }
  
  print(paste0("The number of lines after deletion: ", nrow(data)))
  
  return(data)
}

