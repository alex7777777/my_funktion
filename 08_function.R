#######################################################
# 2018-01-29 - by Alex Gorbach / Update 2019-11-05
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: one event during the day
# RULE 1. If many same events occurred on the same day, 
#         only one event should be left

get_all_my_function  <- function(funct_name) {
  url_my_function <- "https://raw.githubusercontent.com/alex7777777/my_funktion/master/"
  source(url(paste0(url_my_function, funct_name)))
  closeAllConnections()
}

my_one_event_day_rule <- function(my_df) {
  get_all_my_function("05_function.R")
  my_df$delta_t <- round(my_time_diff(my_df$ids, my_df$Datum, "d"))
  line_befor <- nrow(my_df)
  
  my_df$event2 <- my_df$event[2:(nrow(my_df)+1)]
  
  select_rows_to_delete <- rownames(my_df[(my_df$event == my_df$event2) & (my_df$delta_t==0) & (!is.na(my_df$delta_t)),])
  
  # removal of one-day-double
  '%ni%' <- Negate('%in%')
  my_df <- my_df[rownames(my_df) %ni% select_rows_to_delete, ]
  
  cat(paste0("The number of lines befor remove: ", line_befor, "\n",
             "The number of lines after deletion: ", nrow(my_df)))
  
  return(my_df[,1:3])
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
  } else 
  {
    #Remove duplicate combinations of ids, date and event / output only the standard variables
    data <- data[!duplicated(data[,c("ids","date","event")]), c("ids", "Datum","event")]
  }
  
  print(paste0("The number of lines after deletion: ", nrow(data)))
  
  rownames(data) <- NULL
  
  return(data)
}
