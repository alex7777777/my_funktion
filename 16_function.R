#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Substring DF function for the sankey input
# SUPPORT_OR_TOP=T: Default "support"

my_pattern_df_for_sankey <- function(data_seq,
                          SUPPORT2er=0.0025,
                          TOP_n_2er=80,
                          SUPPORT_OR_TOP=T) {
  
  data_seq_head <- data_seq[,1:2]
  data_seq <- data_seq[,-(1:2)]
  
  # event-to-srting function
  events_to_string <- function(events){
    events <- events[is.na(events) == F]
    t <- paste(as.character(events))
    return(paste(t, collapse = " "))
  }
  
  # events as 1x variable with the " "
  data_string <- data.frame(events = as.character(apply(data_seq, 1, events_to_string)))
  
  if(class(data_string$events) != "character") {
    data_string$events <- as.character(data_string$events)
  }
  
  # 2x substring analysis
  
  for(i in 0:((ncol(data_seq)/2)-2)){
    k <- floor(i*2) + 1
    print(k)
    
    temp <- data_seq[,k:(k+2)]
    temp <- na.omit(temp)
    
    summary(temp)
    sequenzes <- as.character(apply(temp, 1, events_to_string))
    i_seq <- (sequenzes)
    if (i == 0) seq <- i_seq else seq <- c(seq, i_seq)
  }
  
  # sort(table(seq), decreasing = F)/length(seq)
  
  events_big <- (sort(table(seq), decreasing = T)/length(seq))
  
  # print(paste0("Support of 2er events: ", SUPPORT2er))
  # print(paste0("Number of 2er events: " , TOP_n_2er))
  
  if(SUPPORT_OR_TOP) {
    events_small <- events_big[events_big >= SUPPORT2er]
  } else {
    events_small <- events_big[1:TOP_n_2er]
  }
  
  # print(paste0("Check: Number of 2er events: " , length(events_small)))
  
  events_names <- row.names(events_small)
  unique_seq_2 <- sort(unique(events_names))
  
  # events frequenz
  library(stringr)
  
  # 2
  data_frequenz_2 <- data.frame(t = 1:nrow(data_string))
  for(i in 1:length(unique_seq_2)){
    data_frequenz_2[,i] <- 0
    names(data_frequenz_2)[i] <- as.character(unique_seq_2[i])
  }
  
  for(i in 1:length(unique_seq_2)){
    event_pattern <- as.character(unique_seq_2[i])
    print(event_pattern)
    count <- str_count(data_string$events, event_pattern)
    data_frequenz_2[,i] <- count
  }
  
  df_to_smartdata <- cbind(data_seq_head, data_frequenz_2)
  # remove null variables
  not_null_vector <- c(1,2)
  
  for(i in 3:ncol(df_to_smartdata)) {
    if(sum(df_to_smartdata[ , i]) > 0) {
      not_null_vector <- c(not_null_vector, i)
    }
  }
  
  df_to_smartdata <- df_to_smartdata[ , not_null_vector]
  
  return(df_to_smartdata)
}
