#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Substring DF function for the modeling
# for 2er, 3er, 4er, 5er substrings
# Default: SUPPORT_OR_TOP=T (support-parameter)

my_pattern_df <- function(data_seq, 
                          SUPPORT1er=0.00000001, # all events
                          SUPPORT2er=0.004,
                          SUPPORT3er=0.006,
                          SUPPORT4er=0.003,
                          SUPPORT5er=0.0012,
                          TOP_n_1er=100,         # all events
                          TOP_n_2er=50,
                          TOP_n_3er=20,
                          TOP_n_4er=15,
                          TOP_n_5er=10,
                          SUPPORT_OR_TOP=T) {
  
  gc(reset = TRUE)
  
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
  
  # 1x substring analysis
  
  # all substrings as a big string
  for(i in 0:((ncol(data_seq)/2)-1)){
    k <- floor(i*2) + 1
    # if(k%%100==25) { print(k) }
    i_seq <- (data_seq[,k])
    if (i == 0) seq <- i_seq else seq <- c(seq, i_seq)
  }
  
  # Counting events:

  # Normalizing events:
  events_big <- (sort(table(seq), decreasing = T)/length(seq))
  
  events_big[1:length(events_big)]
  
  if(SUPPORT_OR_TOP) {
    events_small <- events_big[events_big >= SUPPORT1er]
  } else if(TOP_n_1er < length(events_big)) {
    events_small <- events_big[1:TOP_n_1er]
  } else { events_small <- events_big[1:length(events_big)] }
  
  events_names <- row.names(events_small)
  unique_seq_1 <- sort(events_names)
  
  
  # 2x substring analysis
  
  for(i in 0:((ncol(data_seq)/2)-2)){
    k <- floor(i*2) + 1
    # if(k%%100==25) { print(k) }
    temp <- data_seq[,k:(k+2)]
    temp <- na.omit(temp)
    summary(temp)
    sequenzes <- as.character(apply(temp, 1, events_to_string))
    i_seq <- (sequenzes)
    if (i == 0) seq <- i_seq else seq <- c(seq, i_seq)
  }
  
  events_big <- (sort(table(seq), decreasing = T)/length(seq))
  
  if(SUPPORT_OR_TOP) {
    events_small <- events_big[events_big >= SUPPORT2er]
  } else {
    events_small <- events_big[1:TOP_n_2er]
  }
  
  events_names <- row.names(events_small)
  unique_seq_2 <- sort(unique(events_names))
  
  
  # 3x substring analysis
  
  for(i in 0:((ncol(data_seq)/2)-4)){
    k <- floor(i*2) + 1
    # if(k%%100==25) { print(k) }
    temp <- data_seq[,k:(k+4)]
    temp <- na.omit(temp)
    sequenzes <- as.character(apply(temp, 1, events_to_string))
    i_seq <- (sequenzes)
    if (i == 0) seq <- i_seq else seq <- c(seq, i_seq)
  }
  
  events_big <- (sort(table(seq), decreasing = T)/length(seq))
  
  if(SUPPORT_OR_TOP) {
    events_small <- events_big[events_big >= SUPPORT3er]
  } else {
    events_small <- events_big[1:TOP_n_3er]
  }
  
  events_names <- row.names(events_small)
  unique_seq_3 <- sort(unique(events_names))
  
  
  # 4x substring analysis
  
  for(i in 0:((ncol(data_seq)/2)-6)){
    k <- floor(i*2) + 1
    # if(k%%100==25) { print(k) }
    temp <- data_seq[,k:(k+6)]
    temp <- na.omit(temp)
    sequenzes <- as.character(apply(temp, 1, events_to_string))
    i_seq <- (sequenzes)
    if (i == 0) seq <- i_seq else seq <- c(seq, i_seq)
  }
  
  events_big <- (sort(table(seq), decreasing = T)/length(seq))
  
  if(SUPPORT_OR_TOP) {
    events_small <- events_big[events_big >= SUPPORT4er]
  } else {
    events_small <- events_big[1:TOP_n_4er]
  }
  
  events_names <- row.names(events_small)
  unique_seq_4 <- sort(unique(events_names))
  
  
  # 5x substring analysis
  
  for(i in 0:((ncol(data_seq)/2)-8)){
    k <- floor(i*2) + 1
    # if(k%%100==25) { print(k) }
    temp <- data_seq[,k:(k+8)]
    temp <- na.omit(temp)
    sequenzes <- as.character(apply(temp, 1, events_to_string))
    i_seq <- (sequenzes)
    if (i == 0) seq <- i_seq else seq <- c(seq, i_seq)
  }
  
  events_big <- (sort(table(seq), decreasing = T)/length(seq))
  
  if(SUPPORT_OR_TOP) {
    events_small <- events_big[events_big >= SUPPORT5er]
  } else {
    events_small <- events_big[1:TOP_n_5er]
  }
  
  events_names <- row.names(events_small)
  unique_seq_5 <- sort(unique(events_names))
  
  # events frequenz
  
  library(stringr)
  
  # 1
  data_frequenz_1 <- data.frame(t = 1:nrow(data_string))
  for(i in 1:length(unique_seq_1)){
    data_frequenz_1[,i] <- 0
    names(data_frequenz_1)[i] <- as.character(unique_seq_1[i])
  }
  
  for(i in 1:length(unique_seq_1)){
    event_pattern <- as.character(unique_seq_1[i])
    count <- str_count(data_string$events, event_pattern)
    data_frequenz_1[,i] <- count
  }
  
  # 2
  data_frequenz_2 <- data.frame(t = 1:nrow(data_string))
  for(i in 1:length(unique_seq_2)){
    data_frequenz_2[,i] <- 0
    names(data_frequenz_2)[i] <- as.character(unique_seq_2[i])
  }
  
  for(i in 1:length(unique_seq_2)){
    event_pattern <- as.character(unique_seq_2[i])
    count <- str_count(data_string$events, event_pattern)
    data_frequenz_2[,i] <- count
  }
  
  # 3
  data_frequenz_3 <- data.frame(t = 1:nrow(data_string))
  for(i in 1:length(unique_seq_3)){
    data_frequenz_3[,i] <- 0
    names(data_frequenz_3)[i] <- as.character(unique_seq_3[i])
  }
  
  for(i in 1:length(unique_seq_3)){
    event_pattern <- as.character(unique_seq_3[i])
    count <- str_count(data_string$events, event_pattern)
    data_frequenz_3[,i] <- count
  }
  
  # 4
  data_frequenz_4 <- data.frame(t = 1:nrow(data_string))
  for(i in 1:length(unique_seq_4)){
    data_frequenz_4[,i] <- 0
    names(data_frequenz_4)[i] <- as.character(unique_seq_4[i])
  }
  
  for(i in 1:length(unique_seq_4)){
    event_pattern <- as.character(unique_seq_4[i])
    count <- str_count(data_string$events, event_pattern)
    data_frequenz_4[,i] <- count
  }
  
  # 5
  data_frequenz_5 <- data.frame(t = 1:nrow(data_string))
  for(i in 1:length(unique_seq_5)){
    data_frequenz_5[,i] <- 0
    names(data_frequenz_5)[i] <- as.character(unique_seq_5[i])
  }
  
  for(i in 1:length(unique_seq_5)){
    event_pattern <- as.character(unique_seq_5[i])
    count <- str_count(data_string$events, event_pattern)
    data_frequenz_5[,i] <- count
  }
  
  df_to_smartdata <- cbind(data_seq_head, 
                           data_frequenz_1, 
                           data_frequenz_2, 
                           data_frequenz_3, 
                           data_frequenz_4, 
                           data_frequenz_5)
  # remove null variables
  not_null_vector <- c(1,2)
  
  for(i in 3:ncol(df_to_smartdata)) {
    if(sum(df_to_smartdata[ , i]) > 0) {
      not_null_vector <- c(not_null_vector, i)
    }
  }
  
  df_to_smartdata <- df_to_smartdata[ , not_null_vector]
  
  df_to_smartdata$lght <- (df_to_smartdata$lght+1)/2
  
  rownames(df_to_smartdata) <- NULL
  
  gc(reset = TRUE)
  
  return(df_to_smartdata)
}
