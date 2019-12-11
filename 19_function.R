#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: # Make a sequence data frame <my_df>
my_create_seq_raw_df <- function(ids_raw_df1, 
                              date_raw_df1,
                              data_typ,
                              event_ids_for_join1,
                              event_ids_for_join2,
                              event_label) {
  
  my_df <- data.frame(ids_orig = ids_raw_df1,
                      Datum = date_raw_df1,
                      eventID = event_ids_for_join1,
                      training_test = data_typ)
  
  # ids format check
  if((class(my_df$ids_orig)!="numeric")|(class(my_df$ids_orig)!="integer")) {
    my_df$ids <- as.integer(as.factor(my_df$ids_orig))
    ORIG_ID <- my_df[,c("ids", "ids_orig", "training_test")]
    ORIG_ID <- unique(ORIG_ID)
    ORIG_ID <- ORIG_ID[order(ORIG_ID$ids),]
    save(ORIG_ID, file = "r_objects/ORIG_ID.RData")
    my_df$ids_orig <- NULL
    my_df$training_test <- NULL
    my_df <- my_df[ , c("ids", "Datum", "eventID")]
  } else {
    names(my_df)[names(my_df)=="ids_orig"] <- "ids"
  }
  
  if(class(my_df$Datum)!="POSIXlt") {
    my_df$Datum <- as.POSIXlt(my_df$Datum)
  }
  
  if((class(my_df$eventID)!="numeric")|(class(my_df$eventID)!="integer")) {
    my_df$eventID <- as.integer(my_df$eventID)
  }
  
  # join events
  orig_events <- data.frame(eventID = event_ids_for_join2,
                            event = event_label )
  if((class(orig_events$eventID)!="numeric")|(class(orig_events$eventID)!="integer")) {
    orig_events$eventID <- as.integer(orig_events$eventID)
  }
  
  if(class(orig_events$event)!="character") {
    orig_events$event <- as.character(orig_events$event)
  }
  
  library(dplyr)
  my_df <- cbind(left_join(my_df[,c("ids", "eventID")], orig_events), my_df[ , 2])
  my_df$eventID <- NULL
  names(my_df) <- c("ids", "event", "Datum")
  my_df <- my_df[,c("ids", "Datum", "event")]
  
  # Сompact event recording (delete entries after the space)
  source("my_function/20_function.R")
  my_df$event <- my_compact_event_recording(my_df$event)
  
  # # Сompact event recording (delete entries after the space)
  # my_df$event <- gsub("\\ .*","",my_df$event)
  # (data.frame(table(my_df$event)))
  
  # sorting
  my_df <- my_df[order(my_df$ids, my_df$Datum), ]
  rownames(my_df) <- NULL
  
  paste0("Unique ids: ", length(unique(my_df$ids)),
         "; DF length: ", nrow(my_df),
         "; AVR sequence length: ",
         round(nrow(my_df)/length(unique(my_df$ids)),1)  )
  rownames(my_df) <- NULL
  
  return(my_df)
}
