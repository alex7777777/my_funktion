#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: input substring DF for the modeling without time
#           putput DF for the sankey diagram

my_input_for_sankey <- function(pattern_df, SUPPORT_SANKEY=0.00001) {
  
  substrings <- names(pattern_df[,-(1:2)])
  pattern_df_sum <- 0
  
  for(i in 3:(ncol(pattern_df))){
    pattern_df_sum[i-2] <- nrow(pattern_df[pattern_df[,i]>0,])
  }
  
  links <- data.frame(substrings, pattern_df_sum)
  links$support <- links$pattern_df_sum/nrow(pattern_df)
  links$pattern_df_sum <- NULL
  
  library(stringr)
  links$event_count <- str_count(links$substrings, " ")/2+1
  links <- links[order(links$event_count, -links$support), ]
  links$substrings <- as.character(links$substrings)
  links$event_count <- NULL
  
  # DF "links" & "nodes" for the sankey diagram
  library(reshape2)
  links <- cbind(links, colsplit(links$substrings, " 1 ", c("source_ch", "target_ch")))
  links <- links[,c(ncol(links)-1, ncol(links), ncol(links)-2)]
  links <- links[links$source_ch!=links$target_ch,]
  rownames(links) <- seq(0, nrow(links)-1)
  
  # load("currEventID.RData")
  event_vector <- sort(unique(c(links$source_ch, links$target_ch)))
  event_ids <- data.frame(eventID = as.numeric(as.factor(event_vector)),
                          event = event_vector)
  event_ids$event <- as.character(event_ids$event)
  str(event_ids)
  
  library(dplyr)
  links <- left_join(links, event_ids, by = c("source_ch"="event"))
  links <- left_join(links, event_ids, by = c("target_ch"="event"))
  names(links) <- c("source_ch", "target_ch", "value", "sourceID", "targetID")
  links <- links[links$value > SUPPORT_SANKEY, ]
  
  return(links)
}

