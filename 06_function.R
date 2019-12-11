#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Functions: Lines alignment and data sequencing functions

as.data.frame.list <- function(data)  # Lines alignment function
{
  nCol <- max(vapply(data, length, 0))
  data <- lapply(data, function(row) c(row, rep(NA, nCol-length(row))))
  data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
  data.frame(data)
}

my_data_sequencing <- function(data, seq_length = 1) {
  
  # gc(TRUE)
  
  transitSQ <- as.data.frame(unique(data$ids))
  names(transitSQ) <- c("ids")
  # Formation of the two lists
  l_event <- split(data$event, data$ids)
  l_delta <- split(data$delta_t, data$ids)
  DF2 <- as.data.frame(l_event)
  DF3 <- as.data.frame(l_delta)
  transitSQ = cbind(transitSQ,lght= 2*unlist(lapply(l_event, length))-1)
  for (i in 1:ncol(DF2))
  {
    transitSQ = cbind (transitSQ,DF2[,i])
    transitSQ = cbind (transitSQ,DF3[,i])
  }
  colnames(transitSQ)[1] = "ids"
  colnames(transitSQ)[3:ncol(transitSQ)] =  paste("ev_nr", 1:(2*ncol(DF2)), sep="_")
  
  # all events as character
  for(i in 3:ncol(transitSQ)){
    transitSQ[,i] <- as.character(transitSQ[,i])
  }
  
  transitSQ <- transitSQ[transitSQ$lght >= seq_length, ]
  
  # gc(reset = TRUE)
  
  return(transitSQ)
}
