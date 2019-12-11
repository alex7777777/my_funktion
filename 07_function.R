#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

library(TraMineR)
library(htmlwidgets)
library(data.table)

# Function: transition matrix
my_transition_matrix <- function(transitMatrix) {
  
  # convert to the TraMineR format
  transitDF <- transitMatrix[,-(1:2)]
  to.remove <- seq(from = 2, to = ncol(transitDF), by = 2)
  transitDF <- transitDF[ , -to.remove]
  
  # Sequence position
  (transitDF.alph <- seqstatl(transitDF))
  
  transitDF.seq <- seqdef(transitDF,
                          alphabet = transitDF.alph,
                          weights = transitDF$weight,
                          xtstep = 6)
  
  transitionMatrix <- as.data.frame(round(trate <- seqtrate(transitDF.seq), 3))
  transitionMatrix <- cbind(from...to = rownames(transitionMatrix), transitionMatrix)
  rownames(transitionMatrix) <- NULL
  
  return(transitionMatrix)
}
