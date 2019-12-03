#######################################################
# 2019-12-03 - by Alex Gorbach
#######################################################

# Function: cutting sequences into parts ending with the selected event 
#           Usually this is a dependent variable 'purchase'

# fancier text progress
# install.packages("svMisc")
require(svMisc)

my_sequences_cutting <- function(DF, cut_seq_event="Kauf", save_Y_N="Y") {
  
  # Step 1: set int. parameter
  ids_vector <- unique(DF$ids)
  length_df <- nrow(DF)
  length_uniq_id <- length(unique(DF$ids))
  DF$new_ids <- NA
  t <- rep("=", length_uniq_id)
  
  cut("\n")
  # Step 2: loop
  for(i in 1:length_uniq_id) { 
    
    progress(round(101*(i)/length_uniq_id))
    
    DF2 <- DF[DF$ids %in% ids_vector[i], ]
    
    # Delete sale-duplicates 'cut_seq_event' in one identifier
    DF2$event2[2:nrow(DF2)] <- DF2$event[1:(nrow(DF2)-1)]
    DF2 <- DF2[!((DF2$event==DF2$event2)&(DF2$event==cut_seq_event)&(!is.na(DF2$event2))), ]
    if(DF2[1, ]$event==cut_seq_event) { DF2 <- DF2[2:nrow(DF2), ]}
    
    DF2$event2 <- NULL
    
    if(cut_seq_event %in% DF2$event) {
      DF2[DF2$event==cut_seq_event,]$delta_t <- NA
      DF2$new_ids <- rownames(DF2)
      DF2$new_ids <- paste0(DF2$new_ids, "_", DF2$ids)
      DF2[DF2$event!=cut_seq_event,]$new_ids <- NA
    } else {
      DF2$new_ids[nrow(DF2)] <-  paste0(rownames(DF2)[nrow(DF2)], "_", DF2$ids[nrow(DF2)])
    }
    
    # Fill gaps
    DF2 <- DF2[order(-as.integer(rownames(DF2))), ]
    DF2$new_ids <- fillNAgaps(DF2$new_ids)
    DF2 <- DF2[order(as.integer(rownames(DF2))), ]
    
    DF <- rbind(DF, DF2)
    
  }
  # End loop
  
  # Select new sequences
  DF <- DF[!is.na(DF$new_ids), ]
  rownames(DF) <- NULL
  DF$new_ids <- as.integer(as.factor(DF$new_ids))
  
  # Optional:
  if(save_Y_N=="Y") {
    save_ids_matching <- unique(DF[ , c("ids", "new_ids")])
    save_ids_matching <- save_ids_matching[order(save_ids_matching$ids, save_ids_matching$new_ids), ]
    save(save_ids_matching, file = "save_ids_matching.RObject")
  }
  
  # Return DF
  DF <- DF[ , c("ids", "new_ids", "Datum", "event", "delta_t")]
  names(DF) <- c("old_ids", "ids", "Datum", "event", "delta_t")

  return(DF)
}

fillNAgaps <- function(x, firstBack=FALSE) {
  ## NA's in a vector or factor are replaced with last non-NA values
  ## If firstBack is TRUE, it will fill in leading NA's with the first
  ## non-NA value. If FALSE, it will not change leading NA's.
  
  # If it's a factor, store the level labels and convert to integer
  lvls <- NULL
  if (is.factor(x)) {
    lvls <- levels(x)
    x    <- as.integer(x)
  }
  
  goodIdx <- !is.na(x)
  
  # These are the non-NA values from x only
  # Add a leading NA or take the first good value, depending on firstBack   
  if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
  else             goodVals <- c(NA,            x[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  x <- goodVals[fillIdx]
  
  # If it was originally a factor, convert it back
  if (!is.null(lvls)) {
    x <- factor(x, levels=seq_along(lvls), labels=lvls)
  }
  x
}
