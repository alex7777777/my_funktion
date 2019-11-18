#######################################################
# 2019-11-13 - by Alex Gorbach
#######################################################

# Function: heuristic rule.
# The function finds more than two times repeating events and reduces them.
# The same object is returned back, but only with the first and last event in repetitions.

# file structur: "ids", "Datum", "event"

my_repeating_reduction <- function(my_sqa, event_reduct="") {
  
  if(class(my_sqa$event)!="character") { my_sqa$event <- as.character(my_sqa$event)}
  
  ncol_for_return <- ncol(my_sqa)
  lines_start <- nrow(my_sqa)
  
  my_sqa$event2 <- my_sqa$event[2:(nrow(my_sqa)+1)]
  my_sqa$event2[nrow(my_sqa)] <- ""
  my_sqa$event3[2:nrow(my_sqa)] <- my_sqa$event[1:(nrow(my_sqa)-1)]
  my_sqa$event3[1] <- ""
  
  # default: reduction of all recurring events
  if(event_reduct == "") {
    my_sqa <- my_sqa[!((my_sqa$event==my_sqa$event2)
                       &(my_sqa$event==my_sqa$event3)), ]
  } else {
  # reduction of only the selected event
    my_sqa <- my_sqa[!((my_sqa$event==event_reduct)
                       &(my_sqa$event==my_sqa$event2)
                       &(my_sqa$event==my_sqa$event3)), ]
  }
  
  cat(paste0(lines_start - nrow(my_sqa),
             " lines with recurring events have been deleted\n"))
  
  return(my_sqa[ , 1:ncol_for_return])
}
