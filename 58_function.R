# ######################################################################## #
# 2019-08-28 - by Alex Gorbach                                             #
# Dates generator                                                          #
# ######################################################################## #

my_dates_generator <- function(date_from, days_number)
{
  if(days_number < 1) { stop("The number of days must be at least one day") }
  
  day_list <- c()
  day_list[1] <- date_from
  
  if(days_number >= 2) {
    for(i in 2:days_number) {
    day_list[i] <- as.character(as.Date(day_list[i-1])+1)
    }
  } 
  
  return(day_list)
}
