# ######################################################################## #
# 2019-05-13 - by Alex Gorbach                                             #
# Generating of date strings                                               #
# ######################################################################## #

my_day_date <- function(date_from_char, number_days) {
  
  date_list <- list()
  date_list[1] <- c(date_from_char)
  for (i in 2:(number_days)){
    date_list[i] <- c(as.character(as.Date(date_list[[i-1]])+1))
  }
  return(date_list)
}