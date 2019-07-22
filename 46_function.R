# ######################################################################## #
# 2019-05-13 - by Alex Gorbach                                             #
# Generating of date strings                                               #
# ######################################################################## #

my_day_date <- function(date_from_char, number_days, date_takt=1) {
  date_list <- list()
  date_list[1] <- c(date_from_char)
  if(floor(number_days/date_takt)>=2){
    for (i in 2:floor(number_days/date_takt)){
      date_list[i] <- c(as.character(as.Date(date_list[[i-1]])+date_takt))
    }
  }
  return(date_list)
}
