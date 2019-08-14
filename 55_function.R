# ######################################################################## #
# 2019-08-14 - by Alex Gorbach                                             #
# Last full week generator                                                 #
# ######################################################################## #

# Calculate of start and end dates of the next full calendar
# week in relation to the current date

my_last_full_week <- function() {
  
  cur_date <- Sys.Date()
  
  cur_weekday <- format(cur_date,"%A")
  last_sunday_date <- switch(cur_weekday,
                             "Montag"     = cur_date - 1,
                             "Dienstag"   = cur_date - 2,
                             "Mittwoch"   = cur_date - 3,
                             "Donnerstag" = cur_date - 4,
                             "Freitag"    = cur_date - 5,
                             "Samstag"    = cur_date - 6,
                             "Sonntag"    = cur_date - 7)
  
  last_full_week_list <- list()
  
  last_full_week_list[1] <- as.character(last_sunday_date - 6)
  last_full_week_list[2] <- as.character(last_sunday_date)
  
  return(last_full_week_list)
  
}