#######################################################
# 2019-07-23 - by Alex Gorbach                        #
# DB Zugriff - Adobe                                  #
#######################################################

# Function: SQL-Access to the Adobe Analytics tables

options(scipen = 999) # no scientific notation

library(RSiteCatalyst)
library(curl)
library(tibble)
library(dplyr)
library(stringr)

my_adobe <- function(my_rs, my_date_from, my_date_to,
                     my_used_elements, my_used_metrics) {
  
  library(RSiteCatalyst)

  # key <- Sys.getenv("userid") # Zugriff zu '.Renviron'
  # secret <- Sys.getenv("pwd") # ebd.
  
  SCAuth(Sys.getenv("userid"), Sys.getenv("pwd")) # Access from ".Renviron"

  report_suites <- GetReportSuites()
  elements <- GetElements(my_rs)
  metrics <- GetMetrics(my_rs)
  segments <- GetSegments(rs_web)
  
  # dynamic adjustment of the number of attempts 
  # depending on the number of days for data analysis
  attempts <- 10 * round(as.integer((as.Date(my_date_to) - as.Date(my_date_from) + 1)) ^ 0.8)
  
  #  1x day:  attempts=10
  #  2x days: attempts=20
  #  7x days: attempts=50
  # 15x days: attempts=90
  # 30x days: attempts=150
  
  print(paste("max.attempts =", attempts))
  
  my_adobe_df <- QueueDataWarehouse(reportsuite.id = my_rs,
                                    date.from = my_date_from,
                                    date.to   = my_date_to,
                                    metrics = my_used_metrics,
                                    elements = my_used_elements,
                                    date.granularity = "hour", # = "day"
                                    interval.seconds = 10,
                                    max.attempts = attempts,
                                    # max.attempts = 200, # =  ~10 - for a day 
                                    enqueueOnly = F)
  
  return(my_adobe_df)
}
