#######################################################
# 2019-01-08 - by Alex Gorbach
# DB Zugriff - Adobe
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

  SCAuth(Sys.getenv("userid"), Sys.getenv("pwd")) # Access from ".Renviron"

  report_suites <- GetReportSuites()
  elements <- GetElements(my_rs)
  metrics <- GetMetrics(my_rs)
  segments <- GetSegments(my_rs)
  
  my_adobe_df <- QueueDataWarehouse(reportsuite.id = my_rs,
                                     date.from = my_date_from,
                                     date.to   = my_date_to,
                                     metrics = my_used_metrics,
                                     elements = my_used_elements,
                                     date.granularity = "day",
                                     interval.seconds = 10,
                                     max.attempts = 200,  # =  10 - for a ~day 
                                                          # = 200 - for a ~week 
                                                          # = 700 - for ~15 days etc.
                                     enqueueOnly = F)
  
  return(my_adobe_df)
}
