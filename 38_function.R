#######################################################
# 2019-01-08 - by Alex Gorbach
# DB Zugriff - Adobe
#######################################################

# Function: SQL-Access to the Adobe Analytics tables

my_adobe <- function(my_rs, my_date_from, my_date_to,
                     my_used_elements, my_used_metrics) {
  
  library(RSiteCatalyst)
  
  # Access from the local file '.Renviron'
  SCAuth(Sys.getenv("userid"), Sys.getenv("pwd"))
  report_suites <- GetReportSuites()
  elements <- GetElements(my_rs)
  metrics <- GetMetrics(my_rs)
  
  my_adobe_df <- QueueDataWarehouse(reportsuite.id = rs,
                                     date.from = my_date_from,
                                     date.to   = my_date_to,
                                     metrics = my_used_metrics,
                                     elements = my_used_elements,
                                     date.granularity = "day",
                                     interval.seconds = 10,
                                     max.attempts = 200, # 200
                                     enqueueOnly = F)
  
  return(my_adobe_df)
}
