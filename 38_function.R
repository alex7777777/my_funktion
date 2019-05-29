#######################################################
# 2019-01-08 - by Alex Gorbach
# DB Zugriff - REWE
#######################################################

# Function: SQL-Access to the Adobe Analytics tables

my_adobe <- function(my_rs, my_date_from, my_date_to,
                     my_used_elements, my_used_metrics) {
  
  library(RSiteCatalyst)
  # # from Pedro:
  # key <- "pnadler:rewe digital"
  # secret <- "c2ba62f02fe37b8269dcec880ff7d04b"
  
  # # for me von Stefan Brandt:
  # # User Name:
  # key <- "agorbach:rewe digital"
  key <- "Alexander.Gorbach@rewe-group.com:rewe digital"
  secret <- "76ec7b8b0feca87c3d4cf852bfd3cf4c"
  # # browseURL("https://confluence.rewe-digital.com/pages/viewpage.action?pageId=315621391")
  
  SCAuth(key,secret)
  # SCAuth(Sys.getenv("USER"), Sys.getenv("SECRET")) # Access from ".Renviron"
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
