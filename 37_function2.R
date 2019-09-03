#######################################################
# 2019-09-02 - by Alex S
# DB Zugriff - Write Teradata Table into DWH
#######################################################

write_to_DWH <- function(object, table_name) {
  #conn <- odbcConnect("Teradata_32")
  
  # Write to DB -> only works if table already exists
  # Hacky Workaround: First Execution Fails but Writes Table -> Then Append to Table
  
  if (!(dbExistsTable(conn, table_name))) {
    try(dbWriteTable(conn = conn, 
                     name = table_name, 
                     value = object,
                     row.names = FALSE,
                     overwrite = TRUE))
    
    write_to_DWH(object, table_name) #recursive so it will first create table, then append new data
    
  } else {
    dbWriteTable(conn = conn, 
                 name = table_name, 
                 value = object,
                 row.names = FALSE,
                 overwrite = FALSE,
                 append = TRUE)
  }
}



# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #          MAIN SCRIPT            ~
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# library(lubridate)
# library(bigrquery)
# library(jsonlite)
# library(tidyr)
# library(dplyr)
# library(ggplot2)
# library(httpuv)
# library(RJDBC) # DB Connect
# library(RODBC) # DB Connect
# library(dbplyr)
# library(stringr)
# library('RCurl')
# library(stringr)
# library(readr)
# 
# options(digits = 2)
# options("warning.length"=8000)
# options(scipen = 999)
# 

# # Store webservice keys and authentication details
# # to write in 'RProfile' in 'C:\Users\R\R-3.5.1\library\base\R\RProfile':
# Sys.setenv( TERA_USER = "my_orig_tera_user" )
# Sys.setenv( TERA_PW = "my_orig_tera_pass" )


# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #                DB Connection                   ~
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TD_USER <- Sys.getenv("TERA_USER")    # login    - vgl. function_38.R
# TD_PW <- Sys.getenv("TERA_PW")        # password
# 
# HOMEDRIVE <- Sys.getenv("HOMEDRIVE")
# HOMEPATH <- Sys.getenv("HOMEPATH")
# HOME <- paste0(HOMEDRIVE, HOMEPATH)
# 
# # TODO: Adjust Path to Teradata Drivers
# PATH_TERAJDBC <- paste0(HOME, "\\Software\\TeraJDBC__indep_indep.16.20.00.08\\terajdbc4.jar")
# PATH_TERACONFIG <- paste0(HOME, "\\Software\\TeraJDBC__indep_indep.16.20.00.08\\tdgssconfig.jar")
# PATH_TERA <- paste(PATH_TERAJDBC, PATH_TERACONFIG, sep=";")
# 
# # Setup JDBC Driver: WAY faster reading data than ODBC 
# drv <- JDBC("com.teradata.jdbc.TeraDriver", PATH_TERA)
# .jaddClassPath(PATH_TERACONFIG)
# conn <- dbConnect(drv,
#                   "jdbc:teradata://td2.eil.risnet.de/DATABASE=REWE_DIGITAL,DBS_PORT=1025,charSet=UTF8,LOB_SUPPORT=OFF",
#                   TD_USER, TD_PW)
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #          Write BigQuery Data to DWH            ~
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # the table to write: 'tbl_bigquery':
# 
# dbRemoveTable(conn, "OCMA_BIGQUERY")
# dbExistsTable(conn, "OCMA_BIGQUERY")
# 
# write_to_DWH(object = tbl_bigquery,
#              table_name = "OCMA_BIGQUERY")
