#######################################################
# 2019-01-08 - by Alex Gorbach
# DB Zugriff
#######################################################

# Function: SQL-Access to the DWH table

my_dwh <- function(my_db, my_sql_string) {
  
  library(RODBC)
  myconn <- odbcConnect("Teradata_32")
  sqlTables(myconn, schema = my_db, tableType = "VIEW")
  
  my_dwh_df <- sqlQuery(myconn, my_sql_string)
  close(myconn)
  odbcCloseAll()
  
  return(my_dwh_df)
}
