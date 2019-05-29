#######################################################
# 2019-01-08 - by Alex Gorbach
# DB Zugriff - REWE
#######################################################

# Function: SQL-Access to the DWH table

my_dwh <- function(my_db, my_sql_string) {
  # my_db, i.e.:         my_db <- "REWE_DIGITAL"
  # my_sql_string, i.e.: my_sql_string <- "SEL * FROM REWE_DIGITAL.S_ROL_DIGITAL_KOPF_DM;"
  
  library(RODBC)
  myconn <- odbcConnect("Teradata_32")
  sqlTables(myconn, schema = my_db, tableType = "VIEW")
  # sqlColumns (myconn, "LU_D_PROMO")
  # daten <- sqlFetch(myconn, "REWE_DIGITAL.S_ROL_DIGITAL_KOPF_DM")
  
  my_dwh_df <- sqlQuery(myconn, my_sql_string)
  close(myconn)
  odbcCloseAll()
  
  return(my_dwh_df)
}
