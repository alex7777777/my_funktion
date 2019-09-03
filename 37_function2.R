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
