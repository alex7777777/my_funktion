#######################################################
# 2019-01-08 - by Alex Gorbach
# DB Zugriff - REWE
#######################################################

# Function: SQL-Access to the BigQuery tables

my_bigquery <- function(my_project, my_sql, my_sql_type = F) {
  
  library(bigrquery)
  my_bigquery_df <- query_exec(my_sql, project = my_project, 
                               use_legacy_sql = my_sql_type,
                               max_pages = Inf) 
  
  return(my_bigquery_df)
}
