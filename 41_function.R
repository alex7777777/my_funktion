#################################################################################
# SQL String for selected IDs
#################################################################################

my_ids_string <- function(ids) {
  sql_string <- ids[1]
  for(i in 2:length(ids)){
    sql_string <- paste0(sql_string, "\", \"", ids[i])
  }
  sql_string <- paste0(" (\"", sql_string, "\") ")
  
  return(sql_string)
}
