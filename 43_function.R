# ######################################################################## #
# 2019-05-27 - by Alex Gorbach                                             #
# Log file for check number of ids in data frame                           #
# ######################################################################## #

library(dplyr)

my_spy <- function(df_ids_to_check, 
                   time_check=NA,
                   # timestamp_var_name=F,
                   days_retro=NA,
                   name_spy_file='my_spy_logfile.csv',
                   folder_for_save_check_data='csv_tab',
                   number_max_ids_to_check=3) {
  
  daily <- logical()
  if(max(time_check)-min(time_check) <= 1) { daily <- T } else { daily <- F }
  
  check_df <- data.frame("time_check" = Sys.time(),
                         "df_name" = gsub("\\[.*","", deparse(substitute(df_ids_to_check))),
                         "daily_vs_at_once" = if(daily) "daily" else "at once",
                         "date_from" = if(!is.na(time_check[1])) { min(time_check) } else NA,
                         "date_to" = if(!is.na(time_check[1])) { max(time_check) } else NA,
                         "days_retro" = days_retro,
                         "id1_name" = if(is.na(names(df_ids_to_check)[1])) NA else {names(df_ids_to_check)[1]},
                         "id1_uniq" = if(is.na(names(df_ids_to_check)[1])) NA else {n_distinct(df_ids_to_check[,1])},
                         "id2_name" = if(is.na(names(df_ids_to_check)[2])) NA else {names(df_ids_to_check)[2]},
                         "id2_uniq" = if(is.na(names(df_ids_to_check)[2])) NA else {n_distinct(df_ids_to_check[,2])},
                         "id3_name" = if(is.na(names(df_ids_to_check)[3])) NA else {names(df_ids_to_check)[3]},
                         "id3_uniq" = if(is.na(names(df_ids_to_check)[3])) NA else {n_distinct(df_ids_to_check[,3])},
                         stringsAsFactors = F)
  
  string_for_spy_file <- paste0(folder_for_save_check_data, "/", name_spy_file)
  
  if(file.exists(string_for_spy_file)) {
    FLAG_col.names <- F
  } else {
    FLAG_col.names <- T
  }
  
  write.table(check_df, file = string_for_spy_file, append=T, row.names=F, col.names = FLAG_col.names, na="", sep=";")
  
  return(paste0("Data has been saved to the file <", name_spy_file, "> located in the folder <", folder_for_save_check_data, ">"))
}
