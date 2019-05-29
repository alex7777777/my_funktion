# ######################################################################## #
# 2019-05-23 - by Alex Gorbach                                             #
# Clean strings for df                                                     # 
# ######################################################################## #

my_clean_strings <- function(df_before_clean, search_symb_char, insert_symb_char, exact=T) {
  
  names_before_clean <- names(df_before_clean)
  
  if(exact) {
    # Exact search
    df_before_clean <- data.frame(lapply(df_before_clean, function(x) ifelse(x==search_symb_char, insert_symb_char, x)))
  } else {
    # Substring search
    df_before_clean <- data.frame(lapply(df_before_clean, function(x) gsub(search_symb_char, insert_symb_char, x)))
  }
  
  df_after_clean <- data.frame(sapply(df_before_clean, function(x) as.character(x)), stringsAsFactors=F)
  names(df_after_clean) <- names_before_clean
  
  # df_after_clean <- data.frame(lapply(df_before_clean, function(x) as.character(x)))
  # df_before_clean <- data.frame(lapply(df_before_clean, function(x) ifelse(grepl(search_symb_char, x), insert_symb_char, gsub(search_symb_char, insert_symb_char, x))))
  # for (i in 1:ncol(activities_ref)){ activities_ref[,i] <- gsub('"', '', activities_ref[,i]) }  

  return(df_after_clean)
}
