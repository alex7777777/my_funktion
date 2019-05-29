#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: segmenting and cleaning

my_segmenting_cleaning <- function(my_df) {
  if((nrow(my_df) > nrow(my_df[!is.na(my_df$event),])) | (nrow(my_df[duplicated(my_df), ]) > 0)) {
    print(paste0("The number of lines: ", nrow(my_df)))
    print(paste0("The number of duplicates: ", nrow(my_df[duplicated(my_df), ]) ))
    print(paste0("The number of missings: ", nrow(my_df[is.na(my_df$event),]) ))
    my_df <- my_df[!is.na(my_df$event),]
    my_df <- my_df[!duplicated(my_df), ]
    print(paste0("The number of lines after cleaning: ", nrow(my_df)))
    print(paste0("The number of unique users after cleaning: ", length(unique(my_df$ids)) ))
  } else {
    print("There are no missings & duplicates")
    print(paste0("The number of lines: ", nrow(my_df)))
    print(paste0("The number of unique users: ", length(unique(my_df$ids)) ))
  }
  return(my_df)
}