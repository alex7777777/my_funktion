#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Sampling function
# before start: rownames(my_df) <- NULL

my_sampling_function <- function(row_vector,
                                 ids_vector,
                                 number_sampling=1000) {
  
  row_vector <- as.integer(row_vector)
  
  if(length(unique(ids_vector)) <= number_sampling) {
    print("The number of unique cases is greater than the sample: To do nothing.")
  } else {
    if(length(row_vector) == length(unique(ids_vector))) {
      
      tmp_df <- data.frame(row_vector = row_vector,
                           ids_vector = ids_vector)
      
      print(paste0("Number of lines before sampling: ", length(row_vector)))
      print(paste0("Number of unique ids before sampling: ", length(unique(ids_vector))))
      
      row_vector <- row_vector[row_vector %in% sample(row_vector, number_sampling)]
      tmp_df <- tmp_df[ row_vector, ]
      
      print(paste0("Number of lines after sampling: ", length(row_vector)))
      print(paste0("Number of unique ids after sampling: ", length(unique(tmp_df$ids_vector))))
      
    } else {
      
      print(paste0("Number of lines before sampling: ", length(row_vector)))
      print(paste0("Number of unique ids before sampling: ", length(unique(ids_vector))))
      
      tmp_df <- data.frame(row_vector = row_vector,
                           ids_vector = ids_vector)
      
      ids_vector <- unique(ids_vector)
      ids_vector <- ids_vector[ids_vector %in% sample(ids_vector, number_sampling)]
      
      row_vector <- tmp_df[tmp_df$ids_vector %in% sample(ids_vector, number_sampling), ]$row_vector
      
      print(paste0("Number of lines after sampling: ", length(row_vector)))
      print(paste0("Number of unique ids after sampling: ", length(ids_vector)))
    }
  }
  return(row_vector)
}
