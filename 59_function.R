#######################################################
# 2019-11-06 - by Alex Gorbach
#######################################################

# Function: returning a vector with common elements

my_common_id_seacher <- function(v1, v2) {
  
  v1 <- unique(v1)
  v2 <- unique(v2)
  v3 <- unique(v1[v1 %in% v2])
  
  cat(paste0("Select ", length(v1), " unique elements from the vector 1 and\n",
             length(v2), " unique elements from the vector 2 and return ", length(v3),
             " common elements as vector 3.\n"))
  
  return(v3)
}
