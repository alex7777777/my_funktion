#######################################################
# 2018-03-07 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: # Balanced sample
# (only for binary dependet variable as dummy)

my_balanced_sample <- function(my_dummy_vector_name, my_df, my_ids_name) {
  uniq_value <- unique(my_df[,my_dummy_vector_name])
  if(length(uniq_value)!=2) {
    print(paste0("My Dependent variable <", my_dummy_vector_name, "> is not binary: task failed"))
  } else {
    t1 <- my_df[my_df[, my_dummy_vector_name]==uniq_value[1],]
    t2 <- my_df[my_df[, my_dummy_vector_name]==uniq_value[2],]
    
    if(nrow(t1) < nrow(t2)) {
      t2 <- t2[t2[ , my_ids_name] %in% sample(t2[ , my_ids_name], nrow(t1)), ] 
      cj_balanced <- rbind(t1, t2)
    } else {
      t1 <- t1[t1[ , my_ids_name] %in% sample(t1[ , my_ids_name], nrow(t2)), ] 
      cj_balanced <- rbind(t1, t2)
    }
    
    # Randomly shuffle the data
    cj_balanced <- cj_balanced[sample(nrow(cj_balanced)),]
    rownames(cj_balanced) <- NULL
  }
  
  return(cj_balanced)
}
