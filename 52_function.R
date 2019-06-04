# ######################################################################## #
# 2019-06-04 - by Alex Gorbach                                             #
# Storage of all active R-objects                                          # 
# ######################################################################## #

# Save from active R-Objects

my_save_activ_robjects <- function(save_selected_object=ls()) 
{
  # list_r_objects = ls()
  
  # if(!file.exists("r_objects/DATA.RData")) {
  #   DATA <- list()
  #   save(DATA, file = "r_objects/DATA.RData")
  #   print("A R-Object <DATA.RData> was created")
  # } else {
  #   load("r_objects/DATA.RData")
  #   print("The R-Object <DATA> was loaded")
  # }
  
  # Load or create DATA
  
  if(exists("DATA")) {
    cat("\nThe R-Object <DATA> was created and activated\n\n")
  } else if (file.exists("r_objects/DATA.RData")) {
    load("r_objects/DATA.RData")
    cat("\nThe R-Object <DATA> was loaded\n\n")
  } else {
    DATA <- list()
    cat("\nA new R-Object <DATA> was created\n\n")
  }
  
  for(i in 1:length(save_selected_object)) {
    DATA$t <- NA
    # Rename
    names(DATA)[i] <- save_selected_object[i]
    # fill data
    DATA[[i]] <- get(save_selected_object[i])
    cat(paste0(i, ": The Object class ", class(DATA[[i]]), " <", names(DATA[i]), "> will be saved.\n"))
  }
  
  # if(length(save_selected_object)==(length(ls())-1)) {
  #   for(i in 1:length(save_selected_object)) {
  #     DATA$t <- NA
  #     # Rename
  #     names(DATA)[i] <- save_selected_object[i]
  #     # fill data
  #     DATA[[i]] <- get(save_selected_object[i])
  #     cat(paste0(i, ": The Object class ", class(DATA[[i]]), " <", names(DATA[i]), "> will be saved.\n"))
  #   }
  # } 
  # else {
  #   for(i in 1:length(save_selected_object)) {
  #     DATA$t <- NA
  #     # Rename
  #     names(DATA)[i] <- save_selected_object[i]
  #     # fill data
  #     DATA[[i]] <- get(save_selected_object[i])
  #     cat(paste0(i, ": The previously selected Object class ", class(DATA[[i]]), " <", names(DATA[i]), "> will be saved.\n"))
  #   }
  # }

  # Testing: Save only BigQuery data 
  # DATA$orders_raw <- orders_raw
  # DATA$activities_ref <- activities_ref
  # DATA$activities_uuid <- activities_uuid
  # DATA$orders_all_time <- orders_all_time
  
  # Save und rename file
  save(DATA, file = "r_objects/DATA.RData")
  save_name <- paste0("r_objects/", substr(as.character(Sys.time()), 1, 10),
                      "_DATA", 
                      #"_from_", min(orders$order_created_date),
                      #"_to_", max(orders$order_created_date), 
                      ".RData")
  file.rename(from = "r_objects/DATA.RData",
              to = save_name)
  result_message <- paste0("\nAll active R-objects have been saved in the file <", save_name, ">")
  
  rm(DATA)
  
  cat(result_message)
}