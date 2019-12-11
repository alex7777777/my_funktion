# ######################################################################## #
# 2019-06-04 - by Alex Gorbach                                             #
# Storage of all active R-objects                                          # 
# ######################################################################## #

# Save from active R-Objects

allactiveobjects <- ls()
 
my_save_activ_robjects <- function(save_selected_object=allactiveobjects, 
                                   date_from=NA, date_to=NA) 
{

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
    DATA[[i]] <- NA
    # Rename
    names(DATA)[i] <- save_selected_object[i]
    # fill data
    DATA[[i]] <- get(save_selected_object[i])
    cat(paste0(i, ": The Object class ", class(DATA[[i]]), " <", names(DATA[i]), "> will be saved.\n"))
  }
  
  # Save und rename file
  save(DATA, file = "r_objects/DATA.RData")
  
  if(is.na(date_from)|is.na(date_to)) {
    save_name <- paste0("r_objects/", substr(as.character(Sys.time()), 1, 10),
                        "_DATA.RData")
  } else {
    save_name <- paste0("r_objects/", substr(as.character(Sys.time()), 1, 10),
                        "_DATA",
                        "_from_", date_from,
                        "_to_", date_to,
                        ".RData")
  }
  
  file.rename(from = "r_objects/DATA.RData",
              to = save_name)
  result_message <- paste0("\nAll active R-objects have been saved in the file <", save_name, ">")
  
  rm(DATA)
  
  cat(result_message)
}
