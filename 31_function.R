#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Saving plot of the best predictors

source("my_function/04_function.R")

my_best_predictors_boruta_save_img <- function(boruta_output, 
                                           name_for_saving="boruta_img",
                                           fold_name="images") {

  r_img_to_png <- plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")
  my_save_img_function(r_img_to_png,
                       name_for_saving,
                       fold_name)
  
  # in vector graphics as pdf
  date_file_saving <- substr(as.character(Sys.time()), 1, 10)
  s_pdf <- paste0(fold_name, "/", date_file_saving, "_" , name_for_saving, ".pdf")
  pdf(s_pdf)
  plot(boruta_output, xlab = "", xaxt = "n")
  lz<-lapply(1:ncol(boruta_output$ImpHistory),function(i)
    boruta_output$ImpHistory[is.finite(boruta_output$ImpHistory[,i])
                             ,i])
  names(lz) <- colnames(boruta_output$ImpHistory)
  Labels <- sort(sapply(lz,median))
  axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta_output$ImpHistory), cex.axis = 0.2)
  dev.off()
  
}
