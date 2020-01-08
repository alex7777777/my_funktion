#######################################################
# 65_function.R
# 2019-12-17 - by Alex Gorbach
#######################################################

# Function: visual check images generator
# for time series data
# real (points) vs. predicted (line)

# DF-Structur:
# V1. Date
# V2. VarOrig_1
# V3. VarOrig_2
# V4. VarOrig_3
# ...
# Vm. VarOrig_N
# Vm+1. VarPred_1
# Vm+2. VarPred_2
# ...
# Vm+m. VarPred_m

set.seed(1)

# N - ANZAHL ZUFFAELLIGER ZEITFENSTER (i<-1)
# D - ANZAHL AUF ABBILDUNG GEZEIGTEN TAGEN
# M - ANZAHL PAAR-VARIABLEN ZUM VERGLEICH (j<-1)
# t - LAUFENDE BILDER-NUMMER FUER SPEICHERN
# my_color <- "red" # FARBE FUER LINIEN

my_images_generator <- function(my_df, N=6, D=14, M=15, my_color="red") {
  
  # GENERIEREN VON ZUFFAELLIGEN ZETRAEUMEN
  time_start <- sort(floor(runif(N, min=1, max=nrow(my_df)-D*24)))
  my_images <- list()
  get_all_my_function("04_function.R")
  t <- 1
  
  for(j in 1:M) {
    for(i in 1:N) {
      
      twindow <- time_start[i]:(time_start[i]+D*24)
      
      my_images[[i+j-1]] <- {
        plot(my_df[twindow,c(1,(1+j))], 
             # type='l', col='blue', 
             pch=20,
             # lwd=1, 
             main = names(my_df)[1+j], # pred.var-nr
             sub = paste("Time Window: from", 
                         min(my_df[twindow,1]), "to",
                         max(my_df[twindow,1])),
             xlab = "",
             ylab = paste0("Real (points) vs. Predictiv values (", my_color, ")"),
             ylim=c(0,round(max(my_df[,1+j], na.rm=T))) )
        
        par(new=T)
        
        # vs. 15+2
        plot(my_df[twindow,c(1,15+1+j)], 
             type='l', lwd=1, lty=5, col=my_color, main="", xlab="", ylab="", axes=FALSE) # lwd=2 (Linie-Breite)
      }
      my_save_img_function(my_images[[i+j-1]], 
                           paste0(names(my_df)[1+j], 
                                  # "_from_", min(my_df[twindow,1]),
                                  # "_to_", max(my_df[twindow,1]),
                                  "_", t))
      t <- t+1
      
      dev.off()
    }
  }
  
  for(i in 1:N) {
    twindow <- time_start[i]:(time_start[i]+D*24)
    cat(paste0("Random current hour: ", time_start[i], "-", time_start[i]+D*24, "\n"))
    cat(paste0("TIME WINDOW ", i, ": from ", min(my_df[twindow,1]), 
               " to ", max(my_df[twindow,1]), "\n\n"))
  }
  
  return(cat(paste0(i*j, " pictures were generated\n")))
}

# my_images_generator(resultat_pred, 6, 7, 15, "darkgoldenrod")
