#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: automatical generate Time Classes

my_time_classes_generator <- function(time_difference_variable, 
                                      NumberDeltaTimeClasses=10) {
  
  time_df <- data.frame(delta_t = round(quantile(time_difference_variable,  probs = c(1:100)/100, na.rm = TRUE),1),
                        Quantile_DayDiff = rownames(data.frame(quantile(time_difference_variable,  probs = c(1:100)/100, na.rm = TRUE)))
  )
  rownames(time_df) <- NULL
  
  # Initialize of Time Classes
  TimeClassParam <- list()
  for(i in 1:NumberDeltaTimeClasses) {
    time_df$tmp <- 0
    for(j in 1:i) {
      time_df[as.integer(rownames(time_df) ) > j*nrow(time_df)/(i+1) , ]$tmp <- j #
    }
    colnames(time_df)[ncol(time_df)] <- paste0('delta_t_class', i)
    
    TimeClassParam[[ paste0("delta_t_class_", i) ]] <- NA
    for(m in 1:length(unique(time_df[ , ncol(time_df)]))) {
      TimeClassParam[[ paste0("delta_t_class_", i) ]] <- c(TimeClassParam[[ paste0("delta_t_class_", i) ]] , max(time_df[time_df[ , ncol(time_df)] == m-1 , ]$delta_t))
    }
    TimeClassParam[[ paste0("delta_t_class_", i) ]] <- TimeClassParam[[ paste0("delta_t_class_", i) ]][-c(1, length(TimeClassParam[[ paste0("delta_t_class_", i) ]]))]
  }
  
  TimeClassParam$QuantileDayDiffSheet <- time_df
  
  time_df$Quantile_DayDiff <- NULL
  time_df <- time_df[!duplicated(time_df), ]
  nrow(time_df)
  rownames(time_df) <- NULL
  
  delta_t_uniq <- unique(time_df$delta_t)
  delta_t_uniq_t <- 1:max(delta_t_uniq)
  delta_t_uniq_add <- delta_t_uniq_t[-delta_t_uniq]
  
  time_df2 <- time_df[0,]
  for (i in 1:length(delta_t_uniq_add))
  {
    time_df2[i, ] <- NA
  }
  
  time_df2$delta_t <- delta_t_uniq_add
  head(time_df2)
  
  time_df <- rbind(time_df, time_df2)
  time_df <- time_df[order(time_df$delta_t), ]
  
  for(j in 1:NumberDeltaTimeClasses) {
    t <- time_df[,c(1,j+1)]
    for(i in 1:nrow(t)) {
      if(is.na(t[i,2])) { t[i,2] <- t[i-1,2] }
    }
    time_df[,j+1] <- t[,2]
  }
  
  for(i in 1:(NumberDeltaTimeClasses+1)) {
    print(paste0("CHECK: Numb. of missings, var. ", i, " is ", nrow(time_df[is.na(time_df[,1]),])))
  }
  
  time_df[1:60,c(1, (ncol(time_df)-4):ncol(time_df))]
  time_df[duplicated(time_df[ , 1:4]), ]
  time_df <- time_df[!duplicated(time_df), ]
  
  # save time classes for the xy-join
  time_class_list <- list()
  tc_names <- paste0("tc", 1:NumberDeltaTimeClasses)
  
  for(i in 1:NumberDeltaTimeClasses) {
    time_class_list[[tc_names[i]]] <- unique(data.frame(delta_t=time_df$delta_t,
                                                        delta_t_class=time_df[,i+1]))
    time_class_list[[i]] <- time_class_list[[i]][!duplicated(time_class_list[[i]]$delta_t),]
  }
  
  my_list_output <- list()
  my_list_output$time_class_list <- time_class_list
  my_list_output$TimeClassParam <- TimeClassParam
    
  return(my_list_output)
}
