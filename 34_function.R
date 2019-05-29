#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Modeling quality assessment
# http://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf
# True Negative Rate (Acc−) = TN/(TN+FP)
# True Positive Rate (Acc+) = TP/(TP+FN)
# G-mean = (Acc− * Acc+)^1/2
# Weighted Accuracy = BETA * Acc+ + (1−BETA)Acc−
# Precision = TP/(TP+FP)
# Recall = TP/(TP+FN) = Acc+
# F-measure = 2 * Precision * Recall/(Precision+Recall)

my_modeling_quality_assessment <- function(modeling_name, 
                                           real_vector, 
                                           predictive_vector) {
  
  if(class(real_vector)=="factor") {
    real_vector <- as.integer(real_vector)-1
    print("Convert class <factor> in class <integer>")
  }
  
  TP <- table(real_vector, predictive_vector )[1,1]
  FP <- table(real_vector, predictive_vector )[1,2]
  FN <- table(real_vector, predictive_vector )[2,1]
  TN <- table(real_vector, predictive_vector )[2,2]
  
  tmp <- max(nchar(TP), nchar(FP), nchar(FN), nchar(TN))
  my_space <- function(i) {
    a <- tmp - nchar(i)
    b <- paste0(rep(" ", a), collapse = "")
    return(b)
  }
  
  cat("Confusion matrix:", "\n",
                   "--------------------------------------------------------\n",
                   "True Positive (TP)  = ", my_space(TP), TP, "; False Negative (FN) = ", my_space(FN), FN, "\n",
                   "False Positive (FP) = ", my_space(FP), FP, "; True Negative (TN)  = ", my_space(TN), TN, "\n",
                   "--------------------------------------------------------\n",
                   "The performance metrics:\n",
                   "True Negative Rate (Acc-) = TN/(TN+FP):            ", round(100*TN/(TN+FP),2), "%\n",
                   "True Positive Rate (Acc+) = TP/(TP+FN):            ", round(100*TP/(TP+FN),2), "%\n",
                   "G-mean                    = (Acc- * Acc+)^1/2:     ", round(100*((TN/(TN+FP))*(TP/(TP+FN)))^1/2,2), "%\n",
                   "Precision                 = TP/(TP+FP):            ", round(100*TP/(TP+FP),2), "%\n",
                   "Recall             (Acc+) = TP/(TP+FN):            ", round(100*TP/(TP+FN),2), "%\n",
                   "F-measure = 2*Precision*Recall/(Precision+Recall): ", round(100*(2*TP/(TP+FP)*TP/(TP+FN))/(TP/(TP+FP)+TP/(TP+FN)),2), "%\n")
  
  list_performance_metrics <- c("True Negative Rate (Acc-) = TN/(TN+FP)",
                                "True Positive Rate (Acc+) = Recall = TP/(TP+FN)",
                                "G-mean = (Acc- * Acc+)^1/2",
                                "Precision = TP/(TP+FP)",
                                "F-measure = 2*Precision*Recall/(Precision+Recall)")
  
  list_matrics_value <- c(TN/(TN+FP), 
                          TP/(TP+FN), 
                          ((TN/(TN+FP))*(TP/(TP+FN)))^1/2, 
                          TP/(TP+FP), 
                          (2*TP/(TP+FP)*TP/(TP+FN))/(TP/(TP+FP)+TP/(TP+FN)))
  
  modeling_quality <- data.frame(time_stamp_modeling = rep(as.character(Sys.time()), 5),
                                 TP = rep(TP, 5),
                                 FN = rep(FN, 5),
                                 FP = rep(FP, 5),
                                 TN = rep(TN, 5),
                                 modeling_names = rep(modeling_name, 5),
                                 performance_metrics = list_performance_metrics,
                                 value_metrics = list_matrics_value)
  
  return(modeling_quality)
}
