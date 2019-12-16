#######################################################
# 2019-12-16 - by Alex Gorbach
# Detecting of outliers
#######################################################

my_outliers_detect <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  x1 <- x
  x1[x < (qnt[1] - H)] <- NA
  x1[x > (qnt[2] + H)] <- NA
  oProp <- paste0(round(100*length(x1[is.na(x1)])/length(x), digits = 1), "%")
  return(list(x=x,x1=x1,oProp=oProp))
}

# outliers_df <- data.frame(my_outliers_detect(orig_var)[1],
#                           my_outliers_detect(var_with_outlier)[2])
# print(paste0("Proportion of the outlier: ", my_outliers_detect(orig_var)[3]))
# # OR
# outliers_dv <- data.frame(my_outliers_detect(modeling_df$sales_growth)[1],
#                           my_outliers_detect(modeling_df$sales_growth)[2])
# perc_dv     <- data.frame(my_outliers_detect(modeling_df$sales_growth)[3])
# 
# for(i in 1:ncol(modeling_df)) {
#   perc <- my_outliers_detect(unlist(modeling_df %>% select(i)))[3]
#   cat(paste0("VarN ", i, ". <", names(modeling_df)[i], ">: Proportion of outliers: ", perc, "\n"))
# }
