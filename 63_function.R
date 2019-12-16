# Easy splines for Time Series

# Data structur for DF:
# 'data.frame':
#  $ time_var       : int
#  $ time_series_var: num
# m=100 - Number of points for the image (line)!!!

sp.resampler <- function(sp.frame) {
  n <- nrow(sp.frame)
  resample.rows <- sample(1:n,size=n,replace=TRUE)
  return(sp.frame[resample.rows,])
  }

sp.spline.estimator <- function(data,m=100,spar_par=0.45) {
  fit <- smooth.spline(x=data[,1],y=data[,2],spar=spar_par) #spar=0.85/0.45)
  eval.grid <- seq(from=min(data[,1]),to=max(data[,1]),length.out=m)
  return(predict(fit,x=eval.grid)$y) # We only want the predicted values
}

sp.spline.cis <- function(sp.frame,B,alpha, m=100, spar_par=0.45) {
  spline.main <- sp.spline.estimator(sp.frame,m=m,spar_par)
  spline.boots <- replicate(B,sp.spline.estimator(sp.resampler(sp.frame),m=m))
  cis.lower <- 2*spline.main - apply(spline.boots,1,quantile,probs=1-alpha/2)
  cis.upper <- 2*spline.main - apply(spline.boots,1,quantile,probs=alpha/2)
  return(list(main.curve=spline.main,lower.ci=cis.lower,upper.ci=cis.upper,
              x=seq(from=min(sp.frame[,1]),to=max(sp.frame[,1]),length.out=m)))
}

# TESTING 1:
# set.seed(1)
# n <- 1e3
# test_df <- data.frame(time_var = 1:n,
#                       time_series_var = sin(seq(0, 5*pi, length.out = n)) + 
#                         rnorm(n=n, mean = 0, sd=0.1))
# 
# 
# time_series_spline <- sp.spline.cis(sp.frame=test_df,
#                                     B=1000,
#                                     alpha=0.05, 
#                                     m=nrow(test_df), # 100
#                                     spar_par=0.15)
# plot(test_df$time_var, test_df$time_series_var, xlab="Time", ylab="Time Series")
# 
# # abline(lm(test_df$time_series_var ~ test_df$time_var),col="grey")
# lines(x=time_series_spline$x,y=time_series_spline$main.curve,col="red",lwd=2)
# lines(x=time_series_spline$x,y=time_series_spline$lower.ci,col="blue",lty=2)
# lines(x=time_series_spline$x,y=time_series_spline$upper.ci,col="blue",lty=2) 
# test_df$time_series_var <- time_series_spline$main.curve # attention!!! m = nrow(test_df)!!!

# # TESTING 2:
# # with saved images & different spar parameters 
# set.seed(123)
# n <- 100
# test_df <- data.frame(time_v = 1:n,
#                       umsatz = round(cos(seq(0, 1.4*pi, length.out = n)) +
#                         rnorm(n=n, mean = 30, sd=0.4), digits = 2))
# plot(test_df$time_v, test_df$umsatz, xlab="Zeit", ylab="Umsatz, EUR")
# 
# spar_par <- seq(0.1,1,0.1)
# my_images <- list()
# get_all_my_function("17_function.R")
# test_df <- my_add_new_var(test_df, length(spar_par), "umsatz_glat_", 0)
# 
# get_all_my_function("04_function.R")
# 
# for(i in 1:length(spar_par)) {
#   
#   time_series_spline <- sp.spline.cis(test_df, 1000, 0.05, nrow(test_df),spar_par[i]) # 0.9; 0.5; 0.1
#   
#   my_images[[i]] <- {
#     plot(test_df$time_v, test_df$umsatz, xlab="Zeit", ylab="Umsatz, EUR")
#     lines(x=time_series_spline$x,y=time_series_spline$main.curve,col="red",lwd=2)
#     lines(x=time_series_spline$x,y=time_series_spline$lower.ci,col="blue",lty=2)
#     lines(x=time_series_spline$x,y=time_series_spline$upper.ci,col="blue",lty=2)
#   }
# 
#   test_df[,2+i] <- round(time_series_spline$main.curve, digits = 2)
#   
#   my_save_img_function(my_images[[i]], paste0("smoothing_spar=", spar_par[i]))
#   dev.off()
# }
# 
# View(test_df)

