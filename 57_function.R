# ######################################################################## #
# 2019-08-27 - by Alex Gorbach                                             #
# Passwords generator                                                       #
# ######################################################################## #

my_password_generator <- function(pass_lenght=12, 
                                  n_yrs=5, 
                                  week_or_month="w", 
                                  spec_char=c("$", "§", "&", "!"))
{
  n_days  <- 366
  n_weeks <- 52
  n_month <- 12
  
  set.seed(as.integer(Sys.Date()+sample(1:n_days)[sample(1:n_weeks)[n_yrs]]))
  
  if(n_yrs<2) { n_years <- 2 } else { n_years <- n_yrs }
  
  # initialize vector & data frame
  my_days <- c(1:n_days)
  my_kw <- c(1:n_weeks)
  my_month <- c(1:n_month)
  
  if(week_or_month=="w") {
    my_pass_df <- data.frame(matrix(1:(n_weeks*n_years), n_weeks, n_years))
  } else if(week_or_month=="m") { my_pass_df <- data.frame(matrix(1:(n_month*n_years), n_month, n_years)) }
  else { my_pass_df <- data.frame(matrix(1:(n_days*n_years), n_days, n_years)) }
  
  years_name <- c()
  years_name[1] <- paste0("year_", as.character(year(Sys.Date())))
  for(i in 1:(n_years-1)) { years_name[i+1] <- c(paste("year_", year(Sys.Date())+i, sep = ""))  }
  names(my_pass_df) <- years_name
  
  if(week_or_month=="w") {
    for(j in 1:n_years) {
      for (i in 1:n_weeks)
      {
        my_pass_df[i,j] <- paste(sample(c(0:9, letters, LETTERS, spec_char), pass_lenght, replace=TRUE), collapse="")
        my_kw[i] <- paste("KW",i)
      }
    }
    
    my_df_return <- cbind(data.frame(my_kw), my_pass_df)
    
  } else if(week_or_month=="m") {
    for(j in 1:n_years) {
      for (i in 1:n_month)
      {
        my_pass_df[i,j] <- paste(sample(c(0:9, letters, LETTERS, spec_char), pass_lenght, replace=TRUE), collapse="")
        my_month[i] <- paste0(i, ". Month")
      }
    }
    my_df_return <- cbind(data.frame(my_month), my_pass_df)
  } else {
    for(j in 1:n_years) {
      for (i in 1:n_days)
      {
        my_pass_df[i,j] <- paste(sample(c(0:9, letters, LETTERS, spec_char), pass_lenght, replace=TRUE), collapse="")
        my_days[i] <- paste0(i, ". day")
      }
    }
    my_df_return <- cbind(data.frame(my_days), my_pass_df)
  }
  
  if(n_yrs<2) { my_df_return <- my_df_return[ , 1:2] }
  
  return(my_df_return)
}
