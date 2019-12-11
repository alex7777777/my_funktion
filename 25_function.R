#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Cross validation

my_cross_valid <- function(dep_var_char, my_df, ind_var_list, foldsNumber=10) {
  x <- my_df[,ind_var_list]
  y <- as.factor(my_df[,dep_var_char])
  xy <- data.frame(y, x)
  split <- runif(dim(xy)[1]) > 0.2
  train <- xy[split,]
  test  <- xy[!split,]

  folds <- cut(seq(1,nrow(xy)),breaks=foldsNumber,labels=FALSE)
  
  fit.glm.save <- list()
  
  # Perform XY fold cross validation
  for(i in 1:foldsNumber){
    # Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test <- xy[testIndexes, ]
    train <- xy[-testIndexes, ]
    
    # Test and train data partitions
    # Model building
    fit.glm <- glm(train$y ~ ., family=binomial, data=train[,-1])
    p.glm <- ifelse(predict(fit.glm, train[,-1], type='response')> 0.5, 1,0)
    AccTrain <- sum(train$y == p.glm, na.rm = T)/(nrow(train) - sum(is.na(p.glm)))
    
    # cross-validation
    p.glm2 <- ifelse(predict(fit.glm, test[,-1], type='response')> 0.5, 1,0)
    AccTest <- sum(test$y == p.glm2, na.rm = T)/(nrow(test) - sum(is.na(p.glm2)))
    
    # Save
    fit.glm.save[[paste0(foldsNumber, "-folds cross validation, model-", i, ", train data. Accuracy: ")]] <- AccTrain
    fit.glm.save[[paste0(foldsNumber, "-folds cross validation, model-", i, ", test data.  Accuracy: ")]] <- AccTest
  }
  
  # Output cross validation
  for(i in 1:length(fit.glm.save)) {
    add_line <- paste0(names(fit.glm.save[i]), round(100*fit.glm.save[[i]], 2), "%")
    print(add_line)
  }
}
