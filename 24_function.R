#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: Save output of the logistic regression

my_output_glm <- function(fit_glm, acc=0, r_squared=0) {
  
  glm_output <- data.frame(coef(summary(fit_glm)))
  glm_output$Mod_Predictors <- rownames(glm_output)
  rownames(glm_output) <- NULL
  names(glm_output) <- c("Estimate", "Std.Error", "z_value", "Pr(>|z|)", "Mod_Predictors")
  glm_output <- glm_output[ , c(5,1:4)]
  
  # p>=0.05 n.s., p<0.05 *, p<0.01 **, p<0.001 ***
  glm_output$Mod_Sign <- ifelse((glm_output$`Pr(>|z|)` <= 0.1) & (glm_output$`Pr(>|z|)` > 0.05), ".",
                                ifelse((glm_output$`Pr(>|z|)` <= 0.05) & (glm_output$`Pr(>|z|)` > 0.01), "*",
                                       ifelse((glm_output$`Pr(>|z|)` <= 0.01) & (glm_output$`Pr(>|z|)` > 0.001), "**",
                                              ifelse(glm_output$`Pr(>|z|)` <= 0.001, "***",
                                                     glm_output$Mod_Sign <- "n.s."))))
  glm_output_tmp <- glm_output[-1,]
  glm_output <- rbind(glm_output[1,], glm_output_tmp[order(glm_output_tmp$`Pr(>|z|)`) , ])
  
  glm_output_add_line <- glm_output[0, ]
  glm_output_add_line[1,1] <- "------------------------"
  #glm_output_add_line[1,2:6] <- ""
  glm_output_add_line[2,]$Mod_Predictors <- "Accuracy: "
  glm_output_add_line[2,]$Estimate <- Acc
  #glm_output_add_line[2,3:6] <- ""
  glm_output_add_line[3,]$Mod_Predictors <- "Cragg and Uhler`s pseudo r-squared: "
  glm_output_add_line[3,]$Estimate <- pR2_r2CU
  #glm_output_add_line[3,3:6] <- ""
    
  glm_output <- rbind(glm_output, glm_output_add_line)
  
  glm_output$Mod_Predictors <- gsub("`", "", glm_output$Mod_Predictors)
  
  rownames(glm_output) <- NULL
    
  return(glm_output)
}
