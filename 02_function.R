#######################################################
# 2018-01-29 - by Alex Gorbach
# Datenbasis: DB Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: distribution function

library(Hmisc)

my_distrib <- function(var_distrib) {
  
  if(class(var_distrib) != "integer" | class(var_distrib) != "numeric") 
    {
   
    var_distrib <- as.character(var_distrib)
    
    var_distrib <- ifelse(var_distrib == "" | var_distrib == " " , NA, var_distrib)
    verteilung <- data.frame(table(var_distrib))
    names(verteilung) <- c("label", "frequency")
    Total <- data.frame(label=c("Missings", "Total"),
                        frequency=c(sum(is.na(var_distrib)),
                        sum(verteilung$frequency)+sum(is.na(var_distrib)
                                                      )
                        )
    )
    verteilung <- verteilung[order(-verteilung$frequency), ]
    verteilung <- rbind(verteilung, Total)
    # verteilung$cum_sum_prct <- paste0(round(100*cumsum(verteilung$frequency)/verteilung[nrow(verteilung),"frequency"],1), "%")
    verteilung$cum_sum_prct <- cumsum(verteilung$frequency)/verteilung[nrow(verteilung),"frequency"]
    rownames(verteilung) <- NULL
    verteilung[nrow(verteilung),]$cum_sum_prct <- NA
    } 
  
  else 
    {
      describe(verteilung)
    }
  
  return(verteilung)
}
