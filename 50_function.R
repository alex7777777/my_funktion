# ######################################################################## #
# 2019-05-23 - by Alex Gorbach                                             #
# Distribution tree of individual parameters in the url-strings            # 
# ######################################################################## #

my_tree_distrib <- function(my_df_for_distr,	percentile_cutoff=1, select_event="", select_level_nr=1) {
  
  if(select_event!="") {
    my_df_for_distr <- my_df_for_distr[my_df_for_distr[ , select_level_nr-1] == select_event, ]
  }
  
  source("my_function/02_function.R")
  dist_result <- my_distrib(my_df_for_distr[,select_level_nr])
  
  dist_result <- dist_result[(dist_result$cum_sum_prct <= percentile_cutoff)&
                               (!is.na(dist_result$cum_sum_prct))&
                               (dist_result$label != "Missings"), ]
  
  return(dist_result)
}
