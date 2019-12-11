
# ############################################################## #
# Equal, max, last, first attribution / distribution computation #
# ############################################################## #

library(dplyr)

# attribution_calc <- function(att_004, label_for_att = "No label")
attribution_calc <- function(att_004)
  {
  att_005 <- att_004 %>%
    group_by(order_id) %>% mutate(sequence = row_number())
  att_006 <- att_005 %>%
    arrange(order_id, desc(received_at)) %>%
    group_by(order_id) %>% mutate(sequence_last = row_number()) %>%
    arrange(order_id, received_at)
  lasttouchverteilung <- att_006 %>%
    filter(sequence_last == 1) %>%
    group_by(marketing_channel) %>%
    summarise(lasttouch=n())

  # add
  firsttouchverteilung <- att_006 %>%
    filter(sequence == 1) %>%
    group_by(marketing_channel) %>%
    summarise(firsttouch=n())

  gleichverteilung <- group_by(att_004,marketing_channel) %>% 
    summarise(gleichverteilung=sum(key))
  gleichverteilung$gleichverteilung <- round(gleichverteilung$gleichverteilung,digits=0)

  maximalattribution <- group_by(att_004,marketing_channel) %>% summarise(maximalattribution=n())

  attribution_final <- left_join(gleichverteilung,maximalattribution,by="marketing_channel") %>%
    left_join(.,lasttouchverteilung, by="marketing_channel") %>%
    left_join(.,firsttouchverteilung, by="marketing_channel")
  
  attribution_final <- data.frame(attribution_final)
  # attribution_final$label <- label_for_att

  return(attribution_final)
  }
