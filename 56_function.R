# ######################################################################## #
# 2019-08-14 - by Alex Gorbach                                             #
# Segmenting                                                               #
# ######################################################################## #

# Segment selection according to specified parameters

my_segment_select <- function(my_df, 
                              f_orders=T,
                              f_delivery='LS',
                              f_merchant='',
                              f_webapp='') {
  # a) orders
  if(f_orders) {
    my_df$order_value_cent <- as.integer(my_df$order_value_cent)
    my_df <- subset(my_df, order_value_cent > 0)
  }
  
  # b) service
  if(f_delivery=='LS') {
    
    # only lieferservice (DELIVERY) without mix cusomer (Marktplatz):
    # my_df <- subset(my_df, DELIVERY_TYPE_1 == '"DELIVERY"')
    
    # only lieferservice (DELIVERY) including mix cusomer (Marktplatz):
    my_df <- subset(my_df, (DELIVERY_TYPE_1 == '"DELIVERY"')
                     | (DELIVERY_TYPE_2 == '"DELIVERY"'))
  } else {
    # not 'LS'
    my_df <- subset(my_df, (DELIVERY_TYPE_1 != '"DELIVERY"')
                     & (DELIVERY_TYPE_2 != '"DELIVERY"'))
  }
  
  # c) merchant
  if(f_merchant == 'REWE') {
    # only REWE zzgl. mix cusomer (MERCHANT_TYPE_2 == '"REWE"'):
    my_df <- subset(my_df, (MERCHANT_TYPE_1 == '"REWE"')
                     | (MERCHANT_TYPE_2 == '"REWE"'))
  } else if(f_merchant == 'PARTNER') {
    # only REWE zzgl. mix cusomer (MERCHANT_TYPE_2 == '"PARTNER"'):
    my_df <- subset(my_df, (MERCHANT_TYPE_1 == '"PARTNER"')
                     | (MERCHANT_TYPE_2 == '"PARTNER"'))
  }
  
  # table(my_df$MERCHANT_TYPE_1)
  # table(my_df$MERCHANT_TYPE_2)
  
  # d) APP/WEB
  if(f_webapp == 'WEB') {
    my_df <- my_df[grepl('Web', my_df$web_vs_app) , ]
  } else if(f_webapp == 'APP') {
    my_df <- my_df[grepl('App', my_df$web_vs_app) , ]
  }
  
  return(my_df)
}
