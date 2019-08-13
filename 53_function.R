# ######################################################################## #
# 2019-08-13 - by Alex Gorbach                                             #
# SQL Sets for Big Query                                                   #
# ######################################################################## #

# Generate SQL-Strings for Big Query survey

my_sql_generator <- function(sql_typ_select, 
                             date_fr,   # YYYY-MM-DD
                             days_to=7, 
                             days_fr_past=15) { 
  
  date_to <- as.character(as.Date(date_fr) + days_to)
  date_fr_past <- as.character(as.Date(date_fr) - days_fr_past)
    
  sql_return <- switch(sql_typ_select,
                       
                       "orders_sql" =          paste0("SELECT
                                                      CUSTOMER_UUID,
                                                      REFERENCE,
                                                      ID,
                                                      JSON_EXTRACT(payload,'$.order_value') as order_value_cent,
                                                      received_at order_created_received_at,
                                                      date(received_at) as order_created_date,
                                                      JSON_EXTRACT(payload,'$.market_id') as MARKET_ID,
                                                      JSON_EXTRACT(payload,'$.invoice_address.customer_type') as CUSTOMER_TYPE_1,
                                                      JSON_EXTRACT(payload,'$.sub_orders[0].delivery_type') as DELIVERY_TYPE_1,
                                                      JSON_EXTRACT(payload,'$.sub_orders[1].delivery_type') as DELIVERY_TYPE_2,
                                                      JSON_EXTRACT(payload,'$.sub_orders[0].merchant_type') as MERCHANT_TYPE_1,
                                                      JSON_EXTRACT(payload,'$.sub_orders[1].merchant_type') as MERCHANT_TYPE_2,
                                                      JSON_EXTRACT(payload,'$.id') as order_id,
                                                      JSON_EXTRACT(payload,'$.client_info') as web_app
                                                      
                                                      FROM `rd-bigdata-prd-v002.analytics.customer_activity`
                                                      
                                                      WHERE _PARTITIONTIME  >= '", as.character(as.Date(date_fr) - 15), " 00:00:00'
                                                      AND _PARTITIONTIME    <  '", as.character(as.Date(date_to)   + 15), " 00:00:00'
                                                      AND received_at       >= '", as.character(as.Date(date_fr)), " 00:00:00'
                                                      AND received_at       <  '", as.character(as.Date(date_to)), " 00:00:00'
                                                      AND activity = 'order_created'
                                                      ;"),
                       
	  # 2. SQL: 
    # a) Identifizieren von "customer_uuid" nur fuer Kaeufer ['order_created'] in der Zeit [-11 days; -4 days] vom akt. Datum
    # b) und nur mit einer identifizierbaren CoockiesID [not reference = '']
    # c) aggregiert (group by) "reference" und "customer_uuid"
    # d) Join ueber "reference" von Kaeufern a)-c) mit CJ von sich selbst mit vorangegangenen Activities [-70 days; -4 days], 
    #    auch wenn sie damals nichts gekauft haben [zzgl. 'shop_visited','campaign_hit']
	
                       "activities_ref_sql" =  paste0("#standardSQL
                                                      SELECT
                                                      reference
                                                      ,customer_uuid
                                                      ,activity
                                                      ,received_at
                                                      ,IFNULL(marketing_channel,'dir') as marketing_channel
                                                      ,m_channel_referer
                                                      ,order_id
                                                      
                                                      from
                                                      
                                                      (
                                                      SELECT 
                                                      a.reference
                                                      ,b.customer_uuid
                                                      ,activity
                                                      ,received_at
                                                      ,JSON_EXTRACT(payload,'$.m_channel_referer') as m_channel_referer
                                                      ,SUBSTR(REPLACE(JSON_EXTRACT(payload,'$.marketing_channel'),'\"',''),1,3) as marketing_channel
                                                      ,JSON_EXTRACT(payload,'$.id') as order_id
                                                      
                                                      FROM `rd-bigdata-prd-v002.analytics.customer_activity` as a
                                                      
                                                      inner join 
                                                      (
                                                      select
                                                      reference,
                                                      customer_uuid
                                                      
                                                      from `rd-bigdata-prd-v002.analytics.customer_activity`
                                                      
                                                      where customer_uuid in
                                                      (
                                                      SELECT 
                                                      customer_uuid
                                                      
                                                      FROM `rd-bigdata-prd-v002.analytics.customer_activity`
                                                      WHERE _PARTITIONTIME >= '", as.character(as.Date(date_fr) - 15), " 00:00:00'
                                                      AND _PARTITIONTIME   <  '", as.character(as.Date(date_to) + 15), " 00:00:00'
                                                      AND received_at      >= '", as.character(as.Date(date_fr)), " 00:00:00'
                                                      AND received_at      <  '", as.character(as.Date(date_to)), " 00:00:00'
                                                      AND activity = 'order_created'
                                                      )
                                                      and not (reference = '')
                                                      group by 1,2
                                                      ) as b on (a.reference = b.reference)
                                                      
                                                      WHERE _PARTITIONTIME >= '", as.character(as.Date(date_fr) - 15), " 00:00:00'
                                                      AND _PARTITIONTIME   < '",  as.character(as.Date(date_to) + 15), " 00:00:00'
                                                      AND received_at      >= '", as.character(as.Date(date_fr_past)), " 00:00:00'
                                                      AND received_at      < '",  as.character(as.Date(date_to)), " 00:00:00'
                                                      AND activity in ('shop_visited','campaign_hit','order_created')
                                                      )
                                                      "),
                       
                       "activities_uuid_sql" = paste0("#standardSQL
                                                      SELECT
                                                      reference
                                                      ,customer_uuid
                                                      ,activity
                                                      ,received_at
                                                      ,IFNULL(marketing_channel,'dir') as marketing_channel
                                                      ,m_channel_referer
                                                      ,order_id
                                                      
                                                      from
                                                      
                                                      (
                                                      SELECT 
                                                      a.reference
                                                      ,b.customer_uuid
                                                      ,activity
                                                      ,received_at
                                                      ,JSON_EXTRACT(payload,'$.m_channel_referer') as m_channel_referer
                                                      ,SUBSTR(REPLACE(JSON_EXTRACT(payload,'$.marketing_channel'),'\"',''),1,3) as marketing_channel
                                                      ,JSON_EXTRACT(payload,'$.id') as order_id
                                                      
                                                      FROM `rd-bigdata-prd-v002.analytics.customer_activity` as a
                                                      
                                                      inner join 
                                                      
                                                      (
                                                      SELECT 
                                                      customer_uuid
                                                      
                                                      FROM `rd-bigdata-prd-v002.analytics.customer_activity`
                                                      WHERE _PARTITIONTIME >= '", as.character(as.Date(date_fr) - 15), " 00:00:00'
                                                      AND _PARTITIONTIME   <  '", as.character(as.Date(date_to) + 15), " 00:00:00'
                                                      AND received_at      >= '", as.character(as.Date(date_fr)), " 00:00:00'
                                                      AND received_at      <= '", as.character(as.Date(date_to)), " 00:00:00'
                                                      AND activity = 'order_created' 
                                                      group by 1
                                                      ) as b on (a.customer_uuid = b.customer_uuid)
                                                      
                                                      WHERE _PARTITIONTIME >= '", as.character(as.Date(date_fr) - 15), " 00:00:00'
                                                      AND _PARTITIONTIME   <  '", as.character(as.Date(date_to) + 15), " 00:00:00'
                                                      AND received_at      >= '", as.character(as.Date(date_fr_past)), " 00:00:00'
                                                      AND received_at      <  '", as.character(as.Date(date_to)), " 00:00:00'
                                                      AND activity in ('shop_visited','campaign_hit','order_created')
                                                      AND reference = ''
                                                      )
                                                      "),
                       
	  # 4. SQL: 
    # a) Identifizieren von "customer_uuid" nur fuer die Kaeufer ['order_created'] in der Zeit [-11 days; -4 days] vom akt. Datum,
    #    gruppiert ueber "customer_uuid", "received_at" und "order_id"
    # b) Join ueber "customer_uuid" der Kaeufer mit CJ von sich selbst mit vorangegangenen Kaeufern
    # c) Liefern von Zusatzspalten falls vorhanden: Vorletzter Kauf "order_id_before" mit dem Datum "received_at_before"
    # um nur dijenige Aktivitaeten zu betrachten, die in der Zeit zwischen den Kaeufern vergangen sind
	
                       "orders_all_sql" =      paste0("#standardSQL
                                                      SELECT
                                                      customer_uuid
                                                      ,order_id
                                                      ,received_at
                                                      ,date(received_at) as received_at_date
                                                      ,order_id_before
                                                      ,received_at_before
                                                      
                                                      from
                                                      
                                                      (
                                                      SELECT 
                                                      b.customer_uuid
                                                      ,b.received_at
                                                      ,order_id
                                                      ,a.received_at as received_at_before
                                                      ,JSON_EXTRACT(payload,'$.id') as order_id_before
                                                      
                                                      FROM `rd-bigdata-prd-v002.analytics.customer_activity` as a
                                                      
                                                      inner join 
                                                      
                                                      (
                                                      SELECT 
                                                      customer_uuid,
                                                      received_at,
                                                      JSON_EXTRACT(payload,'$.id') as order_id
                                                      
                                                      FROM `rd-bigdata-prd-v002.analytics.customer_activity`
                                                      WHERE _PARTITIONTIME >= '", as.character(as.Date(date_fr) - 15), " 00:00:00'
                                                      AND _PARTITIONTIME   <  '", as.character(as.Date(date_to) + 15), " 00:00:00'
                                                      AND received_at      >= '", as.character(as.Date(date_fr)), " 00:00:00'
                                                      AND received_at      <  '", as.character(as.Date(date_to)), " 00:00:00'
                                                      AND activity = 'order_created' 
                                                      group by 1,2,3
                                                      ) as b on (a.customer_uuid = b.customer_uuid and a.received_at <= b.received_at)
                                                      
                                                      WHERE activity in ('order_created')
                                                      )
                                                      ")
                       )
  
  return(sql_return)
  
}
