# ######################################################################## #
# 2019-08-13 - by Alex Gorbach                                             #
# SQL Sets for DWH and Big Query                                           #
# ######################################################################## #

# Generate SQL-Strings for Big Query survey

my_sql_generator <- function(sql_typ_select, 
                             date_fr,         # YYYY-MM-DD
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
                                                      ,IFNULL(marketing_channel_long,'dir') as marketing_channel_long
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
                                                      ,REPLACE(JSON_EXTRACT(payload,'$.marketing_channel'),'\"','') as marketing_channel_long
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
                                                      AND received_at      <  '", as.character(as.Date(date_to)), " 00:00:00'
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
                                                      "),
                       
                       "dwh_check_id_sql" =    paste0("SELECT DISTINCT RO_GLOBAL_ORDER_ID
                                                      FROM REWE_DIGITAL.S_ROL_DIGITAL_KOPF_DM
                                                      WHERE RO_BESTELLSTATUS_ID IN (104,109)
                                                      AND RO_ORDER_KAL_TAG_ID >= '", date_fr ,"'
                                                      AND RO_ORDER_KAL_TAG_ID < '", date_to ,"';
                                                      "),
                       
                       "dwh_testing_id_sql" =  paste0("SELECT TOP 100 *
                                                      FROM REWE_DIGITAL.S_ROL_DIGITAL_KOPF_DM
                                                      WHERE RO_BESTELLSTATUS_ID IN (104,109)
                                                      AND RO_ORDER_KAL_TAG_ID >= '", date_fr ,"'
                                                      AND RO_ORDER_KAL_TAG_ID < '", date_to ,"';
                                                      "),
                       
                       # "dwh_orders_all_count_sql" =  paste0("SELECT RO_GLOBAL_ORDER_ID AS order_id 
                       #                                , ANZAHL_KAEUFE_SEQ AS anzahl_orders
                       #                                , CASE WHEN (ANZAHL_KAEUFE_SEQ = 1) THEN 'Neukunde' ELSE 'Bestandskunde' END AS kundenstatus
                       #                                FROM REWE_DIGITAL.S_ROL_DIGITAL_KOPF_DM
                       #                                WHERE RO_BESTELLSTATUS_ID IN (104,109)
                       #                                AND RO_ORDER_KAL_TAG_ID >= '", date_fr ,"'
                       #                                AND RO_ORDER_KAL_TAG_ID < '", date_to ,"';
                       #                                "),
                       
                       "dwh_orders_all_count_sql" =  paste0("SELECT RO_GLOBAL_ORDER_ID AS order_id 
                                                            , ANZAHL_KAEUFE_SEQ
                                                            , ANZAHL_KAEUFE_SEQ_LS
                                                            , ANZAHL_KAEUFE_SEQ_AS
                                                            -- , CASE WHEN (ANZAHL_KAEUFE_SEQ_LS = 1) THEN 'Neukunde' WHEN (ANZAHL_KAEUFE_SEQ_LS IS NULL) THEN NULL ELSE 'Bestandskunde' END AS kundenstatus
                                                            , RO_CHANNEL
                                                            , RO_RFM_SEGMENT_ID
                                                            , RO_CUSTOMERTYPE
                                                            , a.RO_SERVICE_ID
                                                            , Region
                                                            , Standort
                                                            FROM  REWE_DIGITAL.S_ROL_DIGITAL_KOPF_DM AS a INNER JOIN REWE_DIGITAL.V_D_RO_USERDATA AS b ON  a.RO_USER_ID = b.RO_USER_ID
                                                            INNER JOIN REWE_DIGITAL.LU_D_MA AS c ON a.MA_ID = c.MA_ID
                                                            LEFT JOIN REWE_DIGITAL.ocma_kufo_plz_region_zuordnung zz ON a.RO_ZIPCODE_LIEF = zz.PLZ
                                                            WHERE RO_BESTELLSTATUS_ID IN (104,109)
                                                            AND RO_ORDER_KAL_TAG_ID >= '", date_fr ,"'
                                                            AND RO_ORDER_KAL_TAG_ID < '", date_to ,"'
                                                            AND RO_GLOBAL_ORDER_ID NOT LIKE 'Z-ZZZ-ZZZ-ZZZ';
                                                            "),
                       
                       "dwh_bp_sqa_segment" =  paste0("SELECT PB_KONTO_NR_16_BEWEGUNG
                                                      , t1.kal_tag_id
                                                      , t1.arbeitszeit_id --ohne 1 vorne es ist die Uhrzeit - z.B. 12023 bedeutet 20:23
                                                      , BP_BASKET_PROFILE_ID
                                                      , BON_W_UMS_BTO
                                                      FROM CIA.S_BASKET_PROFILES t1
                                                      JOIN CIA.H_PB_KONTO_KARTE_BEWEGUNG t2 ON t1.PB_KARTE_NR_16=t2.PB_KARTE_NR_16 AND t2.AUSWERTBAR_KENZ=1
                                                      JOIN CIA.LU_D_PB_MA_EINSCHLUSS fc ON fc.MA_ID=t1.MA_ID
                                                      WHERE KAL_TAG_ID >= '", date_fr ,"'
                                                      AND KAL_TAG_ID < '", date_to ,"';
                                                      ")
                       
                       )
  
  return(sql_return)
  
}
