# ######################################################################## #
# 2019-05-13 - by Alex Gorbach                                             #
# Attribution as function for daily analyzes                               #
# ######################################################################## #

my_attribution <- function(date_fr, nr_days=1, nr_days_retrospect=30, daily_att=T) {
  
  start_time <- Sys.time()
  
  library(lubridate)
  library(bigrquery)
  library(jsonlite)
  library(tidyr)
  library(dplyr)
  # library(ggplot2)
  library(httpuv)
  
  attribution_v1 <- function(date_from, number_days) {
  
  date_to   <- as.character(as.Date(date_from)+number_days-1)
  date_from_past <- as.character(as.Date(date_from) - nr_days_retrospect)
  
  project <- "rd-bigdata-prd-v002"     # ag: Name DB in Big Query
  orders_sql <- paste0("SELECT
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
                       
                       WHERE _PARTITIONTIME >= '", as.character(as.Date(date_from) - 15), " 00:00:00'
                       AND _PARTITIONTIME <= '", as.character(as.Date(date_to) + 15), " 00:00:00'
                       AND DATE(received_at) <= DATE('", date_to, "')
                       AND DATE(received_at) >= DATE('", date_from, "') 
                       AND activity = 'order_created'
                       ;")
  
  source("my_function/39_function.R")
  orders_raw <- my_bigquery(project, orders_sql)
  
  # Filtering
  
  # a) only orders, order_value_cent > 0
  orders <- orders_raw
  orders$order_value_cent <- as.integer(orders$order_value_cent)
  
  # b) only lieferservice (DELIVERY) zzgl. mix cusomer (Marktplatz):
  orders <- subset(orders, (DELIVERY_TYPE_1 == '"DELIVERY"') | (DELIVERY_TYPE_2 == '"DELIVERY"'))
  
  # c) only REWE zzgl. mix cusomer (MERCHANT_TYPE_2 == '"REWE"'):
  orders <- subset(orders, (MERCHANT_TYPE_1 == '"REWE"') | (MERCHANT_TYPE_2 == '"REWE"'))  # REWE
  
  # d) only WEB
  orders$web_vs_app <- ifelse(orders$web_app == '"WebShop"', "Web", "App")
  orders <- orders[grepl('Web', orders$web_app) , ]
  
  # Activities: Orders, mit vorangegangenen Activities (30 Tage = nr_days_retrospect)
  
  # 2. SQL: 
  # a) Identifizieren von "customer_uuid" nur für Käufer ['order_created'] in der Zeit [-11 days; -4 days] vom akt. Datum
  # b) und nur mit einer identifizierbaren CoockiesID [not reference = '']
  # c) aggregiert (group by) "reference" und "customer_uuid"
  # d) Join über "reference" von Käufern a)-c) mit CJ von sich selbst mit vorangegangenen Activities [-70 days; -4 days], 
  #    auch wenn sie damals nichts gekauft haben [zzgl. 'shop_visited','campaign_hit']
  
  activities_ref_sql <- paste0("#standardSQL
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
                               WHERE _PARTITIONTIME >= '", as.character(as.Date(date_from) - 15), " 00:00:00'
                               AND _PARTITIONTIME <= '", as.character(as.Date(date_to) + 15), " 00:00:00'
                               and DATE(received_at) <= DATE('", date_to, "')
                               and DATE(received_at) >= DATE('", date_from, "')
                               and activity = 'order_created'
                               )
                               and not (reference = '')
                               group by 1,2
                               ) as b on (a.reference = b.reference)
                               
                               WHERE _PARTITIONTIME >= '", as.character(as.Date(date_from) - 15), " 00:00:00'
                               AND _PARTITIONTIME <= '", as.character(as.Date(date_to) + 15), " 00:00:00'
                               AND DATE(received_at) <= DATE('", date_to, "')
                               AND DATE(received_at) > DATE('", date_from_past, "')
                               AND activity in ('shop_visited','campaign_hit','order_created')
                               )
                               ")
  
  activities_ref <- my_bigquery(project, activities_ref_sql)
  
  activities_uuid_sql <- paste0("#standardSQL
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
                                WHERE _PARTITIONTIME >= '", as.character(as.Date(date_from) - 15), " 00:00:00'
                                AND _PARTITIONTIME <= '", as.character(as.Date(date_to) + 15), " 00:00:00'
                                AND DATE(received_at) <= DATE('", date_to, "')
                                AND DATE(received_at) >= DATE('", date_from, "')
                                AND activity = 'order_created' 
                                group by 1
                                ) as b on (a.customer_uuid = b.customer_uuid)
                                
                                WHERE _PARTITIONTIME >= '", as.character(as.Date(date_from) - 15), " 00:00:00'
                                AND _PARTITIONTIME <= '", as.character(as.Date(date_to) + 15), " 00:00:00'
                                AND DATE(received_at) <= DATE('", date_to, "')
                                AND DATE(received_at) >= DATE('", date_from_past, "')
                                AND activity in ('shop_visited','campaign_hit','order_created')
                                AND reference = ''
                                )
                                ")
  
  activities_uuid <- query_exec(activities_uuid_sql,project = project,use_legacy_sql = FALSE,  max_pages = Inf)
  
  
  # ######################################################################### #
  #  alle Order_created einlesen, für Neukunden/Bestandskundenunterscheidung
  # ANMERKUNG: Die Daten wurden erst ab dem 2017-01-01 gesammelt, damit 
  # können Neu-/Bestandskunden nur beschränkt unterschieden werden
  # ######################################################################### #
  
  # 4. SQL: 
  # a) Identifizieren von "customer_uuid" nur für die Käufer ['order_created'] in der Zeit [-11 days; -4 days] vom akt. Datum,
  #    gruppiert über "customer_uuid", "received_at" und "order_id"
  # b) Join über "customer_uuid" der Käufer mit CJ von sich selbst mit vorangegangenen Käufern
  # c) Liefern von Zusatzspalten falls vorhanden: Vorletzter Kauf "order_id_before" mit dem Datum "received_at_before"
  # um nur dijenige Aktivitäten zu betrachten, die in der Zeit zwischen den Käufern vergangen sind
  
  orders_all_sql <- paste0("#standardSQL
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
                           WHERE _PARTITIONTIME >= '", as.character(as.Date(date_from) - 15), " 00:00:00'
                           AND _PARTITIONTIME <= '", as.character(as.Date(date_to) + 15), " 00:00:00'
                           AND DATE(received_at) <= DATE('", date_to, "')
                           AND DATE(received_at) >= DATE('", date_from, "')
                           AND activity = 'order_created' 
                           group by 1,2,3
                           ) as b on (a.customer_uuid = b.customer_uuid and a.received_at <= b.received_at)
                           
                           WHERE activity in ('order_created')
                           )
                           ")
  
  orders_all_time <- my_bigquery(project, orders_all_sql)
  
  orders_all_count <- group_by(orders_all_time, order_id) %>% 
    summarise(anzahl_orders=n())
  
  # umbenennen
  orders_all_count <- mutate(orders_all_count, kundenstatus = case_when(
    orders_all_count$anzahl_orders == 1 ~ "Neukunde",
    orders_all_count$anzahl_orders > 1 ~ "Bestandskunde",
    TRUE ~ "unbekannt")
  )
  
  # es gibt doppelte Eintraege auf BigData-Plattform, => 
  # unique machen (getrennt nach Orders und Nicht-Orders)
  
  # 5. Zusammenfuehren 
  activities <- rbind(activities_uuid, activities_ref)
  activities_orders <- filter(activities,activity =="order_created")
  activities_orders_unique <- distinct(activities_orders, order_id, .keep_all = TRUE)
  activities_non_orders <- filter(activities,activity !="order_created")
  activities_non_orders <- arrange(activities_non_orders, customer_uuid, received_at, activity)
  # Ausschluss von Dubletten, aber auch von shop_visits mit zeitgleicher campaign_hit
  activities_non_orders_unique <- distinct(activities_non_orders, customer_uuid, received_at, .keep_all = TRUE)
  
  # 6. activities_non_orders_unique - Aktivitäten von Käufern
  
  # Aktivitäten der zeitlich darauffolgenden Order zufügen
  activities_unique = rbind(activities_non_orders_unique, activities_orders_unique)
  activities_unique <- arrange(activities_unique, customer_uuid, received_at)
  
  activities_test <- activities_unique %>%
    group_by(customer_uuid) %>% fill(order_id, .direction = c("up")) %>% ungroup()
  activities_001 <- filter(activities_test, !is.na(order_id))
  
  # 7.
  
  # Join von Orders, Orders_all und Aktivitaeten, Verringerung der Menge
  # durch mehrere Orders je customer (ausserhalb des Betrachtungszeitraums)
  
  orders_unique <- distinct(orders, order_id, .keep_all = TRUE)
  orders_unique <- select(orders_unique,-c(MERCHANT_TYPE_1, MERCHANT_TYPE_2, DELIVERY_TYPE_1, DELIVERY_TYPE_2, ID, CUSTOMER_TYPE_1, order_value_cent))
  orders_unique <- left_join(orders_unique, orders_all_count, by = "order_id")
  
  # LOGIK
  
  # 8. attribution_001 a-d
  
  attribution_001 <- inner_join(activities_001, orders_unique, by="order_id")
  
  attribution_001 <- attribution_001 %>%
    select(-c(CUSTOMER_UUID, REFERENCE)) %>%
    arrange(customer_uuid, received_at) %>%        
    filter(received_at <= order_created_received_at) 
  
  attribution_001b <- mutate(attribution_001, marketing_channel = case_when(
    grepl("google", attribution_001$m_channel_referer, fixed=TRUE) ~ "seo",
    grepl("bing", attribution_001$m_channel_referer, fixed=TRUE) ~ "seo",
    grepl("yahoo", attribution_001$m_channel_referer, fixed=TRUE) ~ "seo",
    TRUE ~ attribution_001$marketing_channel)   
  )
  
  attribution_001c <- mutate(attribution_001b, marketing_channel = case_when(
    attribution_001b$marketing_channel == "dir" ~ "ref",
    attribution_001b$marketing_channel == "sea" ~ "sea",
    attribution_001b$marketing_channel == "seo" ~ "seo",
    attribution_001b$marketing_channel == "pla" ~ "pla",
    attribution_001b$marketing_channel == "crm" ~ "crm",
    attribution_001b$marketing_channel == "dim" ~ "dim",
    attribution_001b$marketing_channel == "ret" ~ "ret",
    attribution_001b$marketing_channel == "aff" ~ "aff",
    attribution_001b$marketing_channel == "app" ~ "app",
    attribution_001b$marketing_channel == "ref" ~ "ref",
    attribution_001b$marketing_channel == "cop" ~ "cop",
    
    TRUE ~ "div")
  )
  
  attribution_001d <- mutate(attribution_001c, marketing_channel = case_when(
    attribution_001c$marketing_channel == "ref" & attribution_001c$m_channel_referer == '"direct"' ~ "dir",
    attribution_001c$marketing_channel == "ref" & attribution_001c$m_channel_referer == '"no-referer"' ~ "dir",
    attribution_001c$marketing_channel == "ref" & attribution_001c$m_channel_referer == '""' ~ "dir",
    TRUE ~ attribution_001c$marketing_channel)
  )
  
  # 9. attribution_001 e-i
  
  attribution_001e <- attribution_001d %>%
    mutate(lag.received_at = lag(received_at, n = 1, default =NA)) %>%
    mutate(lag.activity = lag(activity, n=1, default='leer')) %>%
    mutate(lag.customer_uuid = lag(customer_uuid, n=1, default='leer')) %>%
    mutate(lag.order_id = lag(order_id, n=1, default='leer')) %>%
    mutate(lag.marketing_channel = lag(marketing_channel, n=1, default='leer')) %>%
    mutate(time_diff_min = (received_at - lag.received_at)/60) %>%
    mutate(days_activity_order = (order_created_date - date(received_at)))
  
  # Ausschluss von shop_visits_direkt, die innerhalb von 30 Min liegen (3.000)  
  #  attribution_001f <- attribution_001e %>%
  #  filter(!(activity == "shop_visited" & marketing_channel == "dir" & lag.activity == "shop_visited" & lag.marketing_channel == 'dir' & time_diff_min < 30 & order_id == lag.order_id))
  
  attribution_001f <- attribution_001e %>%
    filter(!(activity == lag.activity & marketing_channel == lag.marketing_channel & time_diff_min < 30 & order_id == lag.order_id))
  
  # Markierung von order_created ohne ZwischenVisit: order_double und order_created ohne visit/campaign: order_wo_visit
  attribution_001g <- attribution_001f %>%
    mutate(order_double = if_else(customer_uuid == lag.customer_uuid & activity == "order_created" & lag.activity == "order_created",1,0)) %>%
    mutate(order_wo_visit = if_else(order_id != lag.order_id & activity == 'order_created',1,0))
  count(attribution_001g,order_double)  # 334 Faelle / new 2019-03-11: 296 Faelle
  count(attribution_001g,order_wo_visit) # 80556 Faelle (334 sind hier eingeschlossen)
  
  
  # Activities, die mehr als 30 Tage vor Order liegen, rausfiltern
  
  attribution_001h <- attribution_001g %>%
    filter(days_activity_order < 30)
  
  # Order_Created rausfiltern
  attribution_001i <- attribution_001h %>%
    filter(activity != 'order_created')
  
  # 10. attribution_002 - 006
  
  # Attribution erstellen
  attribution_002 <- group_by(attribution_001i,order_id) %>%
    summarise(touchpoints=n())
  attribution_003 <- left_join(attribution_001i,attribution_002,by="order_id")
  attribution_004 <- within(attribution_003,key <- 1/touchpoints)
  
  # Function
  source("my_function/my_attr_calc_func.R")
  attribution_final <- attribution_calc(attribution_004)
  att_date <- paste0("from_", date_from, "_to_", date_to)
  attribution_final$datum <- att_date
  
  source("my_function/03_function.R")
  my_save_tab_function(attribution_final, "attribution_final")
  
  }
  
  if(daily_att) {
    source("my_function/46_function.R")
    day_date_list <- my_day_date(date_fr, nr_days)
    
    for(i in 1:nr_days){
      date_from <- day_date_list[[i]]
      date_to   <- as.character(as.Date(day_date_list[[i]])+1)
      date_from_past <- as.character(as.Date(date_from) - nr_days_retrospect)
      cat(paste0("\nSTEP ", i, "\n", i, ": Date from: ", date_from, "\n", 
                 i, ": Date to: ", date_to, "\n"))
      attribution_v1(date_from, 1)
    }
    
  } else {
    
    cat(paste0("\nDate from: ", date_fr, 
               "\nDate to:   ", as.character(as.Date(date_fr) + nr_days - 1), "\n"))
    attribution_v1(date_fr, nr_days)
  }

  
  
  return(cat("\n\nTotal time for the script execution:", round(Sys.time()-start_time, 1), 
             "\nTime for the script execution per day:", round((Sys.time()-start_time)/nr_days, 1), "\n"))
}