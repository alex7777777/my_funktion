# my_funktion
Here are my public R functions for preparing and analyzing CRM data.

CRM Customer Journey Analysis
Erstellt von Alex Gorbach

Abkürzungen
CJ – Customer Journey
DF – Data Frame

Alle hier beschriebenen Funktionen stehen auf GutHub zur Verfügung. Um sie nutzen zu können, sollte im Anfang des R-Skripts eine Zugrifffunktion eingegeben warden:

get_all_my_function  <- function(funct_name) {
  url_my_function <- "https://raw.githubusercontent.com/alex7777777/my_funktion/master/"
  source(url(paste0(url_my_function, funct_name)))
  closeAllConnections()
}

NUTZUNG DER FUNKTIONEN
get_all_my_function("NN_function.R") #NN-Nummer der Funktion aus der Liste unten eingeben
my_save_activ_robjects()

AUFLISTUNG UND BESCHREIBUNG DER FUNKTIONEN
00 Read raw data Liest Rohdaten aus dem eingegeben Ordner und gibt das RObjekt "list" unter dem Namen "raw_data" mit allen gelesenen *.csv-Dateien mit den entsprechenden Namen (ohne "*.csv") zurück. Die Datei kann auch als RObjekt gespeichert werden (SAVE_RAW_DATA=F) my_read_rawdata_csv <- function(folder_for_raw_data, SAVE_RAW_DATA=F, folder_for_save_raw_data_r="r_objects")
Output: return(raw_data) # as list:
01 Creation of help directories Erstellt Ordner mit den eingegebenen Namen aus dem Character-Vektor c("dir1", "dir2", etc.) my_create_help_dir <- function(create_dir_names)
Output: Ordner mit den eingegebenen Namen
02 Distribution function Übergibt Verteilung der Variablen. Der Häufigkeiten werden sortiert, mit kumulativen Prozentual-Werten und Missings als RObject zurückgegeben my_distrib <- function(var_distrib)
Output: return(verteilung)
03 Save table function Speichert tabellarisches RObject in den eingegebenen Ordner als *.scv-Dateien my_save_tab_function <- function(r_object_to_csv, name_for_saving, folder_name="csv_tab")
Output: return(func_message)
04 Save image function Speichert grafische Outputs / Diagramme in den eingegebenen Ordner als *.png-Dateien my_save_img_function <- function(r_img_to_png, name_for_saving, folder_name="images")
Output: return(func_message)
05 Time difference between events Übernimmt IDs und Datum der Events als Input und berechnet die täglichen Zeitdifferenzen zwischen Events als Output-Vektor my_time_diff <- function(df_ids, df_date, wihtout_time=F)
Output: return(my_df$delta_t)
06 Lines alignment and data sequencing functions Übernimmt DF mit der Zeitdifferenz in Tagen oder in Zeitklassen und liefert transponierte Kontaktketten unter Eingabe der minimalen Länge der Sequenzen (Default=1) für den Output data_sequencing <- function(data, seq_length = 1)
Output: return(transitSQ)
07 Transition matrix Nimmt transponierte Kontaktsequenzen ohne Zeitangaben und gibt die Übergangswahrscheinlichkeits-matrix zurück my_transition_matrix <- function(my_df)
Output: return(transitionMatrix)
08 One event during the day Löscht wiederholenden Events, wenn sie am gleichen Tag vorgekommen sind my_one_event_day_rule <- function(my_df)
Output: return(my_df)
09 Segmenting and cleaning Checkt & löscht Duplikate und Missings in CJ my_segmenting_cleaning <- function(my_df)
Output: return(my_df)
10 Input for the sankey diagram Berechnet aus der Pattern-Datenmatrix ohne Berücksichtigung der Zeit eine Input-Datei für das Sankey Diagram. Die Anzahl der 2er-Subsequenzen für das Diagramm wird mit Hilfe des SUPPORT_SANKEY-Parameters begrenzt. my_input_for_sankey <- function(my_df)
Output: return(links)
11 Automatical generate Time Classes Übernimmt Zeitdifferenz-Verteilung als Vektor und berechnet Codes für verschiedene Zeitklassen für die darauffolgende Reduktion der Zeitabstände (Tagen Zeitklassen-Werten). Der Output wird als list-Objekt übergeben. my_time_classes_generator <- function(time_difference_variable, NumberDeltaTimeClasses=10)
Output: return(my_list_output)
12 Automatical recode time differences to the selected time class Übernimmt eine mit Hilfe in der 11 Funktion berechnete Zeitklasse und gibt die reduzierten Zeitdifferenz-Werten als Vektor zurück. my_time_classes_recode <- function(ids, time_difference_variable, time_class_select_for_join)
Output: return(my_df[,ncol(my_df)])
13 Substring DF function for the modeling Übernimmt transponierte Rohdaten und berechnet eine Datenmatrix mit den Pattern-Häufigkeitsvariablen (Pattern-Datenbank) für die darauffolgende Modellierung, Berechnung der Übergangsmatrix sowie Links für das Sankey Diagram. Als Grenzwerte für die Anzahl der Häufigkeitsvariablen werden entweder Support- (rel. Anzahl) oder TOP-(abs. Anzahl) Parameter eingegeben. my_pattern_df <- function(data_seq, SUPPORT1er=0.00001, SUPPORT2er=0.0025, SUPPORT3er=0.0035, TOP_n_1er=40, TOP_n_2er=80, TOP_n_3er=30, SUPPORT_OR_TOP=T, WITHOUT_TIME=F)
Output: return(df_to_smartdata)
14 Create substring analysis (support) from the pattern db Berechnet Support-Output auf Basis der Smart-Daten für 1er-, 2er- und 3er-Substrings inklusive Zeitklassen my_create_support <- function(pattern_db)
Output: return(substr_support)
15 Dummy time difference between events (without time difference) Ähnlich wie die Funktion 05: Ignoriert Zeitabstände zwischen Events und gibt alle Zeitdifferenzen mit dem Wert "1" als Vektor zurück my_without_time_diff <- function(df_ids, df_date)
Output: return(my_df$delta_t)
16 Substring DF function for the sankey input Berechnet eine Datenmatrix mit den Pattern-Häufigkeitsvariablen aus den Rohdaten ohne Berücksichtigung der Zeitdifferenz. Die Anzahl der Häufigkeitsvariablen wird entweder als "Support" (rel. Anzahl) oder als "TOP" (abs. Anzahl) begrenzt. my_pattern_df_for_sankey <- function(data_seq, SUPPORT2er=0.0025, TOP_n_2er=80, SUPPORT_OR_TOP=T)
Output: return(df_to_smartdata)
17 Adding new numbered variables with NA Ergänzt die Datenmatrix um die neuen XY-Variablen mit dem vorgegebenen Wert (Default=NA) my_add_new_var <- function(my_df, numb_new_var, r_labels, fill_values=NA)
Output: return(my_df)
18 Do help variables Übernimmt Rohdaten mit Zeitklassen, Deadline für die Trennung der Datensätze in Training- und Resultat-Datensätze, Anzahl der neuen Variablen für Berechnung der neuen Attribute (count, day_for, QNr1, …, QNr12) sowie Abhängige Größe (Miete des Autos in den letzten sechs Monaten). my_help_var <- function(my_df, deadline=as.Date("2017-01-01"), numb_new_var=12, var_labels="QNr", fill_values=NA)
Output: return(data_users)
19 Create sequence data frame Übernimmt drei Vektoren aus dem DF1 (Objekt-IDs, Datum, Sequenz-IDs) und zwei Vektoren aus dem DF2 (Sequenz-IDs und Sequenz-Name) und liefert das gejointe [DF1+DF2] RObject im sequenzanalytischen Format für die weiteren Sequenzanalysen zurück. 
Als Zwischenschritt checkt die Funktion den IDs-Input des DF1 und konvertiert Character-IDs in Numeric-Format. In diesem Fall werden die Original-IDs unter dem Namen "ORIG_ID.RData" gespeichert. my_create_seq_raw_df <- function(ids_raw_df1, date_raw_df1, data_typ, event_ids_for_join1, event_ids_for_join2, event_label)
Output: return(my_df)
20 Сompact event recording (delete entries after the space) Umkodierung der Rohdaten-Events: Es wurde nur der Teil bis zum Lehrzeichen übernommen my_compact_event_recording <- function(my_events_vector)
Output: return(my_events_vector)
21 Balanced sample (only for binary dependet variable as dummy) Die Verteilung des binären Zielattributes wird balanciert: 
Sampling mit einem Zufallsgenerator 80% 20% [50% vs. 50%].
Input: Name des Zielattributes
Output: Balancierter Datensatz my_balanced_sample <- function(my_dummy_vector_name, my_df, my_ids_name)
Output: return(cj_balanced)
22 Preparation of independent variable sets for the modeling Teilt die Attribute in drei Variablen-Sets: 
- nur 1er-Häufigkeitsvariablen
- 2er- und 3er-Häufigkeits-variablen
- alle Prädiktoren 
Input: DF, Name des Zielattributes, Output-Set-Nummer
Output: Vektor mit den Nummern der ausgewählten Zielattribute my_independ_var_set <- function(my_df, dep_var, set_number)
Output: return(predictor_select[[set_number]])
23 Solution of the multi-collinearity problem Löst das Multikollinearitätsproblem: Ein von zwei miteinander stark korrelierten Prädiktoren wird für die Modellierung nicht berücksichtigt my_multicoll_rm <- function(dep_var_char, my_df, ind_var_list, break_value, 
MULTICOLL_REMOVE=T)
Output: return(ind_var_list)
24 Save output of the logistic regression Fasst die Ergebnisse der logistischen Regression zusammen, inklusive Berechnung der richtig klassifizierten Fälle (Accuracy) sowie Cragg and Uhler`s Pseudo R-Quadrat my_output_glm <- function(fit_glm, acc=0, r_squared=0)
Output: return(glm_output)
25 Cross validation Kreuzvalidierung für die logistische Regression. Als Input nimmt der Name des Zielattributes, DF, Liste der unabhängigen Variablen sowie Anzahl der Datenteile für die Kreuzvalidierung.
Output: Das Ergebnis wird nur im Konsole angezeigt my_cross_valid <- function(dep_var_char, my_df, ind_var_list, foldsNumber=10)
Output: return(add_line)
26 Save text function Speicherung Output als ASCII-Textdateien my_save_txt_function <- function(txt_for_saving, name_for_saving, folder_name="tmp", file_extention="txt")
Output: return(func_message)
27 Check missings in DF Checkt all Missings im DF. Die Missings-Zeilen werden identifiziert und gelöscht. Wenn MISSINGS_REMOVE=F, werden Missings nur identifiziert und nicht gelöscht.
Output: Gesamtzahl der Missings im DF. my_check_missings <- function(my_df, MISSINGS_REMOVE=T)
Output: return(TOTALMISSINGS)
28 Сhecking and deleting aliased variables Prüfen und Löschen das Vorhandensein mehrerer Bezeichner für eine Variable.
Wenn ALIASED_REMOVE=F, werden die Aliasingsvariablen identifiziert, aber nicht gelöscht.
Output: Neue Liste mit den nicht-aliasierten Prädiktoren. my_check_alias <- function(dep_var_char, my_df, indep_var_list, ALIASED_REMOVE=T)
Output: return(indep_var_list)
29 Select of the best predictors Diese Funktion wertet Output des Boruta -Verfahrens aus und liefert eine neue Liste mit den Prädiktoren zurück, die für die darauffolgenden (insbesondere zeitaufwändige ML-) Modelle signifikant sind. my_best_predictors_output <- function(boruta_output, SELECT_PREDICTORS=T)
Output: return(select_var_list_nr)
30 Saving of the best predictors Speichert den Output des Boruta-Verfahren als *.csv-Datei my_best_predictors_boruta_save <- function(
boruta_output, file_name="boruta", fold_name="csv_tab", file_ext="csv")
Output: -"-
31 Saving plot of the best predictors Speichert den grafischen Output des Boruta-Verfahren als *.png- und als *.pdf-Datei  my_best_predictors_boruta_save_img <- function(boruta_output, name_for_saving="boruta_img", fold_name="images")
Output: -"-
32 Samplng function Prüft der Typ des DF:
- Rohdaten (Anzahl Zeilen > Anzahl uniq IDs)
- Smart data (Anzahl Zeilen = Anzahl uniq IDs)
In beiden Fällen wird korrekt eine Stichprobe mit den vorgegebenen Wert zugrückgegeben
Output: Die Nummer der Zeilen als Vektor my_sampling_function <- function(row_vector, ids_vector, number_sampling=1000)
Output: return(row_vector)
33 Run all r-scripts Übernimmt ein Vektor mit den Namen der R-Skripts und führt diese Skripts aus.
Es wird parallel auch einen Log-File erstellt und gespeichert. Der Log-File wird auch als Output zurückgeliefert. my_run_rscripts <- function(my_r_scripts)
Output: return(log_file)
34 Modeling quality assessment Übernimmt der Name des Modells als "Char" und zwei Vektoren: Abhängige Größe sowie die aus dem Modell berechnete vorhergesagte abhängige Größe. Zurückgeliefert werden die wichtigsten Merkmale zur Modellgüte : (1) "True Negative Rate", (2) "True Positive Rate (Recall)", (3) "G-mean", (4) "Precision" und 
(5) "F-measure" my_modeling_quality_assessment <- function(modeling_name, real_vector, predictive_vector)
Output: return(modeling_quality)
35 GLM for the automatical select a time class Übernimmt unstrukturierte Rohdaten, die laufende Nummer der ausgewählten Zeitklasse, die Gesamtanzahl der zu testenden Zeitklassen, die Deadline der Training DF, der Name der Zielgröße. Zurückgegeben wird der "Pseudo R2" my_glm_for_time_class <- function(my_rawdata, SELECTED_TIME_CLASS, NUMBERTIMECLASSES=20, DEADLINE, TARGET_VAR="rent_2017", my_rawdata_dm)
Output: return(pseudo_r_squared)
36 GLM Modeling Regression diagnostic & GLM my_glm <- function(my_df, TARGET_VAR_LIST, INDEPEND_VAR_LIST, SAMPLING=1000)
Output: return(glm_output)
37 Connection DWH (View) Zugriff zur DWH DB
Geeignet um Tabellen abzufragen my_dwh <- function(my_db, my_sql_string)
Output: return(my_dwh_df)
37a Connection DWH (Write) Zugriff zur DWH DB
Speichert der RObjekte  write_to_DWH <- function(object, table_name)
Output: return()
38 Connection Adobe Zugriff zur Adobe Analytics my_adobe <- function(my_rs, my_date_from, my_date_to, my_used_elements, my_used_metrics)
Output: return(my_adobe_df)
39 Connection BigQuery Zugriff zur BigQuery my_bigquery <- function(my_project, my_sql, my_sql_type = F)
Output: return(my_bigquery_df)
40 Confidence interval Confidence intervals 
41 Transforme IDs for SQL Anpassung SQL-Befehl für "WHERE IN ("ID_1", "ID_2", …, "ID_N") my_ids_string <- function(ids)
Output: return(sql_string)
42 Convert nominal or factor to binary (metric) Konvertieren eine Nominal- oder Faktor-Variable zu den binären Variablen. Anzahl von neuen Variablen = Anzahl von Ausprägungen in der zur Konvertierung stehenden Variable. Für Leerzeichen sowie Missings wird noch extra zwei neuen Variablen berechnet my_nominal_to_binary <- function(nominal_feature_as_list, prefix_for_new_variables = "")
Output: return(nominal_feature_as_list)
43 Сhecking the number of unique ids in data frame Prüft im Laufe des Skripts die Anzahl von IDs für vorgegebenen Merkmale
WURDE am 27.05.19 GEÄNDERT my_spy <- function(df_ids_to_check, time_check=NA, daily=F, name_spy_file='my_spy_logfile.csv', folder_for_save_check_data='csv_tab', days_retro=NA, number_max_ids_to_check=3)
Output: Saved Logfile ‘my_spy_logfile.csv’
return(message)
44 Correlation plot matrices using the ellipse library Source: https://hlplab.wordpress.com/ 
my_ellipse_corr <- function(corr, outline=F,
 col = "grey",
 upper.panel=c("ellipse", "number", "none"), 
 lower.panel=c("ellipse", "number", "none"), 
 diag = c("none", "ellipse", "number"), 
 digits = 2, bty = "n", axes = FALSE, 
 xlab = "", ylab = "", asp = 1,
 cex.lab = par("cex.lab"), 
 cex = 0.75 * par("cex"), 
 mar = 0.1 + c(2, 2, 4, 2), ...)
45 Correlation plot matrices using the ellipse library Creating image my_ellipse_corr_img <- function(corr_df, main_header="Predictor correlations")
46 Listing of date from the entered day Auflistung von Daten ab dem eingegebenen tag. Das Ergebnis wird als ein List-Objekt mit "date_from"- und "date_to"-List generiert my_day_date <- function(date_from_char, number_days, date_takt=1)
Output: return(date_list)
47 Attribution without additional features Das Attributionsskript wurde ohne Zwischenspeichern und grafischen Outputs als Funktion angepasst. Parameter: date_from, date_to=date_from+nr_days my_attribution <- function(date_fr, nr_days=1, nr_days_retrospect=30, daily_att=T)
Output: return(date_list)
48 Splitting HTTP-Strings into new separate variables Nimmt eine String-Spalte (url_with_delimiters_symbol) mit einem Trennzeichen (default: "\_") und erstellt neue Variablen mit den Werten aus diesen Strings my_split_save <- function(col_to_split, delimiter="\_")
return(new_string_var)
49 Search and replace In Charakter-Strings wird nach einem Symbol oder Symbolen gesucht und ersetzt my_clean_strings <- function(df_before_clean, search_symb_char, insert_symb_char, exact=T)
return(df_after_clean)
50 Distribution tree of individual parameters in the url-strings Übernimmt ein Resultat der Funktion 48 als df und liefert Verteilung für ausgewählte Spalte (select_level_nr). Ein Segment aus der vorherigen Spalte (select_event) kann vordefiniert werden. Wenig frequentierten Events können gefiltert (percentile_cutoff) werden my_tree_distrib <- function(my_df_for_distr, percentile_cutoff=1, select_event="", select_level_nr=1)
return(dist_result)
51 Save Url for pivot table Übernimmt ein URL und liefert ein DF und eine CSV für Erstellung einer Pivot Tabelle 
52 Save all active RObjects Es wird eine Liste von Objekten übernommen (default=all objects) und im Format r_objects/JJJJ-MM-DD_DATA_from_JJJJ-MM-DD_to_JJJJ_MM_DD.RData gespeichert my_save_activ_robjects <- function(save_selected_object=ls(), date_from=NA, date_to=NA)
return(result_message)
53 Generate SQL-Strings for Big Query and DWH survey Generiert SQL-Abfragen für BigQuery und DWH. Sie übergibt SQL-Befehle mit dem Datum als Parameter über Strings my_sql_generator <- function(sql_typ_select, date_fr, days_to=7, days_fr_past=15)
return(sql_return)
54 Initial function for calculating the attribution model Basisfunktion für die Berechnung der Attribution old_attribution <- function(date_fr, number_days, nr_days_retrospect, filter_orders=T, filter_delivery="LS", filter_merchant="NO", my_spy_y=F)
55 Last full week generator Berechnung der letzten vollen Woche vom aktuellen Datum. Zurückgeliefert wird einen List mit zwei Daten als Strings: Montag und Sonntag my_last_full_week <- function() 
return(last_full_week_list)
56 Segment selecting Auswahl eines eingestellten Segmentes. Sollte keine Parameter übergeben werden, werden nur "Order" und "Lieferservice" selectiert. my_segment_select <- function(my_df, f_orders=T, f_delivery='LS', f_merchant=NA, f_webapp=NA)
return(my_df)
57 Password generator Erstellt Passwörter für verschiedene Zeiträume – täglich, wöchentlich oder monatlich sowie für bestimmte Anzahl von Jahren my_password_generator <- function(pass_lenght=12, n_yrs=5, week_or_month_or_days="m", spec_char=c("$", "§", "&", "!"))
return(my_df_return)
58 Dates generator Generiert einen String von nacheinander folgenden Datums als "Char" YYYY-MM-DD my_dates_generator <- function(date_from, days_number)
return(day_list)
