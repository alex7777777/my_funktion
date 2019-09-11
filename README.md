# Here are my public R functions for preparing and analyzing CRM data 
 
## CRM Customer Journey Analysis 
 _Erstellt von Alex Gorbach_
 
 Alle hier beschriebenen Funktionen stehen auf GutHub zur Verfügung. Um sie nutzen zu können, sollte im Anfang des R-Skripts eine Zugrifffunktion eingegeben warden: 
 
    get_all_my_function  <- function(funct_name) {
    url_my_function <- "https://raw.githubusercontent.com/alex7777777/my_funktion/master/"
    source(url(paste0(url_my_function, funct_name)))
    closeAllConnections() 
    }
