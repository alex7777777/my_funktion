#######################################################
# 2018-01-29 - by Alex Gorbach
# DB Datensatz Carsharing - Flinkster
# Buchungen Carsharing 
# http://data.deutschebahn.com/dataset/data-flinkster
#######################################################

# Function: # Сompact event recording (delete entries after the space)
my_compact_event_recording <- function(my_events_vector) {
  my_events_vector <- gsub("\\ .*", "", my_events_vector)
  print(data.frame(table(my_events_vector)))
  return(my_events_vector)
}
