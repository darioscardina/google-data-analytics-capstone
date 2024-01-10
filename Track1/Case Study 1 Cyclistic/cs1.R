install.packages("dplyr")                           # Install dplyr package
install.packages("plyr")                            # Install plyr package
install.packages("readr")                           # Install readr package
install.packages("tidyverse")                           # Install readr package
install.packages("validate")
install.packages("here")
install.packages("skimr")
install.packages("janitor")

library("tidyverse")
library("dplyr")                                    # Load dplyr package
library("plyr")                                     # Load plyr package
library("readr")                                    # Load readr package
library("validate")
library("here")
library("skimr")
library("janitor")

# set current working directory to the script directory 
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

df <- list.files(path = "dataset",  # Identify all CSV files in relative folder
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% # Store all files in list: apply read_csv to each csv and combine them into a list
  imap(~ .x %>% mutate(file_title = .y)) %>% # add column with the index of file TODO enhance with the name of the file
  bind_rows                                         # Combine data sets into one data set: bind any number of data frames by row

# write_csv(df, file="dataset/merged/2022-merged-divvy-tripdata.csv") # per uso futuro salvo unico file -> check if dir exists if not create dir

# df <- read_csv(df, file="dataset/merged/2022-merged-divvy-tripdata") 

df <- read_csv(file="dataset/202201-divvy-tripdata.csv") %>% mutate(file_title = 1) # uso solo un dataset per test
summary(df)
skim(df)

# ride_id è univoco
df %>%  distinct(rideable_type) # tipi di veicolo: 3 come da descrizione
df %>%  distinct(member_casual) # tipi di veicolo: 2 valori -> member, casual ma non abbiamo info sul tipo di casual (one-ride/ full-day)
# notiamo che station_names e station_id sono incompleti
# invece start_lat e start_lng sono completi, ma gli end_ quasi
# su end_lng salta all'occhio l'elevata sd. C'è qualche valore errato? o è normale e si tratta di long trips?

# controlliamo che ogni station_id corrisponda a soltanto una station_name e viceversa
start_station_ids <- df %>% 
  filter(!is.na(start_station_id) & !is.na(start_station_name)) %>% 
  select(start_station_id, start_station_name) %>% 
  dplyr::rename(station_id = start_station_id, station_name = start_station_name) %>% 
  distinct() 

# notiamo che start_station_ids ha un numero maggiore di elementi rispetto ai valori univoci di start_station_id

pivot_ids <- start_station_ids %>% 
  group_by(station_id) %>% 
  dplyr::summarize(count_station_names=n()) %>% 
  plyr::arrange(desc(count_station_names))
  # unite("station_id", c("start_station_id", "end_station_id"), na.rm = TRUE, remove=TRUE) %>% 
  
# esaminiamo il primo elemento
troubles <- start_station_ids %>%  filter(station_id == "564")

# in parte sembra che ci siano informazioni simili
words_frequency <- data.frame(table(unlist(strsplit((df$start_station_name), " ")))) %>% arrange(desc(Freq))

end_station_ids <- df %>% 
  filter(!is.na(end_station_id)) %>% 
  select(end_station_id, end_station_name) %>% 
  dplyr::rename(station_id = end_station_id, station_name = end_station_name) %>% 
  # unite("station_id", c("start_station_id", "end_station_id"), na.rm = TRUE, remove=TRUE) %>% 
  distinct()

station_ids <- rbind(start_station_ids, end_station_ids) %>% distinct()

start_station_names <- df %>% 
  filter(!is.na(start_station_name)) %>% 
  select(start_station_id, start_station_name) %>%
  distinct()

end_station_names <- df %>% 
  filter(!is.na(end_station_name)) %>% 
  select(end_station_id, end_station_name) %>%
  distinct()



df %>%  filter(is.na(start_station_name))

coords <- df %>%  filter(is.na(start_station_name)) %>% select(start_lat, start_lng) %>% distinct() # estraiamo tutte coordinate per missing values

a <- df %>%  filter(is.na(start_station_name)) %>% select(start_lat, start_lng) %>% distinct()
b <- df %>%  filter(!is.na(start_station_name)) %>% select(start_lat, start_lng, start_station_name) %>% distinct() %>%  dplyr::rename(station_name = start_station_name)
c <- df %>%  filter(!is.na(end_station_name)) %>% select(start_lat, start_lng, end_station_name) %>% distinct() %>%  dplyr::rename(station_name = end_station_name)

# meglio: facciamo al contrario per ogni station_name trovo max(lat), max(lng), min(lat), min(lng) 
# e poi per gli is.na(start_station_name) controllo se le coordinate ricadono nell'intervallo [max(lat), min(lat)] e [max(lng), min(lng)]

start_stations <- df %>% 
  select(start_station_name, start_lat, start_lng) %>% 
  filter(!is.na(start_station_name)) %>% 
  rename_with(~str_remove(., 'start_'))

end_stations <- df %>% 
  select(end_station_name, end_lat, end_lng) %>% 
  filter(!is.na(end_station_name)) %>% 
  rename_with(~str_remove(., 'end_'))

stations <- rbind(start_stations, end_stations) %>% 
  distinct()

station_coords <- stations %>% 
  group_by(station_name) %>% 
  summarize_all(list(min,max)) %>% 
  rename_with(~str_replace(., 'fn1','min')) %>% 
  rename_with(~str_replace(., 'fn2','max')) %>% 
  mutate(geo_precision = ((lat_max - lat_min) * (lng_max - lng_min)))

# ora che ho la lut stations_limits posso fillare le start_station_name e end_station_name 
install.packages("shiny")
library(shiny)
getStationName <- function(lat, lng, stations_limits) { 
  sink(stdout(), type="message")
  message(sprintf("(%52.50f, %52.50f);\n",  lat, lng))
  station_name <- stations_limits %>% 
    filter(lat_min <= lat & lat_max >= lat & lng_min <= lng & lng_max >= lng) %>% 
    filter(geo_precision == min(geo_precision)) %>% 
    select(station_name)
  message(sprintf("(%52.50f, %52.50f, %s);\n",  lat, lng, station_name))
  return(station_name)
  }
lat <- 41.88324797199999949270932120271027088165283203125000
lng <- -87.64124357699999734450102550908923149108886718750000
stations_limits %>% 
  filter(lat_min <= lat & lat_max >= lat & lng_min <= lng & lng_max >= lng) %>% 
  filter(geo_precision == min(geo_precision)) %>% 
  select(station_name)

df1 <- df #backup
df <- df %>% 
  filter(is.na(start_station_name) & !is.na(start_lat) & !is.na(start_lng)) %>% 
  mutate_at(c("start_station_name"), funs=(getStationName(start_lat, start_lng, stations_limits)))
df %>% distinct() # duplicated rows
df %>% distinct(ride_id) # duplicated ride_id

df1 <- df %>% 
  filter(!is.na(start_station_name)) 
df2 <- df %>% 
  filter(is.na(start_station_name))

df3 <- df2 %>% 
  left_join(stations_limits, join_by(between(start_lat, lat_min, lat_max), between(start_lng, lng_min, lng_max))) %>% 
  group_by(ride_id) %>% 
  filter(rank(geo_precision, ties.method= "first") == 1) %>% 
  mutate(start_station_name = station_name) %>% 
  select(-colnames(stations_limits))  

df2 %>% select(ride_id, station_name, geo_precision) %>%  group_by(ride_id) %>% filter(rank(geo_precision, ties.method= "first") == 1)

df4 <- rbind(df1, df3)

install.packages("sf")
install.packages("mapview")
library(sf)
library(mapview)

start_coords <- st_as_sf(select(df, start_lat, start_lng), coords = c("start_lng", "start_lat"),  crs = 4326)
mapview(start_coords, map.types = "Stamen.Toner") 
mapview(select(df, start_lat, start_lng), xcol = "start_lng", ycol = "start_lat", crs = 4269, grid = FALSE)

rules <- validator(SLAT = in_range(start_lat, min=-90, max=90)) + 
  validator(SLNG = in_range(start_lng, min=-90, max=90)) + 
  validator(ELAT = in_range(end_lat, min=-90, max=90)) + 
  validator(ELNG = in_range(end_lng, min=-90, max=90))

rules <- rules +
  # validator(STAT = in_range(started_at$year, min="2022", max="2022")) +
  # validator(EDAT = in_range(end_at$year, min="2022", max="2022")) +
  validator(TEST = is.finite.POSIXlt(started_at)) +
  validator(TEST1 = in_range(partenza, min="2022", max="2022"))

out <- df %>% select(started_at) %>% mutate(partenza = started_at$year) %>% confront(df, rules, lin.ineq.eps=0)
summary(out)