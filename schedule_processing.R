# 1) Processing Timetables

library(tidyverse)
library(UK2GTFS)
library(tidytransit)
library(gtfstools)
library(janitor)

# ------ Get GTFS network ---------

#Convert London transport network to GTFS
# path <- "london_traveline.zip"
# gtfs <- transxchange2gtfs(path_in = path, ncores = 3)
# gtfs <- gtfs_merge(gtfs, force = TRUE)
# gtfs_write(gtfs, folder = getwd(), name = "gtfs_london")

#Load in created network
gtfs <- read_gtfs("gtfs_london.zip")
gtfs$stops <- gtfs$stops %>%
  mutate(stop_lon = as.numeric(stop_lon))
summary(gtfs)

#Get distinct route types
gtfs$routes %>%
  select(route_type) %>% unique()
#0 = tram
#1 = tube
#2 = DLR and cable car
#3 = bus
#4 = ferries

#Filter out ferries
gtfs <- filter_by_route_type(gtfs, route_type = 4, keep = FALSE)
summary(gtfs)

#Filter out cable car
gtfs$routes <- gtfs$routes %>%
  filter(agency_id != "CAB")
#Now ensure compatibility with other gtfs files
gtfs$trips <- gtfs$trips %>%
  filter(route_id %in% gtfs$routes$route_id)
gtfs$stop_times <- gtfs$stop_times %>%
  filter(trip_id %in% gtfs$trips$trip_id)
gtfs$stops <- gtfs$stops %>%
  filter(stop_id %in% gtfs$stop_times$stop_id)
summary(gtfs)

# ------ QA GTFS network ------

#Count total underground stations - we need to check it contains all stations (including those outside London)

#All stations
gtfs$stops %>%
  filter(str_detect(stop_id, "9400ZZLU")) %>%
  mutate(base_id = substr(stop_id, 1, 11))%>%
  distinct(base_id) %>%
  nrow()

#Check this against station list from TfL API
station_list <- read_csv("data/station_list.csv")%>%
  clean_names() %>%
  select(name, unique_id)

#Find stations in TfL list which aren't in gtfs$stops

#First, we need to have a mutual column to join on
stops_base_id <- gtfs$stops %>%
  select(stop_id, stop_name) %>%
  filter(startsWith(stop_id, "9")) %>%
  mutate(base_id = substr(stop_id, 7, 11)) 
station_list <- station_list %>%
  filter(startsWith(unique_id, "9")) %>%
  mutate(base_id = substr(unique_id, 7, nchar(unique_id)))
station_list <- station_list %>%
  left_join(., stops_base_id, by = "base_id")

#Matching between GTFS stops and TfL codes

#Use routes, trips, and stop_times to add a column onto stops saying if it's LU, Overground, bus, etc.
#Then use this for easier matching onto TfL
#How do I know which platform stands for which?
#Does it match with TfL data?


#We need to match between GTFS stops and TfL codes!
#Example:
#GTFS: 9400ZZLUGPK
#TfL: 940GZZLUGPK

view(head(gtfs$trips))

#Could I use routes to ascertain which stations are on LU, Overground etc?
#That way I can match IDs
#And then QA

#To do:
# - Check missing stations
# - Check stations with multiple IDs (e.g. Edgware Road, Hammersmith)
# - Accessible network!
  
#Note that there are some stations (e.g. Edgware road) with multiple stop names
#Search for bracket to find these
#Could lead to inaccurate network representation
#But changing this could disrupt the network
#So next step is to see what TfL accessibility data looks like - need to compare to this