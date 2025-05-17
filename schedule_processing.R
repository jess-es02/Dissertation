# 1) Processing Timetables

library(tidyverse)
library(UK2GTFS)
library(tidytransit)
library(gtfstools)
library(janitor)
library(sf)
library(tmap)
library(tmaptools)
library(osmextract)
library(httr)
library(jsonlite)
options(java.parameters = "-Xmx2G")
library(r5r)

# ------ Build GTFS network ---------

#Convert London transport network to GTFS
path <- "large_data/london_traveline.zip"
gtfs <- transxchange2gtfs(path_in = path, silent = FALSE, ncores = 3)

#Fix inconsistency in the network: some stops are mentioned in gtfs$stop_times but not gtfs$stpops
#Adding stops from TfL which aren't in NaPTAN dataset

#First, find stop ids which are in stop_times but not stops
missing_stop_ids <- setdiff(unique(gtfs$stop_times$stop_id), gtfs$stops$stop_id)

#Get TfL API key from root R environment
api_key <- Sys.getenv("tfl_api_key")

#Function to query a single stop
get_stop_id <- function(stop_id) {
  url <- paste0('https://api.tfl.gov.uk/StopPoint/', stop_id, "?app_key=", api_key)
  response <- GET(url)

  if(status_code(response) == 200){
    text <- content(response, as = "text", encoding = "UTF-8") 
    stop_data <- fromJSON(text)
    
    data.frame(
      gtfs_id = stop_id,
      tfl_id = stop_data$naptanId,
      stop_name = stop_data$commonName,
      lat = stop_data$lat,
      lon = stop_data$lon
      )
  } else {
    warning(paste("Could not find stop ID", stop_id))
    return(NULL)
  }
}

#Get information for all
extra_stop_ids <- missing_stop_ids %>%
  map(get_stop_id)%>%
  list_rbind()

#Check whether NAPTAN ID is already in gtfs$stops
extra_stop_ids %>%
  filter(tfl_id %in% gtfs$stops$stop_id) #no duplicates

#It looks like TfL IDs are a different format to GTFS - let's add the GTFS IDs first, and TfL later
#Append new GTFS IDs to gtfs$stops
stops_to_append <- extra_stop_ids%>%
  mutate("stop_code" = NA)%>%
  rename("stop_id" = gtfs_id,
         "stop_lon" = lon,
         "stop_lat" = lat)%>%
  select(stop_id, stop_code, stop_name, stop_lon, stop_lat)
gtfs$stops <- rbind(gtfs$stops, stops_to_append)
rm(stops_to_append)

#Filter out ferries
gtfs <- filter_by_route_type(gtfs, route_type = 4, keep = FALSE)

#Filter out cable car (currently classed as a bus)
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

#Convert calendar_date to date format
gtfs$calendar_dates <- gtfs$calendar_dates %>%
  mutate(date = as.Date(date, format = "%Y%m%d"))

#Check GTFS object
output_path <- tempfile("validation_result")
validator_path <- download_validator(tempdir())
validate_gtfs(gtfs, output_path, validator_path)

#To look into:
# - duplicate route names
# - fast travel between stops (and stops further away)

#Write completed object locally
#gtfs <- gtfs_merge(gtfs, force = TRUE)
gtfs_write(gtfs, folder = "large_data", name = "gtfs_london")

#Clean workspace XXX
rm(path, missing_stop_ids, output_path, validator_path, extra_stop_ids)

#Load in created network
gtfs <- read_gtfs("large_data/gtfs_london.zip")
gtfs$stops <- gtfs$stops %>%
  mutate(stop_lon = as.numeric(stop_lon))
summary(gtfs)

# ------ Create lookup table between GTFS IDs and TfL stop IDs ------
#Just filter for stops where route_type = 1?
#Then use API
#Then look to parts below to QA network, e.g. having two Edgware Roads?
#Or any TfL stops which aren't in GTFS?

# ----- Match GTFS stop IDs and TfL stop IDs ------

# #First, load in station list from TfL API 
# station_list <- read_csv("data/station_list.csv")%>%
#   clean_names() %>%
#   select(name, unique_id)
# 
# #Example match - though it is often more complex than this!
# #GTFS: 9400ZZLUGPK
# #TfL: 940GZZLUGPK
# 
# view(gtfs$routes %>% 
#   count(agency_id))

#Relevant agency_ids:
#DLR
#LUL - LU
#TCL - London Tramlink
#Where is Overground???

#All others are buses/coaches, except for:

#Use routes, trips, and stop_times to add a column onto stops saying if it's LU, Overground, bus, etc.
#Then use this for easier matching onto TfL
#How do I know which platform stands for which?
#Does it match with TfL data?


#Could I use routes to ascertain which stations are on LU, Overground etc?
#That way I can match IDs
#And then QA

# # ------ QA GTFS network ------
# 
# #Count total underground stations - we need to check it contains all stations (including those outside London)
# 
# #All stations
# gtfs$stops %>%
#   filter(str_detect(stop_id, "9400ZZLU")) %>%
#   mutate(base_id = substr(stop_id, 1, 11))%>%
#   distinct(base_id) %>%
#   nrow()
# 
# #Find stations in TfL list which aren't in gtfs$stops
# 
# #First, we need to have a mutual column to join on
# stops_base_id <- gtfs$stops %>%
#   select(stop_id, stop_name) %>%
#   filter(startsWith(stop_id, "9")) %>%
#   mutate(base_id = substr(stop_id, 7, 11)) 
# station_list <- station_list %>%
#   filter(startsWith(unique_id, "9")) %>%
#   mutate(base_id = substr(unique_id, 7, nchar(unique_id)))
# station_list <- station_list %>%
#   left_join(., stops_base_id, by = "base_id")
# 
# 
# 
# #To do:
# # - Check missing stations
# # - Check stations with multiple IDs (e.g. Edgware Road, Hammersmith)
# # - Accessible network!
# 
# 
# #Could do TfL network, query distance between adjacent stops with API?
#   
# #Note that there are some stations (e.g. Edgware road) with multiple stop names
# #Search for bracket to find these
# #Could lead to inaccurate network representation
# #But changing this could disrupt the network
# #So next step is to see what TfL accessibility data looks like - need to compare to this

# -------- Download street network --------
# osm_path <- oe_get("Greater London",
#                    provider = "geofabrik",
#                    download_directory = "large_data",
#                    download_only = TRUE)

# ------- Prepare other necessary data -------

#1) London LSOAs
#LSOA boundaries
lsoas <- st_read("data/LSOA_2021_EW_BSC_V4.shp")%>%
  clean_names()%>%
  select("lsoa21cd", "lsoa21nm", "geometry")

#Load in a csv with only London LSOA codes
#obtained from a random download from London Datastore (https://data.london.gov.uk/census/2021-ward-and-lsoa-estimates/)
london_codes <- read_csv("data/london_lsoas.csv")%>%
  clean_names()

#Filter for only London LSOAs
london_lsoas <- lsoas %>%
  filter(lsoa21cd %in% london_codes$lsoa21cd)

#2) Origins: pop-weighted centroids
#LSOA pop-weighted centroids
pop_centroids <- read_csv("data/lsoa_pop_weighted_centroids.csv")%>%
  clean_names()%>%
  st_as_sf(., coords = c("x", "y"), crs = 4326)%>%
  st_transform(., crs=27700)%>%
  select("lsoa21cd","geometry")%>%
  filter(lsoa21cd %in% london_codes$lsoa21cd)

#3) Destinations: workplace-weighted centroids
#The ONS does not release these, so we will estimate using OA-level data

#First, load in OA shapefile
oas <- st_read("data/OA_2021_EW_BFC_V8.shp")%>%
  clean_names() %>%
  select(oa21cd, lsoa21cd, lat, long) #this is a representative centroid which always falls inside the OA; different to a centroid

#Filter for only OAs in London
oas <- oas %>%
  filter(lsoa21cd %in% london_lsoas$lsoa21cd)

#Load in OA-level working population
working_pop_oa <- read_csv("data/workforce_pop_oa.csv")%>%
  clean_names()%>%
  rename("oa21cd" = output_areas_code, 
         "working_pop" = count)

#Append working population to sf object
oas <- oas %>%
  left_join(., working_pop_oa, by = "oa21cd")

#Find weighted average
#We will use the provided "representative centroids", but could change these to geometric centroids
workforce_centroids <- oas %>%
  st_drop_geometry()%>%
  group_by(lsoa21cd)%>%
  summarise(
    weighted_lat = sum(lat * working_pop)/sum(working_pop),
    weighted_lon = sum(long * working_pop)/sum(working_pop))%>%
  st_as_sf(., coords = c("weighted_lon", "weighted_lat"), crs=4326)%>%
  st_transform(., 27700)

#4) Origin Attributes

#Age: proportion under 5 and 65+ (TS007B - Age by broad age bands)
age <- read_csv("data/nomis_age.csv")%>%
  clean_names()%>%
  rename(
    "lsoa21cd" = mnemonic,
    "total_pop" = total,
    "total_under_5" = aged_4_years_and_under)%>%
  mutate(total_65_plus = aged_65_to_74_years + aged_75_to_84_years + aged_85_years_and_over,
         pct_65_plus = round(100*total_65_plus/total_pop, 3),
         pct_under_5 = round(100*total_under_5/total_pop, 3))%>%
  select(lsoa21cd, total_pop, total_under_5, pct_under_5, total_65_plus, pct_65_plus)

#Disability: disabled under equality act (TS038 - Disability)
disability <- read_csv("data/nomis_disability.csv")%>%
  clean_names()%>%
  rename(
    "lsoa21cd" = mnemonic,
    "total_pop" = total_all_usual_residents,
    "total_disabled" = disabled_under_the_equality_act)%>%
  mutate(pct_disabled = round(100*total_disabled/total_pop, 3))%>%
  select(lsoa21cd, total_disabled, pct_disabled)

#Join to pop centroids
pop_centroids <- pop_centroids %>%
  left_join(., age, by="lsoa21cd")%>%
  left_join(., disability, by="lsoa21cd")
  
#5) Destination Attributes: workforce pop
working_pop_lsoa <- read_csv("data/workforce_pop_lsoa.csv")%>%
  clean_names()%>%
  rename("lsoa21cd" = lower_layer_super_output_areas_code, 
         "working_pop" = count)%>%
  select(-lower_layer_super_output_areas_label)

#Join to workforce centroids
workforce_centroids <- workforce_centroids %>%
  left_join(., working_pop_lsoa, by="lsoa21cd")

#Clean workspace
rm(london_codes, lsoas, oas, working_pop_lsoa, working_pop_oa, age, disability)

# -------- Basic r5r query -----------
r5r_core <- setup_r5(data_path = "large_data", verbose=TRUE)
#Looks like lots of stops not linked to street network?
#e.g. 109, 110

#use accessibility function
#see how long each takes - then add to for loop?

#To do:
# - Investigate problems in GTFS validator
# - GTFS calendar looks wrong!
# - Create NAPTAN/TfL ID lookup table
# - Look into GTFS in more detail (see commented section) - e.g. multiple stops for Edgware Road
# - See warnings with r5r_core setup - e.g. stops not linking to street network
  # - See error message file
# - Then see if running a basic r5r query works
  # - Some centroids don't lie on roads - need to make sure it is still getting these in the query output!
