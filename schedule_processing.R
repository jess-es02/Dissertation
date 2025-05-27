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

# ------ Build base GTFS network ---------

# #Convert London transport network to GTFS
# path <- "large_data/london_traveline.zip"
# gtfs <- transxchange2gtfs(path_in = path, silent = FALSE, ncores = 3)
# 
# #Fix inconsistency in the network: some stops are mentioned in gtfs$stop_times but not gtfs$stpops
# #Adding stops from TfL which aren't in NaPTAN dataset
# 
# #First, find stop ids which are in stop_times but not stops
# missing_stop_ids <- setdiff(unique(gtfs$stop_times$stop_id), gtfs$stops$stop_id)
# 
#Get TfL API key from root R environment
api_key <- Sys.getenv("tfl_api_key")

#Function to query a single stop 
# #We add an option to get stop information without coordinates, for calling later
get_stop_id <- function(stop_id, with_coords = TRUE) {
 url <- paste0('https://api.tfl.gov.uk/StopPoint/', stop_id, "?app_key=", api_key)
 response <- GET(url)
 #Get data
 if (status_code(response) == 200) {
   text <- content(response, as = "text", encoding = "UTF-8")
   stop_data <- fromJSON(text)
   #Don't request unnecessary data if we don't need coordinates!
   if (with_coords) {
     data.frame(
       gtfs_id = stop_id,
       tfl_id = stop_data$naptanId,
       stop_name = stop_data$commonName,
       lat = stop_data$lat,
       lon = stop_data$lon)
   } else {
     data.frame(
       gtfs_id = stop_id,
       tfl_id = stop_data$naptanId,
       stop_name = stop_data$commonName)
   }
 } else {
   warning(paste("Could not find stop ID", stop_id))
   return(NULL)
 }
}
 
# #Get information for all
# extra_stop_ids <- missing_stop_ids %>%
#   map(get_stop_id)%>%
#   list_rbind()
# 
# #Check whether NAPTAN ID is already in gtfs$stops
# extra_stop_ids %>%
#   filter(tfl_id %in% gtfs$stops$stop_id) #no duplicates
# 
# #It looks like TfL IDs are a different format to GTFS - let's add the GTFS IDs first, and TfL later
# #Append new GTFS IDs to gtfs$stops
# stops_to_append <- extra_stop_ids%>%
#   mutate("stop_code" = NA)%>%
#   rename("stop_id" = gtfs_id,
#          "stop_lon" = lon,
#          "stop_lat" = lat)%>%
#   select(stop_id, stop_code, stop_name, stop_lon, stop_lat)
# gtfs$stops <- rbind(gtfs$stops, stops_to_append)
# rm(stops_to_append)
# 
# #Filter out ferries
# gtfs <- filter_by_route_type(gtfs, route_type = 4, keep = FALSE)
# 
# #Filter out unwanted buses:
# #DBSE, BM, and SS are non-TfL (coaches/buses to Heathrow)
# #Cable car is currently classed as a bus
# gtfs$routes <- gtfs$routes %>%
#   filter(!agency_id %in% c("CAB", "DBSE", "BM", "SS"))
# #Now ensure compatibility with other gtfs files
# gtfs$trips <- gtfs$trips %>%
#   filter(route_id %in% gtfs$routes$route_id)
# gtfs$stop_times <- gtfs$stop_times %>%
#   filter(trip_id %in% gtfs$trips$trip_id)
# gtfs$stops <- gtfs$stops %>%
#   filter(stop_id %in% gtfs$stop_times$stop_id)
# summary(gtfs)
# 
# #Convert calendar_date to date format
# gtfs$calendar_dates <- gtfs$calendar_dates %>%
#   mutate(date = as.Date(date, format = "%Y%m%d"))
# 
# #Brief look into "duplicate" routes
# duplicate_routes <- gtfs$routes %>%
#   group_by(route_long_name, route_short_name) %>%
#   filter(n() > 1) %>%
#   ungroup()
# duplicate_routes %>% distinct(route_id) %>% nrow() #each is a different route, hopefully should be fine
# 
# #Manually correcting incorrect route information
# 
# #Sort 'Willow Lawn (Ruislip Lido Railway)' - it has the wrong ID
# #Change ID to 9400ZZRLWLN
# gtfs$stops <- gtfs$stops %>%
#   mutate(stop_id = if_else(stop_id == '9400ZZLUHPC2', '9400ZZRLWLN', stop_id))
# #Create a duplicate of Hyde Park Corner with 9400ZZLUHPC2
# hpc_row <- data.frame(stop_id = '9400ZZLUHPC2', stop_code = NA, stop_name = 'Hyde Park Corner Underground Station', stop_lon = -0.153129, stop_lat = 51.50278)
# gtfs$stops <- rbind(gtfs$stops, hpc_row)
# 
# #The NAPTAN dataset contains two Oakhill Roads with the same stop ID in different locations
# #Oakhill Road Sutton should have different coordinates - the TfL ID corresponds to a different NAPTAN code
# #490002334ZT should be on the S2, rather than 490013611E at present
# #490002334ZT is not in gtfs$stops
# #490013611E is in the same place as 490010487E, which is a functioning stop for the 255
# #So we will update the coordinates for 490013611E to reflect stop 490002334ZT, recognising that this does not fully match the NAPTAN data
# gtfs$stops <- gtfs$stops %>%
#   mutate(stop_lon = if_else(stop_id == '490013611E', -0.19446, stop_lon),
#          stop_lat = if_else(stop_id == '490013611E', 51.37127, stop_lat))
# 
# #There are still unrealistic travel times between some consecutive stops 
# #But the stops seem to be in the right location - it is a limitation of the timetabling
# #e.g. stop 1: 10:59:59 -> stop 2: 11:00:00
# 
# #Moving some stops as setup_r5r was not joining them to the network
# #1) Barking stops were excluded because they are closer to a private walkway - let's move them nearer the public-facing footpath
# barking_stops <- c("9400ZZLUBKG3", "9400ZZLUBKG2", "9400ZZLUBKG1")
# gtfs$stops <- gtfs$stops %>%
#   mutate(stop_lon = if_else(stop_id %in% barking_stops, 0.081114, stop_lon),
#          stop_lat = if_else(stop_id %in% barking_stops, 51.53926, stop_lat))
# rm(barking_stops)
# 
# #2) Upney Underground station looks too far from a walkway - let's move it closer to the road (the only entrance is here anyway)
# gtfs$stops <- gtfs$stops %>%
#   mutate(stop_lon = if_else(stop_id == '9400ZZLUUPY1', 0.1014915, stop_lon),
#          stop_lat = if_else(stop_id == '9400ZZLUUPY1', 51.53846, stop_lat))
# 
# #3) Not sure why Christchurch Road won't connect - but let's move it slightly closer to the main road to see if that works
# gtfs$stops <- gtfs$stops %>%
#   mutate(stop_lon = if_else(stop_id == '490005233W', 0.0973208, stop_lon),
#          stop_lat = if_else(stop_id == '490005233W', 51.42659, stop_lat))
# 
# #Check GTFS object
# output_path <- tempfile("validation_result")
# validator_path <- download_validator(tempdir())
# validate_gtfs(gtfs, output_path, validator_path)
# 
# #Write completed object locally
# gtfs_write(gtfs, folder = "large_data", name = "gtfs_london")
# 
# rm(path, missing_stop_ids, output_path, validator_path, extra_stop_ids, hpc_row)

#Load in created network
gtfs <- read_gtfs("large_data/gtfs_london.zip")
gtfs$stops <- gtfs$stops %>%
  mutate(stop_lon = as.numeric(stop_lon))
summary(gtfs)

# ------ Create lookup table between GTFS IDs and TfL stop IDs ------

#Filter to only stops on London Underground routes (route_type = 1); bus, DLR, and tram are fully accessible
tube_trips <- gtfs$routes %>%
  filter(route_type == 1)%>%
  left_join(., gtfs$trips, by ="route_id")%>%
  distinct()
stops_on_tube_trips <- tube_trips %>%
  select(trip_id)%>%
  left_join(., gtfs$stop_times, by="trip_id")%>%
  select(stop_id)%>%
  distinct()
tube_stops <- stops_on_tube_trips$stop_id

#Use API to ensure that we have an associated TfL ID for each GTFS ID
id_lookup <- tube_stops %>%
  map(get_stop_id, with_coords=FALSE)%>%
  list_rbind()

id_lookup %>% distinct(tfl_id) %>% nrow() #270 - correct (as there are two Edgware Roads and Hammersmiths)
rm(tube_trips, stops_on_tube_trips, tube_stops)

# ------- Prepare geographic and demographic data -------

#1) LSOAs - inside London, and close to stops
#LSOA boundaries
lsoas <- st_read("data/LSOA_2021_EW_BSC_V4.shp")%>%
  clean_names()%>%
  select("lsoa21cd", "lsoa21nm", "geometry")

#Load in a csv with only London LSOA codes
#obtained from a random download from London Datastore (https://data.london.gov.uk/census/2021-ward-and-lsoa-estimates/)
london_codes <- read_csv("data/london_lsoas.csv")%>%
  clean_names()

#Filter for only London LSOAs
london_lsoas <- lsoas %>%#
  filter(lsoa21cd %in% london_codes$lsoa21cd)

#Add on any LSOAs with GTFS stops (even if they're outside of London)
stop_locations <- gtfs$stops %>%
 st_as_sf(., coords = c("stop_lon", "stop_lat"), crs=4326)%>%
 st_transform(., 27700)

#Let's do a 2km buffer, to reflect people who can feasibly walk to these stops
stop_buffers <- st_buffer(stop_locations, dist = 2000)
#Find LSOAs intersecting with the stop buffers
stop_buffer_lsoas <- st_filter(lsoas, stop_buffers)
 
#Combine all potential LSOAs: those in London, and those within 2km of a London transport stop
study_lsoas <- rbind(london_lsoas, stop_buffer_lsoas)%>%
 distinct(lsoa21cd, .keep_all = TRUE)

#2) Origins: pop-weighted centroids
#LSOA pop-weighted centroids: r5r-compatible format
pop_centroids <- read_csv("data/lsoa_pop_weighted_centroids.csv")%>%
  clean_names()%>%
  #st_as_sf(., coords = c("x", "y"), crs = 4326)%>%
  #st_transform(., crs=27700)%>%
  select("lsoa21cd", "x", "y")%>%
  rename("id" = lsoa21cd,
         "lon" = x,
         "lat" = y) %>%
  filter(id %in% study_lsoas$lsoa21cd)

#Hillingdon 001E does not join to the r5r network because it's in a gated community
#Moving it very slightly so it aligns with the next (non-private) road over
pop_centroids <- pop_centroids %>%
  mutate(lon = if_else(id == 'E01002482', -0.410789, lon),
         lat = if_else(id == 'E01002482', 51.61021, lat))

#3) Destinations: workplace-weighted centroids
#The ONS does not release these, so we will estimate using OA-level data

#First, load in OA shapefile
oas <- st_read("data/OA_2021_EW_BFC_V8.shp")%>%
  clean_names() %>%
  select(oa21cd, lsoa21cd, lat, long) #this is a representative centroid which always falls inside the OA; different to a centroid

#Filter for only OAs in study area
oas <- oas %>%
  filter(lsoa21cd %in% study_lsoas$lsoa21cd)

#Load in OA-level working population
working_pop_oa <- read_csv("data/workforce_pop_oa.csv")%>%
  clean_names()%>%
  rename("oa21cd" = output_areas_code, 
         "working_pop" = count)

#Append working population to sf object
oas <- oas %>%
  left_join(., working_pop_oa, by = "oa21cd")

#Find weighted average: r5r-compatible formatting
#We use the provided "representative centroids", but could change these to geometric centroids
workforce_centroids <- oas %>%
  st_drop_geometry()%>%
  group_by(lsoa21cd)%>%
  summarise(
    lat = sum(lat * working_pop)/sum(working_pop),
    lon = sum(long * working_pop)/sum(working_pop))%>%
  rename("id" = lsoa21cd)#%>%
  #st_as_sf(., coords = c("lon", "lat"), crs=4326)%>%
  #st_transform(., 27700)

#Three centroids do not join to the r5r network - manually moving these

#1) Hillingdon 001E - on a gated road
#Moving to the closest non-gated road
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01002482', -0.41118, lon),
         lat = if_else(id == 'E01002482', 51.61036, lat))

#2) Kingston upon Thames 020C - inside Chessington World of Adventures!
#Move coordinates to the park entrance
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01002948', -0.314287, lon),
         lat = if_else(id == 'E01002948', 51.35000, lat))

#3) Hillingdon 031A - inside Heathrow
#Tricky to find an optimal place to move the centroid, because r5r does not support indoor routing
#Let's move to a spot very close to the PT stops and see if that works
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01002444', -0.45215, lon),
         lat = if_else(id == 'E01002444', 51.47121, lat))
#This is the only spot I found which works
#But is likely to overestimate accessibility to jobs as it is so close to the station entrance - in reality, people would have to walk more

#4) Origin Attributes

#Age: proportion under 5 and 65+ (TS007B - Age by broad age bands)
age <- read_csv("data/nomis_age.csv")%>%
  clean_names()%>%
  rename(
    "id" = mnemonic,
    "total_pop" = total,
    "total_under_5" = aged_4_years_and_under)%>%
  mutate(total_65_plus = aged_65_to_74_years + aged_75_to_84_years + aged_85_years_and_over,
         pct_65_plus = round(100*total_65_plus/total_pop, 3),
         pct_under_5 = round(100*total_under_5/total_pop, 3))%>%
  select(id, total_pop, total_under_5, pct_under_5, total_65_plus, pct_65_plus)

#Disability: disabled under equality act (TS038 - Disability)
disability <- read_csv("data/nomis_disability.csv")%>%
  clean_names()%>%
  rename(
    "id" = mnemonic,
    "total_pop" = total_all_usual_residents,
    "total_disabled" = disabled_under_the_equality_act)%>%
  mutate(pct_disabled = round(100*total_disabled/total_pop, 3))%>%
  select(id, total_disabled, pct_disabled)

#Join to pop centroids
pop_centroids <- pop_centroids %>%
  left_join(., age, by="id")%>%
  left_join(., disability, by="id")
  
#5) Destination Attributes: workforce pop
working_pop_lsoa <- read_csv("data/workforce_pop_lsoa.csv")%>%
  clean_names()%>%
  rename("id" = lower_layer_super_output_areas_code, 
         "working_pop" = count)%>%
  select(-lower_layer_super_output_areas_label)

#Join to workforce centroids
workforce_centroids <- workforce_centroids %>%
  left_join(., working_pop_lsoa, by="id")

rm(london_codes, lsoas, oas, working_pop_lsoa, working_pop_oa, age, disability, london_lsoas, stop_buffers, stop_buffer_lsoas, stop_locations)

# -------- Download street network --------
 
# #Union study area, so we can clip it to one boundary
# study_boundary <- study_lsoas %>% st_union()
# 
# #Increase timeout
# old_timeout <- getOption("timeout")
# options(timeout = 3000)
#
# osm_path <- oe_get("England",
#                    #boundary = study_boundary,
#                    provider = "geofabrik",
#                    download_directory = "large_data2",
#                    download_only = TRUE)
# 
# options(timeout = old_timeout)
# rm(old_timeout, study_boundary)

#The above isn't working due to some problems with tags!
#Trying a manual download from HOT Export Tool instead

# -------- Basic r5r query -----------

#Set up r5r network
r5r_core <- setup_r5(data_path = "large_data", verbose=TRUE)
#There are some "invalid turn restriction" errors but nothing too serious
#Note that Heathrow stops are not reachable by foot, but are by PT
#Some stops and centroids had to be manually moved to make them reachable via the street/PT network (see above)

#Test!
sample_origin <- workforce_centroids %>%
  filter(id == 'E01000001')
test_ttm <- travel_time_matrix(r5r_core,
                               sample_origin,
                               workforce_centroids,
                               mode = c("WALK", "TRANSIT"),
                               max_trip_duration = 5000L)

#Use accessibility function
#see how long each takes - then add to for loop?
# - trams classed as 0 - check whether these are included in prompt
# - make sure RRS are not running?!

# To do:
# - GTFS calendar looks wrong! Quick check to see if I am being silly? Otherwise could TfL API help?
# - New r5r_core with street network for larger area
# - Follow up re Overground
# - When OSM is sorted, add to README which day it is from

#Basic vis I will need:
# - public transport network
# - public transport accessible network
# - disability distribution
# - workplace pop dist (autocorrelation?)