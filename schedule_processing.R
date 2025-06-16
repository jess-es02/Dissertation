#Building the multimodal GTFS object

#In this file, we:
  # - Convert the Traveline dataset for the entirety of London from TransXChange to GTFS
  # - Use TfL topology data to match each NAPTAN ID to its platform ID (for joining with accessibility data)
  # - Merge the resulting object with the Elizabeth Line and Overground GTFS files
  # - Process TfL topology data to create a pathways.txt file reflecting wheelchair accessibility

#Note that for memory reasons, the final exported GTFS object excludes weekends
#Also note that the wheelchair accessibility only reflects accessibility to the platform - not necessarily to the train (I could extend this if I have time)

library(tidyverse)
library(UK2GTFS)
library(tidytransit)
library(gtfstools)
library(janitor)
library(sf)
library(tmap)
library(tmaptools)
library(httr)
library(jsonlite)
options(java.parameters = "-Xmx2G")
library(r5r)
library(igraph)
library(data.table)

# ------ Build base GTFS network ---------

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
#We add an option to get stop information without coordinates, for calling later
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

#Filter out unwanted buses:
#DBSE, BM, and SS are non-TfL (coaches/buses to Heathrow)
#Cable car is currently classed as a bus
gtfs$routes <- gtfs$routes %>%
  filter(!agency_id %in% c("CAB", "DBSE", "BM", "SS"))
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

#Brief look into "duplicate" routes
duplicate_routes <- gtfs$routes %>%
  group_by(route_long_name, route_short_name) %>%
  filter(n() > 1) %>%
  ungroup()
duplicate_routes %>% distinct(route_id) %>% nrow() #each is a different route, hopefully should be fine

#Manually correcting incorrect route information

#Sort 'Willow Lawn (Ruislip Lido Railway)' - it has the wrong ID
#Change ID to 9400ZZRLWLN
gtfs$stops <- gtfs$stops %>%
  mutate(stop_id = if_else(stop_id == '9400ZZLUHPC2', '9400ZZRLWLN', stop_id))
#Create a duplicate of Hyde Park Corner with 9400ZZLUHPC2
hpc_row <- data.frame(stop_id = '9400ZZLUHPC2', stop_code = NA, stop_name = 'Hyde Park Corner Underground Station', stop_lon = -0.153129, stop_lat = 51.50278)
gtfs$stops <- rbind(gtfs$stops, hpc_row)

#The NAPTAN dataset contains two Oakhill Roads with the same stop ID in different locations
#Oakhill Road Sutton should have different coordinates - the TfL ID corresponds to a different NAPTAN code
#490002334ZT should be on the S2, rather than 490013611E at present
#490002334ZT is not in gtfs$stops
#490013611E is in the same place as 490010487E, which is a functioning stop for the 255
#So we will update the coordinates for 490013611E to reflect stop 490002334ZT, recognising that this does not fully match the NAPTAN data
gtfs$stops <- gtfs$stops %>%
  mutate(stop_lon = if_else(stop_id == '490013611E', -0.19446, stop_lon),
         stop_lat = if_else(stop_id == '490013611E', 51.37127, stop_lat))

#There are still unrealistic travel times between some consecutive stops
#But the stops seem to be in the right location - it is a limitation of the timetabling
#e.g. stop 1: 10:59:59 -> stop 2: 11:00:00

#Moving some stops as setup_r5r was not joining them to the network
#1) Barking stops were excluded because they are closer to a private walkway - let's move them nearer the public-facing footpath
barking_stops <- c("9400ZZLUBKG3", "9400ZZLUBKG2", "9400ZZLUBKG1")
gtfs$stops <- gtfs$stops %>%
  mutate(stop_lon = if_else(stop_id %in% barking_stops, 0.081114, stop_lon),
         stop_lat = if_else(stop_id %in% barking_stops, 51.53926, stop_lat))
rm(barking_stops)
#This obviously changes the distance that would be traversed between them, but it seems negligible (0.3 min previously versus 0 now)

#2) Upney Underground station looks too far from a walkway - let's move it closer to the road (the only entrance is here anyway)
gtfs$stops <- gtfs$stops %>%
  mutate(stop_lon = if_else(stop_id == '9400ZZLUUPY1', 0.1014915, stop_lon),
         stop_lat = if_else(stop_id == '9400ZZLUUPY1', 51.53846, stop_lat))

#3) Not sure why Christchurch Road won't connect - but let's move it slightly closer to the main road to see if that works
gtfs$stops <- gtfs$stops %>%
  mutate(stop_lon = if_else(stop_id == '490005233W', 0.0973208, stop_lon),
         stop_lat = if_else(stop_id == '490005233W', 51.42659, stop_lat))

rm(path, missing_stop_ids, extra_stop_ids, hpc_row)

# ------ Join stops with platform IDs -------
#We need to do this to be able to join with the National Rail GTFS files and TfL accessibility data

#First, create lookup table between GTFS IDs and TfL stop IDs
#This isn't used later in the script but was important for manual checks and corrections

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
rm(tube_trips, stops_on_tube_trips)

#Load platforms
platforms <- read_csv("data/tfl_station_data_detailed/Platforms.csv") %>%
  clean_names() %>%
  select(unique_id, station_unique_id, platform_number, cardinal_direction, platform_naptan_code) #%>%

#Lots of platform_naptan_codes are null - let's check that it's just for non-tube lines only
platforms <- platforms %>%
  mutate(mode = str_extract(unique_id, "[^-]+$"))
view(platforms %>% 
       filter(is.na(platform_naptan_code))%>%
       distinct(mode))
#Manually append missing NAPTANs for London Underground stops
platforms <- platforms %>%
  mutate(
    platform_naptan_code = case_when(
      unique_id == '940GZZLUALD-Plat02-NB-metropolitan'  ~ '9400ZZLUALD1',
      unique_id == '940GZZLUALD-Plat01-EB-circle'  ~ '9400ZZLUALD2',
      unique_id == '940GZZLUALD-Plat04-WB-circle' ~ '9400ZZLUALD3',
      unique_id == '940GZZLUALD-Plat03-NB-metropolitan' ~ '9400ZZLUALD4',
      unique_id == '940GZZLUASG-Plat02-EB-piccadilly' ~ '9400ZZLUASG2',
      unique_id == '940GZZLUASG-Plat03-WB-piccadilly' ~ '9400ZZLUASG3',
      unique_id == '940GZZLUBWT-Plat01-WB-circle|district' ~ '9400ZZLUBWT2',
      unique_id == '940GZZLUBWT-Plat02-EB-circle|district' ~ '9400ZZLUBWT1',
      unique_id == '940GZZLUHSK-Plat01-WB-circle|district' ~ '9400ZZLUHSK2',
      unique_id == '940GZZLUHSK-Plat02-EB-circle|district' ~ '9400ZZLUHSK1',
      unique_id == '940GZZLUNHG-Plat04-WB-central' ~ '9400ZZLUNHG2',
      unique_id == '940GZZLUNHG-Plat03-EB-central' ~ '9400ZZLUNHG1',
      unique_id == '940GZZLUNHG-Plat01-WB-circle|district' ~ '9400ZZLUNHG4',
      unique_id == '940GZZLUNHG-Plat02-EB-circle|district' ~ '9400ZZLUNHG3',
      unique_id == '940GZZLUEPG-Plat01-WB-central' ~ '9400ZZLUEPG1',
      unique_id == '940GZZLUNAN-Plat01-WB-central' ~ '9400ZZLUNAN2',
      unique_id == '940GZZLUNAN-Plat03-EB-central' ~ '9400ZZLUNAN1',
      unique_id == 'HUBEAL-Plat05-EB-central' ~ '9400ZZLUEBY1',
      unique_id == 'HUBEAL-Plat05-EB-central' ~ '9400ZZLUEBY4',
      unique_id == '940GZZLUMDN-Plat02-NB-northern' ~ '9400ZZLUMDN2',
      unique_id == '940GZZLUMPK-Plat02-SB-metropolitan' ~ '9400ZZLUMPK2',
      unique_id == '940GZZLUMPK-Plat01-NB-metropolitan' ~ '9400ZZLUMPK1',
      unique_id == '940GZZLUMPK-Plat03-NB-metropolitan' ~ '9400ZZLUMPK3',
      unique_id == 'HUBAMR-Plat02-SB-metropolitan' ~ '9400ZZLUAMS2',
      unique_id == 'HUBAMR-Plat03-SB-metropolitan' ~ '9400ZZLUAMS1',
      unique_id == '940GZZLURYO-Plat01-WB-circle|hammersmith-city' ~ '9400ZZLURYO2',
      unique_id == '940GZZLURYO-Plat02-EB-circle|hammersmith-city' ~ '9400ZZLURYO1',
      unique_id == '940GZZLUUXB-Plat01-EB-metropolitan|piccadilly' ~ '9400ZZLUUXB1',
      unique_id == 'HUBKPA-Plat01-EB-district' ~ '9400ZZLUKOY1',
      unique_id == 'HUBH13-Plat01-WB-piccadilly' ~ '9400ZZLUHRC2',
      unique_id == 'HUBH13-Plat02-EB-piccadilly' ~ '9400ZZLUHRC1',
      TRUE ~ platform_naptan_code))
#Arnos Grove 1 and 4 will be left in platforms (Picadilly)
#High St Ken 3 and 4 will be left in platforms (Circle|District)
#Epping 2, North Acton 2, Ealing 6, and White City 2 and 3 will be left in platforms (Central)
#One platform picked at random for Morden (all accessible anyway)
#Moor Park 4, Amersham 1 left in platforms (Met)
#Uxbridge picked one at random (doesn't matter as all accessible)

#Manually remove NAPTANs for non-tube modes in case it complicates joining
platforms <- platforms %>%
  mutate(platform_naptan_code = if_else(mode == 'dlr' & !is.na(platform_naptan_code),
                                        NA_character_,
                                        platform_naptan_code))
#Remove 9100STFD4 as it's not in the main NAPTAN CSV
platforms <- platforms %>%
  mutate(platform_naptan_code = if_else(platform_naptan_code == '9100STFD4',
                                        NA_character_,
                                        platform_naptan_code))

#Look for duplicate NAPTAN codes
#We can join multiple platforms to the same NAPTAN, but not vice versa
view(platforms %>%
      filter(!is.na(platform_naptan_code)) %>%
      count(platform_naptan_code) %>%
      filter(n > 1))
#There are lots missing :( - manually fixing these
platforms <- platforms %>%
  mutate(
    platform_naptan_code = case_when(
      unique_id == '940GZZLUACY-Plat02-SB-northern'  ~ '9400ZZLUACY2',
      unique_id == '940GZZLUAGL-Plat01-SB-northern' ~ '9400ZZLUAGL2',
      unique_id == '940GZZLUBDS-Plat01-EB-piccadilly' ~ '9400ZZLUBDS2',
      unique_id == '940GZZLUBLG-Plat01-WB-central' ~ '9400ZZLUBLG2',
      unique_id == 'HUBBHO-Plat01-NB-victoria' ~ '9400ZZLUBLR2',
      unique_id == 'HUBBDS-Plat01-WB-central' ~ '9400ZZLUBND4',
      unique_id == '940GZZLUBTK-Plat02-SB-northern' ~ '9400ZZLUBTK2',
      unique_id == '940GZZLUBTX-Plat02-SB-northern' ~ '9400ZZLUBTX2',
      unique_id == '940GZZLUCAR-Plat01-EB-piccadilly' ~ '9400ZZLUCAR2',
      unique_id == '940GZZLUCFM-Plat02-SB-northern' ~ '9400ZZLUCFM2',
      unique_id == 'HUBCHX-Plat02-SB-bakerloo' ~ '9400ZZLUCHX2',
      unique_id == '940GZZLUCND-Plat02-SB-northern' ~ '9400ZZLUCND2',
      unique_id == '940GZZLUCPC-Plat02-SB-northern' ~ '9400ZZLUCPC2',
      unique_id == '940GZZLUCPN-Plat02-SB-northern' ~ '9400ZZLUCPN2',
      unique_id == '940GZZLUCPS-Plat02-SB-northern' ~ '9400ZZLUCPS2',
      unique_id == '940GZZLUCSD-Plat02-SB-northern' ~ '9400ZZLUCSD2',
      unique_id == 'HUBCAW-Plat01-WB-jubilee' ~ '9400ZZLUCYF2',
      unique_id == '940GZZLUDOH-Plat02-SB-jubilee' ~ '9400ZZLUDOH2',
      unique_id == '940GZZLUEPK-Plat01-WB-district' ~ '9400ZZLUEPK2',
      unique_id == '940GZZLUERC-Plat01-EB-circle|hammersmith-city' ~ '9400ZZLUERC1',
      unique_id == 'HUBGUN-Plat01-WB-district|london-overground' ~ '9400ZZLUGBY2',
      unique_id == '940GZZLUGDG-Plat02-SB-northern' ~ '9400ZZLUGDG2',
      unique_id == '940GZZLUGPK-Plat01-WB-piccadilly' ~ '9400ZZLUGPK6',
      unique_id == '940GZZLUGTH-Plat01-WB-central' ~ '9400ZZLUGTH2',
      unique_id == '940GZZLUHCL-Plat02-SB-northern' ~ '9400ZZLUHCL2',
      unique_id == '940GZZLUHGR-Plat01-WB-central' ~ '9400ZZLUHGR2',
      unique_id == '940GZZLUHGT-Plat02-SB-northern' ~ '9400ZZLUHGT2',
      unique_id == '940GZZLUHNX-Plat02-EB-piccadilly' ~ '9400ZZLUHNX2',
      unique_id == '940GZZLUHWC-Plat02-EB-piccadilly' ~ '9400ZZLUHWC2',
      unique_id == '940GZZLUHWT-Plat02-EB-piccadilly' ~ '9400ZZLUHWT2',
      unique_id == '940GZZLUHWY-Plat01-WB-piccadilly' ~ '9400ZZLUHWY2', 
      unique_id == '940GZZLUKPK-Plat02-SB-bakerloo' ~ '9400ZZLUKPK2',
      unique_id == 'HUBKGX-Plat02-EB-circle|hammersmith-city|metropolitan' ~ '9400ZZLUKSX8',
      unique_id == '940GZZLULBN-Plat02-SB-bakerloo' ~ '9400ZZLULBN2',
      unique_id == '940GZZLULGT-Plat01-WB-central' ~ '9400ZZLULGT2',
      unique_id == 'HUBLBG-Plat03-WB-jubilee' ~ '9400ZZLULNB3',
      unique_id == 'HUBLBG-Plat02-SB-northern' ~ '9400ZZLULNB4',
      unique_id == '940GZZLUMBA-Plat02-EB-central' ~ '9400ZZLUMBA2',
      unique_id == '940GZZLUMRH-Plat02-WB-piccadilly' ~ '9400ZZLUMRH2',
      unique_id == '940GZZLUMTC-Plat02-SB-northern' ~ '9400ZZLUMTC2',
      unique_id == '940GZZLUMVL-Plat02-SB-bakerloo'~ '9400ZZLUMVL2',
      unique_id == '940GZZLUNDN-Plat03-SB-jubilee' ~ '9400ZZLUNDN2',
      unique_id == '940GZZLUNHT-Plat01-WB-central' ~ '9400ZZLUNHT2',
      unique_id == '940GZZLUNKP-Plat02-SB-metropolitan' ~ '9400ZZLUNKP2',
      unique_id == '940GZZLUOAK-Plat02-EB-piccadilly' ~ '9400ZZLUOAK2',
      unique_id == 'HUBOLD-Plat02-SB-northern' ~ '9400ZZLUODS2',
      unique_id == '940GZZLUPCO-Plat02-SB-victoria' ~ '9400ZZLUPCO2',
      unique_id == '940GZZLUPRD-Plat02-SB-metropolitan' ~ '9400ZZLUPRD2',
      unique_id == '940GZZLUPVL-Plat01-WB-central' ~ '9400ZZLUPVL2',
      unique_id == '940GZZLUQWY-Plat01-WB-central' ~ '9400ZZLUQWY2',
      unique_id == '940GZZLURBG-Plat01-WB-central' ~ '9400ZZLURBG2',
      unique_id == '940GZZLURSG-Plat01-WB-central' ~ '9400ZZLURSG2',
      unique_id == 'HUBSPB-Plat01-WB-central' ~ '9400ZZLUSBC2',
      unique_id == '940GZZLUSFS-Plat02-WB-district' ~ '9400ZZLUSFS2',
      unique_id == '940GZZLUSGT-Plat02-WB-piccadilly' ~ '9400ZZLUSGT2',
      unique_id == '940GZZLUSJW-Plat02-SB-jubilee' ~ '9400ZZLUSJW2',
      unique_id == 'HUBSOK-Plat01-SB-bakerloo|london-overground' ~ '9400ZZLUSKT2',
      unique_id == 'HUBSRU-Plat01-WB-central' ~ '9400ZZLUSRP2',
      unique_id == '940GZZLUSWC-Plat02-SB-jubilee' ~ '9400ZZLUSWC2',
      unique_id == '940GZZLUSWN-Plat02-SB-northern' ~ '9400ZZLUSWN2',
      unique_id == '940GZZLUTBC-Plat02-SB-northern' ~ '9400ZZLUTBC2',
      unique_id == '940GZZLUTBY-Plat02-SB-northern' ~ '9400ZZLUTBY2',
      unique_id == 'HUBTOM-Plat02-SB-victoria' ~ '9400ZZLUTMH2',
      unique_id == '940GZZLUTWH-Plat02-WB-circle|district' ~ '9400ZZLUTWH3',
      unique_id == '940GZZLUUPB-Plat01-WB-district' ~ '9400ZZLUUPB2',
      unique_id == '940GZZLUUPY-Plat01-WB-district' ~ '9400ZZLUUPY2',
      unique_id == 'HUBVXH-Plat02-SB-victoria' ~ '9400ZZLUVXL2',
      unique_id == 'HUBWEH-Plat01-WB-district|hammersmith-city' ~ '9400ZZLUWHM3',
      unique_id == 'HUBWEH-Plat05-WB-jubilee' ~ '9400ZZLUWHM4',
      unique_id == '940GZZLUWHP-Plat01-NB-jubilee' ~ '9400ZZLUWHP2',
      unique_id == '940GZZLUWIP-Plat01-EB-district' ~ '9400ZZLUWIP2',
      unique_id == 'HUBWIJ-Plat01-SB-bakerloo|london-overground' ~ '9400ZZLUWJN2',
      unique_id == '940GZZLUWKA-Plat01-NB-bakerloo' ~ '9400ZZLUWKA2',
      unique_id == 'HUBWAT-Plat02-SB-northern' ~ '9400ZZLUWLO7',
      unique_id == 'HUBWAT-Plat05-WB-jubilee' ~ '9400ZZLUWLO8',
      unique_id == '940GZZLUWOG-Plat02-WB-piccadilly' ~ '9400ZZLUWOG2',
      unique_id == '940GZZLUWSD-Plat01-WB-central' ~ '9400ZZLUWSD2',
      unique_id == '940GZZLUWIG-Plat03-SB-jubilee' ~ '9400ZZLUWIG2',
      TRUE ~ platform_naptan_code))
#Filtering out individual platforms - only doing this if it won't affect accessibility classification (or if platform is very infrequently used)
platforms_to_remove <- c("940GZZLUBST-Plat02-NB-metropolitan", "940GZZLUBST-Plat04-NB-metropolitan",
                         "HUBBAL-Plat02-SB-northern", "HUBBRX-Plat02-NB-victoria", "HUBCFO-Plat03-WB-metropolitan",
                         "940GZZLUCKS-Plat02-WB-piccadilly", "940GZZLUCKS-Plat03-WB-piccadilly", "940GZZLUCKS-Plat04-WB-piccadilly",
                         "940GZZLUDGE-Plat03-WB-district", "HUBEAL-Plat08-EB-district", "HUBEAL-Plat09-EB-district",
                         "940GZZLUECT-Plat02-EB-district", "940GZZLUECT-Plat04-WB-district",
                         "940GZZLUEFY-Plat02-NB-northern", "940GZZLUEFY-Plat04-SB-northern",
                         "940GZZLUEGW-Plat03-SB-northern", "940GZZLUFYC-Plat02-NB-northern",
                         "940GZZLUGGN-Plat01-NB-northern", "940GZZLUGGN-Plat03-NB-northern", "940GZZLUGGN-Plat04-SB-northern",
                         "940GZZLUHBT-Plat02-SB-northern", "940GZZLUHBT-Plat03-SB-northern",
                         "940GZZLUHLT-Plat03-WB-central",
                         "HUBHOH-Plat06-SB-metropolitan", "HUBHOH-Plat04-NB-metropolitan", "HUBHOH-Plat03-NB-metropolitan",
                         "HUBHX5-Plat06-EB-piccadilly",
                         "HUBHMS-Plat02-EB-circle|hammersmith-city", "HUBHMS-Plat03-EB-circle|hammersmith-city",
                         "940GZZLUKNG-Plat03-NB-northern", "940GZZLUKNG-Plat04-SB-northern",
                         "940GZZLULGN-Plat02-WB-central", "940GZZLULGN-Plat03-WB-central", 
                         "940GZZLULYS-Plat02-WB-central",
                         "HUBZMG-Plat03-WB-circle|hammersmith-city|metropolitan", "HUBZMG-Plat04-WB-circle|hammersmith-city|metropolitan",
                         "940GZZLUNFD-Plat02-WB-piccadilly", "940GZZLUNFD-Plat04-EB-piccadilly",
                         "940GZZLUNGW-Plat02-WB-jubilee", "940GZZLUPLW-Plat03-WB-district|hammersmith-city",
                         "HUBQPW-Plat01-SB-bakerloo|london-overground", "HUBQPW-Plat04-NB-bakerloo|london-overground",
                         "HUBRMD-Plat03-EB-district|london-overground", "HUBRMD-Plat04-EB-district|london-overground", "HUBRMD-Plat05-EB-district|london-overground", "HUBRMD-Plat07-EB-district",
                         "940GZZLUSEA-Plat02-EB-piccadilly", "940GZZLUSEA-Plat04-WB-piccadilly",
                         "HUBSRA-Plat14-WB-jubilee", "HUBSRA-Plat15-WB-jubilee",
                         "940GZZLUSTM-Plat02-SB-jubilee", "940GZZLUSTM-Plat03-SB-jubilee",
                         "HUBSVS-Plat04-NB-victoria",
                         "940GZZLUTPN-Plat02-WB-piccadilly", "940GZZLUTPN-Plat01-EB-piccadilly",
                         "HUBUPM-Plat05-WB-district", "940GZZLUWAF-Plat02-SB-metropolitan",
                         "HUBWIM-Plat02-EB-district", "HUBWIM-Plat03-EB-district", "HUBWIM-Plat04-EB-district",
                         "940GZZLUWOF-Plat01-WB-central", "HUBWRU-Plat02-EB-central",
                         "HUBWHC-Plat02-SB-victoria", 
                         "940GZZLUWIG-Plat01-NB-metropolitan", "940GZZLUWIG-Plat04-SB-metropolitan")
platforms <- platforms %>%
  filter(!unique_id %in% platforms_to_remove)

#Check for any NAPTAN codes in platforms which aren't in gtfs$stops
missing_platforms <- platforms %>%
  filter(!(platform_naptan_code %in% gtfs$stops$stop_id) &
           !(mode %in% c("car", "dlr", "elizabeth", "thameslink", "overground", "rail", "tram")))
#Mostly just match with the platforms I purposefully didn't join above due to redundancy
#One small change:
platforms <- platforms %>%
  mutate(platform_naptan_code = if_else(platform_naptan_code == '940GZZLUWHM1', '9400ZZLUWHM1', platform_naptan_code))
#Platform-specific stops for Battersea Power Station and Nine Elms are missing from GTFS stops
#As both stations are fully accessible, we won't decompose by platform and will instead just assign the entire station to one of them
gtfs$stops <- gtfs$stops %>%
  mutate(stop_id = if_else(stop_id == '9400ZZBPSUST', '9400ZZBPSUST1', stop_id))
gtfs$stop_times <- gtfs$stop_times %>%
  mutate(stop_id = if_else(stop_id == '9400ZZBPSUST', '9400ZZBPSUST1', stop_id))
gtfs$stops <- gtfs$stops %>%
  mutate(stop_id = if_else(stop_id == '9400ZZNEUGST', '9400ZZNEUGST1', stop_id))
gtfs$stop_times <- gtfs$stop_times %>%
  mutate(stop_id = if_else(stop_id == '9400ZZNEUGST', '9400ZZNEUGST1', stop_id))

#Check all tube stops included
#Update the tube_stops vector to have the new Battersea Power Station/Nine Elms IDs
tube_stops[tube_stops == "9400ZZBPSUST"] <- "9400ZZBPSUST1"
tube_stops[tube_stops == "9400ZZNEUGST"] <- "9400ZZNEUGST1"
missing_stops <- tube_stops[!(tube_stops %in% platforms$platform_naptan_code)]
#Ruislip has one stop ID per line but these are at the same platform - combining to have IDs in same direction
gtfs$stop_times <- gtfs$stop_times %>%
  mutate(stop_id = if_else(stop_id == '9400ZZLURSP1', '9400ZZLURSP3', stop_id))
gtfs$stops <- gtfs$stops %>%
  filter(!stop_id == '9400ZZLURSP1')
gtfs$stop_times <- gtfs$stop_times %>%
  mutate(stop_id = if_else(stop_id == '9400ZZLURSP2', '9400ZZLURSP4', stop_id))
gtfs$stops <- gtfs$stops %>%
  filter(!stop_id == '9400ZZLURSP2')
#Turnpike Lane is missing from platform data
platforms <- add_row(platforms, unique_id = "940GZZLUTPN-Plat01-EB-piccadilly", platform_naptan_code = "9400ZZLUTPN1")
platforms <- add_row(platforms, unique_id = "940GZZLUTPN-Plat02-WB-piccadilly", platform_naptan_code = "9400ZZLUTPN2")
#Balham southbound is missing from platform data
platforms <- add_row(platforms, unique_id = "HUBBAL-Plat02-SB-northern", platform_naptan_code = "9400ZZLUBLM2")
#Combine Ealing Broadway District line
gtfs$stop_times <- gtfs$stop_times %>%
  mutate(stop_id = if_else(stop_id == '9400ZZLUEBY4', '9400ZZLUEBY3', stop_id))
gtfs$stops <- gtfs$stops %>%
  filter(!stop_id == '9400ZZLUEBY4')
#Manually add platform IDs for Harlesden
platforms <- platforms %>%
  mutate(
    platform_naptan_code = case_when(
      unique_id == 'HUBHDN-Plat01-SB-bakerloo|london-overground'  ~ '9400ZZLUHSN1',
      unique_id == 'HUBHDN-Plat02-NB-bakerloo|london-overground' ~ '9400ZZLUHSN2',
      TRUE ~ platform_naptan_code))

#Join platform IDs to stops - ensure to only alter tube stops
gtfs_stops <- gtfs$stops
gtfs_stops <- gtfs_stops %>%
  left_join(., platforms, by = c("stop_id" = "platform_naptan_code"))%>%
  select(-platform_number, -cardinal_direction, -mode)
gtfs_stops %>% filter(!is.na(unique_id))%>%nrow() #629 - correct, as we removed 3 stops from tube_stops
#For tube stops, have the ID as platform_id, if not, keep NAPTAN ID
gtfs_stops <- gtfs_stops %>%
  mutate(unique_id = if_else(is.na(unique_id), stop_id, unique_id),
         stop_code = if_else(is.na(stop_code) | stop_code == "", station_unique_id, stop_code))
#Manually add stop_code for Turnpike Lane
turnpike_ids <- c("9400ZZLUTPN1", "9400ZZLUTPN2")
gtfs_stops <- gtfs_stops %>%
  mutate(stop_code = if_else(stop_id %in% turnpike_ids, '940GZZLUTPN', stop_code))
#And to Balham
gtfs_stops <- gtfs_stops %>%
  mutate(stop_code = if_else(stop_id == 'HUBBAL-Plat02-SB-northern', 'HUBBAL', stop_code))

#Update GTFS stop_times using new platform IDs
gtfs_stop_times <- gtfs$stop_times
gtfs_stop_times <- gtfs_stop_times %>%
  left_join(gtfs_stops %>% select(stop_id, unique_id), by = "stop_id")%>%
  mutate(stop_id = unique_id) %>%
  select(-unique_id)

#Reformat GTFS stops
gtfs_stops <- gtfs_stops %>%
  mutate(stop_id = unique_id) %>%
  select(-unique_id, -station_unique_id)

#Reintegrate both back into GTFS file
gtfs$stops <- gtfs_stops
gtfs$stop_times <- gtfs_stop_times

# #Check GTFS object - all looks good
# output_path <- tempfile("validation_result")
# validator_path <- download_validator(tempdir())
# gtfstools::validate_gtfs(gtfs, output_path, validator_path)

#Write object locally
gtfs_write(gtfs, folder = "large_data", name = "gtfs_london")

#Clean workspace - keep platforms as a lookup in case it's needed later
#write.csv(platforms, "data/platforms_naptans_joined.csv", row.names = FALSE)
rm(tube_stops, gtfs_stops, gtfs_stop_times, id_lookup, missing_platforms, pathways, platforms, tfl_stops, api_key, missing_stops, platforms_to_remove, tube_stops, turnpike_ids, get_stop_id, gtfs)

# ------ Merge all GTFS files into one -------

#Load in created networks
gtfs_london <- gtfstools::read_gtfs("large_data/gtfs_london.zip")
gtfs_london$stops <- gtfs_london$stops %>%
  mutate(stop_lon = as.numeric(stop_lon))
summary(gtfs_london)

gtfs_overground <- gtfstools::read_gtfs("large_data/gtfs_overground.zip")
summary(gtfs_overground)

gtfs_lizzie <- gtfstools::read_gtfs("large_data/gtfs_elizabeth_line.zip")
summary(gtfs_lizzie)

#Align formatting
#If a stop_code in Lizzie/Overground is the same as in the overall London GTFS, ensure the stop name is the same as the London dataset
london_stops <- gtfs_london$stops %>%
  distinct(stop_code, .keep_all = TRUE)%>%
  select(stop_code, london_stop_name = stop_name, london_lon = stop_lon, london_lat = stop_lat)
gtfs_overground$stops <- gtfs_overground$stops %>%
  left_join(london_stops, by = "stop_code") %>%
  mutate(stop_name = coalesce(london_stop_name, stop_name)) %>%
  select(-london_stop_name, -london_lon, -london_lat)
gtfs_lizzie$stops <- gtfs_lizzie$stops %>%
  left_join(london_stops, by = "stop_code") %>%
  mutate(stop_name = coalesce(london_stop_name, stop_name)) %>%
  select(-london_stop_name, -london_lon, -london_lat)
#If a platform in Lizzie/Overground is the same as in the overall London GTFS, ensure the stop lon/lat are the same as the London dataset
london_stops <- gtfs_london$stops %>%
  select(stop_id, london_lon = stop_lon, london_lat = stop_lat)
gtfs_overground$stops <- gtfs_overground$stops %>%
  left_join(london_stops, by = "stop_id") %>%
  mutate(stop_lon = coalesce(london_lon, stop_lon),
         stop_lat = coalesce(london_lat, stop_lat))%>%
  select(-london_lon, -london_lat)
gtfs_lizzie$stops <- gtfs_lizzie$stops %>%
  left_join(london_stops, by = "stop_id") %>%
  mutate(stop_lon = coalesce(london_lon, stop_lon),
         stop_lat = coalesce(london_lat, stop_lat))%>%
  select(-london_lon, -london_lat)

#Merge files
final_gtfs <- merge_gtfs(gtfs_london, gtfs_overground, gtfs_lizzie)
summary(final_gtfs)

#Remove duplicate entries
final_gtfs$agency <- final_gtfs$agency %>%
  filter(!agency_name == 'London Overground')
final_gtfs$stops <- final_gtfs$stops %>%
  distinct(stop_id, .keep_all = TRUE)

#Filter out weekends to reduce size
final_gtfs_no_weekends <- filter_by_weekday(final_gtfs, 
                                            weekday = c("saturday", "sunday"), 
                                            keep=FALSE)

#Check validity
output_path <- tempfile("validation_result")
validator_path <- download_validator(tempdir())
gtfstools::validate_gtfs(final_gtfs_no_weekends, output_path, validator_path)

#Export to a new folder, for r5r core
dir.create("final_r5r")
gtfs_write(final_gtfs_no_weekends, folder = "final_r5r", name = "gtfs")

rm(final_gtfs, gtfs_lizzie, gtfs_london, gtfs_overground, london_stops, output_path, validator_path, final_gtfs_no_weekends)

# -------- Adding accessibility information -------

gtfs <- gtfstools::read_gtfs("final_r5r/gtfs.zip")
summary(gtfs)

#Loading TfL accessibility files
stops <- read_csv("data/tfl_station_data/stops.txt")%>%
  clean_names()
pathways <- read_csv("data/tfl_station_data/pathways.txt")%>%
  clean_names()

#Build graph from pathways.txt
edges <- pathways %>%
  rename("from" = from_stop_id, 
         "to" = to_stop_id) %>%
  select(from, to, is_bidirectional)
all_edges <- bind_rows(
  edges,
  edges %>% filter(is_bidirectional == 1) %>%
    transmute(from = to, to = from, is_bidirectional))
G <- graph_from_data_frame(all_edges, directed = TRUE)

#First, let's check whether there's a path between station entrances and platforms

#Extract entrances and platforms, and join these together
entrances <- stops %>% 
  filter(str_starts(stop_name, "Outside"))%>%
  select(stop_id, parent_station)
platforms <- stops %>%
  filter(location_type == 0) %>%
  select(stop_id, parent_station)

platform_entrance_pairs <- entrances %>%
  inner_join(platforms, by = "parent_station", suffix = c("_entrance", "_platform"))

#Filter for only pairs where both are in the graph
valid_pairs <- platform_entrance_pairs %>%
  filter(stop_id_entrance %in% V(G)$name,
         stop_id_platform %in% V(G)$name)

unique_entrances <- unique(valid_pairs$stop_id_entrance)
unique_platforms <- unique(valid_pairs$stop_id_platform)

#Calculate distances between entrances and platforms
dist_matrix_entrance_to_platform <- distances(G, v = unique_entrances, to = unique_platforms)
dist_matrix_platform_to_entrance <- distances(G, v = unique_platforms, to = unique_entrances)

#Extract the distance for each pair (ChatGPT helped with indexing due to duplicates)
entrance_idx <- setNames(seq_along(unique_entrances), unique_entrances)
platform_idx <- setNames(seq_along(unique_platforms), unique_platforms)
valid_pairs <- valid_pairs %>%
  mutate(
    dist_entrance_to_platform = dist_matrix_entrance_to_platform[cbind(
      entrance_idx[stop_id_entrance], 
      platform_idx[stop_id_platform])],
    dist_platform_to_entrance = dist_matrix_platform_to_entrance[cbind(
      platform_idx[stop_id_platform], 
      entrance_idx[stop_id_entrance])]
  )

#Reformat for integration into pathways.txt
valid_pairs <- valid_pairs %>%
  mutate(dist_entrance_to_platform = if_else(dist_entrance_to_platform == Inf, NA, dist_entrance_to_platform),
         dist_platform_to_entrance = if_else(dist_platform_to_entrance == Inf, NA, dist_platform_to_entrance))
accessible_paths <- valid_pairs %>%
  filter(!is.na(dist_entrance_to_platform) | !is.na(dist_platform_to_entrance)) %>%
  mutate(
    is_bidirectional = case_when(
      !is.na(dist_entrance_to_platform) & !is.na(dist_platform_to_entrance) ~ 1,
      TRUE ~ 0),
    from_stop_id = case_when(
      is_bidirectional == 1 ~ stop_id_entrance,
      !is.na(dist_entrance_to_platform) ~ stop_id_entrance,
      TRUE ~ stop_id_platform),
    to_stop_id = case_when(
      is_bidirectional == 1 ~ stop_id_platform,
      !is.na(dist_entrance_to_platform) ~ stop_id_platform,
      TRUE ~ stop_id_entrance)) %>%
  select(from_stop_id, to_stop_id, is_bidirectional)

#Now, let's check whether there is a path between individual platforms in the same station
platform_pairs <- platforms %>%
  group_by(parent_station) %>%
  filter(n() >= 2) %>%  #remove any stations which don't have multiple platforms
  summarise(pairs = list(as.data.frame(t(combn(stop_id, 2)))), .groups = "drop") %>% #generate all potential pairs
  unnest(cols = pairs) %>%
  rename("stop_id_platform_1" = V1, 
         "stop_id_platform_2" = V2)

#Make sure both are in the accessible graph
valid_platform_pairs <- platform_pairs %>%
  filter(stop_id_platform_1 %in% V(G)$name,
         stop_id_platform_2 %in% V(G)$name)

#Remove duplicate platforms
unique_platforms_in_pairs <- unique(c(valid_platform_pairs$stop_id_platform_1, valid_platform_pairs$stop_id_platform_2))

#Calculate distances
dist_matrix_platforms <- distances(G, v = unique_platforms_in_pairs, to = unique_platforms_in_pairs)

#Extract the distance for each pair
platform_pairs_idx <- setNames(seq_along(unique_platforms_in_pairs), unique_platforms_in_pairs)
valid_platform_pairs <- valid_platform_pairs %>%
  mutate(
    dist_1_to_2 = dist_matrix_platforms[cbind(platform_pairs_idx[stop_id_platform_1], platform_pairs_idx[stop_id_platform_2])],
    dist_2_to_1 = dist_matrix_platforms[cbind(platform_pairs_idx[stop_id_platform_2], platform_pairs_idx[stop_id_platform_1])])

#Reformat for integration into pathways.txt
valid_platform_pairs <- valid_platform_pairs %>%
  mutate(dist_1_to_2 = if_else(dist_1_to_2 == Inf, NA, dist_1_to_2),
         dist_2_to_1 = if_else(dist_2_to_1 == Inf, NA, dist_2_to_1))
accessible_platform_paths <- valid_platform_pairs %>%
  filter(!is.na(dist_1_to_2) | !is.na(dist_2_to_1)) %>%
  mutate(
    is_bidirectional = case_when(
      !is.na(dist_1_to_2) & !is.na(dist_2_to_1) ~ 1,
      TRUE ~ 0),
    from_stop_id = case_when(
      is_bidirectional == 1 ~ stop_id_platform_1,
      !is.na(dist_1_to_2) ~ stop_id_platform_1,
      TRUE ~ stop_id_platform_2),
    to_stop_id = case_when(
      is_bidirectional == 1 ~ stop_id_platform_2,
      !is.na(dist_1_to_2) ~ stop_id_platform_2,
      TRUE ~ stop_id_platform_1)) %>%
  select(from_stop_id, to_stop_id, is_bidirectional)

#Combine both dataframes and finalise formatting
pathways_final <- rbind(accessible_paths, accessible_platform_paths)%>%
  mutate(pathway_id = paste0(from_stop_id, "-", to_stop_id),
         pathway_mode = 1)%>% #this is a lie - but keeping the column in just in case
  select(pathway_id, from_stop_id, to_stop_id, pathway_mode, is_bidirectional)

#Add station entrances to GTFS stops
entrance_rows <- stops %>%
  filter(stop_id %in% entrances$stop_id | stop_id %in% entrances$parent_station)%>%
  select(-stop_desc, -level_id, -platform_code)%>%
  select(stop_id, stop_code, stop_name, stop_lon, stop_lat, location_type, parent_station)
#Alter existing tube/Overground stations - should have parent station, no stop code, location_type = 0
gtfs_stops_altered <- gtfs$stops %>%
  mutate(location_type = 0,
         parent_station = if_else(stop_code %in% stops$stop_code, stop_code, NA),
         stop_code = if_else(stop_code %in% stops$stop_code, NA, stop_code))
#Combine
final_stops <- rbind(entrance_rows, gtfs_stops_altered)

#Remove stops/pathways if they are not already in gtfs$stops
final_stops <- final_stops %>%
  filter(stop_code %in% gtfs$stops$stop_code | parent_station %in% gtfs$stops$stop_code)
pathways_final <- pathways_final %>%
  filter(from_stop_id %in% final_stops$stop_id,
         to_stop_id %in% final_stops$stop_id)

#Pathways logic above doesn't deal well with unidirectional edges - let's manually correct these
pathways_final <- pathways_final %>%
  mutate(is_bidirectional = if_else(from_stop_id == 'HUBBKG-Plat07-EB-london-overground|national-rail' & to_stop_id == 'HUBBKG-Plat08-WB-london-overground|national-rail', 0, is_bidirectional))
pathways_final <- pathways_final %>%
  mutate(pathway_id = if_else(pathway_id == 'HUBPAD-Outside-HUBPAD-Plat02-EB-circle|district', 'HUBPAD-Plat02-EB-circle|district-HUBPAD-Outside', pathway_id),
         from_stop_id = if_else(pathway_id == 'HUBPAD-Plat02-EB-circle|district-HUBPAD-Outside', 'HUBPAD-Plat02-EB-circle|district', from_stop_id),
         to_stop_id = if_else(pathway_id == 'HUBPAD-Plat02-EB-circle|district-HUBPAD-Outside', 'HUBPAD-Outside', to_stop_id),
         is_bidirectional = if_else(pathway_id == 'HUBPAD-Plat02-EB-circle|district-HUBPAD-Outside', 0, is_bidirectional))

#Fill in lon and lat for each station using platform coordinates (imperfect, but should be fine)
first_coords <- final_stops %>%
  filter(location_type == 0 & !is.na(parent_station)) %>%
  group_by(parent_station) %>%
  slice(1)%>%
  select(parent_station, stop_lon, stop_lat)%>%
  rename("platform_lon" = stop_lon, "platform_lat" = stop_lat)
final_stops <- final_stops %>%
  left_join(first_coords, by = "parent_station") %>%
  mutate(
    stop_lon = if_else(is.na(stop_lon), platform_lon, stop_lon),
    stop_lat = if_else(is.na(stop_lat), platform_lat, stop_lat)) %>%
  select(-platform_lon, -platform_lat)%>%
  left_join(first_coords, by = c("stop_code" = "parent_station")) %>%
  mutate(
    stop_lon = if_else(is.na(stop_lon), platform_lon, stop_lon),
    stop_lat = if_else(is.na(stop_lat), platform_lat, stop_lat)) %>%
  select(-platform_lon, -platform_lat)
#This solves the problem but means walking distances are likely inaccurate

#Add updated stops and pathways to GTFS
gtfs$stops <- final_stops
gtfs$pathways <- pathways_final

#Add wheelchair_boarding field to stops.txt, to ensure stops not mentioned in pathways.txt are recognised as accessible
#We will only do this for stops not in pathways.txt
gtfs$stops <- gtfs$stops %>%
  mutate(wheelchair_boarding = if_else(location_type == 0 & is.na(parent_station), 1, NA))

#Set wheelchair_boarding to 2 for LU stops not mentioned in pathways, and 1 if they are
pathways_parent <- gtfs$pathways %>%
  mutate(parent_station = str_extract(pathway_id, "^[^\\-]+"))
potential_stops <- gtfs$stops %>%
  filter(is.na(wheelchair_boarding) & location_type == 0)
potential_stops <- potential_stops %>%
  mutate(
    wheelchair_boarding = case_when(
      parent_station %in% pathways_parent$parent_station ~ 1,
      !parent_station %in% pathways_parent$parent_station ~ 2,
      TRUE ~ wheelchair_boarding))
gtfs$stops <- gtfs$stops %>%
  rows_update(potential_stops, by = "stop_id")

#And try same logic for parent stations
parent_stations <- gtfs$stops %>%
  filter(location_type==1)%>%
  mutate(wheelchair_boarding = if_else(stop_id %in% pathways_parent$parent_station, 1, 2))
gtfs$stops <- gtfs$stops %>%
  rows_update(parent_stations, by = "stop_id")

#Set all trips as wheelchair accessible (stops.txt and pathways.txt will determine if they actually are or not)
gtfs$trips <- gtfs$trips %>%
  mutate(wheelchair_accessible = 1L)

#Set all pathways to wheelchair accessible
gtfs$pathways <- gtfs$pathways %>%
  mutate(wheelchair_accessible = 1L)

#Change stop entrance/exit logic in case that helps OTP
gtfs$stops <- gtfs$stops %>%
  mutate(location_type = if_else(location_type == 3, 2, location_type))

#Check validity
output_path <- tempfile("validation_result")
validator_path <- download_validator(tempdir())
gtfstools::validate_gtfs(gtfs, output_path, validator_path)
#We get certain warnings about dangling stops, or stops excluded from pathways.txt, but this is because pathways.txt only includes accessible paths

#Export
gtfs_write(gtfs, folder = "final_r5r", name = "gtfs_accessible")

#Set Battersea Park location type to 1 (station) rather than 0 (platform) - for overall classifications
gtfs$stops <- gtfs$stops %>%
mutate(location_type = if_else(stop_id == 'BATRSPK', 1, location_type))
gtfs_write(gtfs, folder = "final_r5r", name = "gtfs_accessibleBAT1")

#Accessibility summary:
  # - Set all non-Underground/Overground stops to be wheelchair accessible in stops.txt (wheelchair_boarding = 1)
  # - All Underground/Overground stops not mentioned at all in pathways are marked as inaccessible (wheelchair_boarding = 2)
  # - Other Underground/Overground stops are set to be wheelchair accessible (wheelchair_boarding = 1), even if they are not fully accessible
    # - Then it is hoped that pathways.txt overrides situations where it is only partially accessible
  # - All trips are set to wheelchair accessible because all vehicles can accommodate wheelchairs

rm(list=ls())
