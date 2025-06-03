#Downloading and processing London Overground data

#In this file, we:
  # - Download ATOC data from the National Rail API
  # - Filter for Overground services and convert to GTFS format
  # - Process the station IDs using the TfL API and topological data to obtain a separate ID for each Overground platform

#This data wrangling was a messy process and certain assumptions had to be made - please see the comments below for more details

library(tidyverse)
library(janitor)
library(httr)
library(UK2GTFS)
library(jsonlite)
library(gtfstools)
options(java.parameters = "-Xmx2G")
library(r5r)

# ----- Download data ------

#Get National Rail API details
username <- Sys.getenv("national_rail_username")
password <- Sys.getenv("national_rail_password")

#Authentication request
response <- POST(
  url = "https://opendata.nationalrail.co.uk/authenticate",
  body = list(
    username = username,
    password = password),
  encode = "form")
token <- content(response, as = "parsed", type = "application/json")$token

#Download timetable data, using token
timetable <- GET(
  url = "https://opendata.nationalrail.co.uk/api/staticfeeds/3.0/timetable",
  add_headers(`X-Auth-Token` = token),
  accept("application/zip"))

#Save the data locally
writeBin(content(timetable, "raw"), "large_data/national_rail_atoc.zip")
unzipped_path <- "large_data/atoc_extracted"
unzip("large_data/national_rail_atoc.zip", exdir = unzipped_path)

# ------ Convert to GTFS format --------

#Reformatting for compatibility with UK2GTFS package
  #1) Making file extensions lowercase
  #2) Removing metadata (starting with '/!!') at the start of certain file types
#Note ChatGPT was used heavily here

target_extensions <- c("dat", "flf", "msn", "set")
files <- list.files(unzipped_path, full.names = TRUE)

for (file in files) {
  #Convert file extension to lowercase
  ext <- tools::file_ext(file)
  new_ext <- tolower(ext)

  #Rename file if extension was changed
  if (ext != new_ext) {
    new_name <- sub(paste0("\\.", ext, "$"), paste0(".", new_ext), file)
    file.rename(file, new_name)
    file <- new_name
    ext <- new_ext
  }

  #Remove metadata from the start of certain file types
  if (ext %in% target_extensions) {
    lines <- readLines(file, warn = FALSE)
    cleaned <- lines[!grepl("^/!!", lines)]
    writeLines(cleaned, file)
  }
}

#Rezip file
zipped_path <- "large_data/national_rail_atoc.zip"
file.remove(zipped_path)
files_to_zip <- list.files(unzipped_path, full.names = TRUE)
zip(zipfile = zipped_path, files = files_to_zip, flags = "-j")

#Now we can convert to GTFS
gtfs_nr <- atoc2gtfs(path_in = zipped_path, ncores = 3, silent=FALSE)

#Need to export and re-import it to get it in GTFS object-type
gtfs_write(gtfs_nr, folder = "large_data", name = "gtfs_nr_test")
gtfs_nr <- read_gtfs("large_data/gtfs_nr_test.zip")
file.remove("large_data/gtfs_nr_test.zip")

#Filter for London Overground services only
gtfs_nr$routes <- gtfs_nr$routes %>%
  filter(agency_id == 'LO') #routes
#Now ensure compatibility with other gtfs files
gtfs_nr$trips <- gtfs_nr$trips %>%
  filter(route_id %in% gtfs_nr$routes$route_id) #trips
gtfs_nr$stop_times <- gtfs_nr$stop_times %>%
  filter(trip_id %in% gtfs_nr$trips$trip_id) #stop_times
gtfs_nr$stops <- gtfs_nr$stops %>%
  filter(stop_id %in% gtfs_nr$stop_times$stop_id) #stops
gtfs_nr$agency <- gtfs_nr$agency %>%
  filter(agency_id == 'LO') #agency
gtfs_nr$calendar <- gtfs_nr$calendar %>%
  filter(service_id %in% gtfs_nr$trips$service_id) #calendar
gtfs_nr$calendar_dates <- gtfs_nr$calendar_dates %>%
  filter(service_id %in% gtfs_nr$trips$service_id) #calendar_dates

#Removing transfers.txt, as we don't have this in the Traveline data
gtfs_nr$transfers <- NULL

#Let's filter out rail replacement buses - for easier platform ID matching later
gtfs_nr <- filter_by_route_type(gtfs_nr, route_type = 2, keep = TRUE)

#Make trains stopping at Cannonbury ELL (CNNBELL) just stop at Cannonbury (CNNB) - it is the same station
gtfs_nr$stop_times <- gtfs_nr$stop_times %>%
  mutate(stop_id = if_else(stop_id == 'CNNBELL', 'CNNB', stop_id))
gtfs_nr$stops <- gtfs_nr$stops %>%
  filter(!stop_id == 'CNNBELL')

#And Highbury & Islington ELL (HIGHBYE) to Highbury & Islington Rail Station (HIGHBYA)
gtfs_nr$stop_times <- gtfs_nr$stop_times %>%
  mutate(stop_id = if_else(stop_id == 'HIGHBYE', 'HIGHBYA', stop_id))
gtfs_nr$stops <- gtfs_nr$stops %>%
  filter(!stop_id == 'HIGHBYE')

#And Willesden Junction Low Level (WLSDNJL) to Willesden Junction Rail Station (WLSDJHL)
gtfs_nr$stop_times <- gtfs_nr$stop_times %>%
  mutate(stop_id = if_else(stop_id == 'WLSDNJL', 'WLSDJHL', stop_id))
gtfs_nr$stops <- gtfs_nr$stops %>%
  filter(!stop_id == 'WLSDNJL')

#And New Cross Gate ELL (NEWXGEL) to New Cross Gate (NEWXGTE)
gtfs_nr$stop_times <- gtfs_nr$stop_times %>%
  mutate(stop_id = if_else(stop_id == 'NEWXGEL', 'NEWXGTE', stop_id))
gtfs_nr$stops <- gtfs_nr$stops %>%
  filter(!stop_id == 'NEWXGEL')

#Combine both Clapham Junctions (CLPHMJ1, CLPHMJC)
gtfs_nr$stop_times <- gtfs_nr$stop_times %>%
  mutate(stop_id = if_else(stop_id == 'CLPHMJ1', 'CLPHMJC', stop_id))
gtfs_nr$stops <- gtfs_nr$stops %>%
  filter(!stop_id == 'CLPHMJ1')

#Move Barking station onto street network
gtfs_nr$stops <- gtfs_nr$stops %>%
  mutate(stop_lon = if_else(stop_id == 'BARKING', 0.081114, stop_lon),
         stop_lat = if_else(stop_id == 'BARKING', 51.53926, stop_lat))

# ------ Cleaning Stops and Matching IDs --------

#In order to join to accessibility data, we need to create a separate stop for each platform - at present each code represents a station

#First, let's load in the TfL detailed station list - for matching via name
tfl_stations <- read_csv("data/tfl_station_data_detailed/Stations.csv")%>%
  clean_names()%>%
  select(unique_id, name)%>%
  filter(!str_starts(unique_id, "940")) #remove any underground stops

#Match GTFS_NR stops via name
gtfs_nr_stops <- gtfs_nr$stops
gtfs_nr_stops <- gtfs_nr_stops %>%
  mutate(name_cleaned = gtfs_nr_stops$stop_name <- sub(" Rail Station$", "", gtfs_nr_stops$stop_name))
gtfs_nr_stops <- gtfs_nr_stops %>%
  left_join(tfl_stations, by = c("name_cleaned" = "name"))

#Manually joining the rest
gtfs_nr_stops <- gtfs_nr_stops %>%
  mutate(unique_id = if_else(stop_id == 'CAMHTH', '910GCAMHTH', unique_id),
         unique_id = if_else(stop_id == 'EUSTON', 'HUBEUS', unique_id),
         unique_id = if_else(stop_id == 'LIVST', 'HUBLST', unique_id),
         unique_id = if_else(stop_id == 'NWCRELL', 'HUBNWX', unique_id),
         unique_id = if_else(stop_id == 'QPRK', 'HUBQPW', unique_id),
         unique_id = if_else(stop_id == 'RICHNLL', 'HUBRMD', unique_id),
         unique_id = if_else(stop_id == 'SHPDSB', 'HUBSPB', unique_id),
         unique_id = if_else(stop_id == 'STFD', 'HUBSRA', unique_id),
         unique_id = if_else(stop_id == 'STJMSST', '910GSTJMSST', unique_id),
         unique_id = if_else(stop_id == 'WLTHQRD', '910GWLTHQRD', unique_id),
         unique_id = if_else(stop_id == 'BARKRIV', '910GBKRVS', unique_id),
         unique_id = if_else(stop_id == 'WMBYDC', 'HUBWMB', unique_id))

#Battersea Park's ID can stay as it is - it won't affect the analysis as the Overground only stops here very early or late, and TfL lacks accessibility data for it anyway
gtfs_nr_stops <- gtfs_nr_stops %>%
  mutate(unique_id = if_else(stop_id == 'BATRSPK', 'BATRSPK', unique_id))

#Now load in TfL platform data - this will help us with directions
platforms <- read_csv("data/tfl_station_data_detailed/Platforms.csv") %>%
  clean_names() %>%
  select(unique_id, station_unique_id, platform_number, cardinal_direction) %>%
  filter(grepl("overground", unique_id, ignore.case = TRUE))

#Check all platform IDs are now in the station list
gtfs_nr_stops_join <- gtfs_nr_stops %>%
  left_join(platforms, by = c("unique_id" = "station_unique_id"))
#All joined except for Battersea Park - looks good
rm(platforms)

#Cleaning IDs for easier joining
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  rename("tfl_id" = unique_id,
         "platform_id" = unique_id.y,
         "nr_id" = stop_id)%>%
  select(nr_id, tfl_id, stop_name, stop_lon, stop_lat, platform_id, platform_number, cardinal_direction)

#Update Battersea Park platform, as we will probably be joining on this field
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  mutate(platform_id = if_else(tfl_id == 'BATRSPK', 'BATRSPK', platform_id))

#Now we need to join stops to platforms - we will use trip directions for this

#Work out trip directions:
trip_directions <- gtfs_nr$stop_times %>%
  group_by(trip_id) %>%
  arrange(stop_sequence) %>%
  summarise(
    first_stop = first(stop_id),
    last_stop = last(stop_id))
trip_directions_unique_stations <- trip_directions %>%
  distinct(first_stop, last_stop)%>%
  left_join(gtfs_nr_stops, by = c("first_stop" = "stop_id"))%>%
  left_join(gtfs_nr_stops, by = c("last_stop" = "stop_id"))%>%
  select(first_stop, last_stop, stop_name.x, stop_name.y)%>%
  rename("first_stop_name" = stop_name.x,
         "last_stop_name" = stop_name.y)
#write.csv(trip_directions_unique_stations, "data/trip_directions_unique_stations.csv", row.names = FALSE)
rm(trip_directions, trip_directions_unique_stations)

#Manually appended these in Excel - there is probably a better way
trip_directions <- read_csv("data/trip_directions_unique_stations.csv")%>%
  clean_names()%>%
  select(-first_stop_name, -last_stop_name)

#Before we join directions to stop_times, we need to check whether some stations have multiple overground platforms in the same direction
direction_test <- gtfs_nr_stops_join %>%
  group_by(tfl_id, cardinal_direction) %>%
  summarise(platform_count = n_distinct(platform_id), .groups = "drop") %>%
  filter(platform_count > 1)

#At some stations, the same route travels from multiple platforms - we will have to make simplifications

#Keep only the first Liverpool Street Platform - it looks like accessibility is the same for each
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  group_by(tfl_id) %>%
  filter(!(tfl_id == "HUBLST" & row_number() > 1)) %>%
  ungroup()
#Do the same for Euston
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  group_by(tfl_id) %>%
  filter(!(tfl_id == "HUBWFJ" & row_number() > 1)) %>%
  ungroup()
#Chingford - looks like most services are from Platform 2
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  group_by(tfl_id) %>%
  filter(!(tfl_id == "910GCHINGFD" & row_number() != 2)) %>%
  ungroup()
#Dalston Junction - looks like all Southbound platforms are used equally frequently, let's take the first
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  filter(!(tfl_id == "910GDALS" & platform_number %in% c(3, 4)))
#Richmond - looks like platform 5 is the most frequent for Overground
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  filter(!(tfl_id == "HUBRMD" & platform_number %in% c(3, 4)))
#Barking Riverside - looks like both platforms are used equally, will just pick 1
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  filter(!(tfl_id == "910GBKRVS" & platform_number == 2))
#Barking - looks like platform 8 is more used for Westbound services than 1
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  filter(!(tfl_id == "HUBBKG" & platform_number == 1))
#Crystal Palace - departures mostly from platform 3
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  filter(!(tfl_id == "HUBCYP" & platform_number == 5))
#Euston - departures mostly from platform 9
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  filter(!(tfl_id == "HUBEUS" & platform_number == 10))
#Norwood Junction - departures mostly from 1 and 5
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  filter(!(tfl_id == "HUBNWD" & platform_number %in% c(3, 6)))

#Note there are still some stops with multiple Overground routes in the same cardinal direction
#Let's join anyway and manually fix those later

#Some of the TfL platform information is either incorrect or out of date - we need to manually fix it
#Imperial Wharf
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  mutate(
    cardinal_direction = case_when(
      nr_id == 'CSEAH' & platform_number == '2' ~ 'Eastbound',
      nr_id == 'CSEAH' & platform_number == '1' ~ 'Westbound',
      TRUE ~ cardinal_direction))
#West Brompton
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  mutate(
    cardinal_direction = case_when(
      nr_id == 'WBRMPTN' & platform_number == '4' ~ 'Eastbound',
      nr_id == 'WBRMPTN' & platform_number == '3' ~ 'Westbound',
      TRUE ~ cardinal_direction))
#Kensington (Olympia)
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  mutate(
    cardinal_direction = case_when(
      nr_id == 'KENOLYM' & platform_number == '2' ~ 'Eastbound',
      nr_id == 'KENOLYM' & platform_number == '3' ~ 'Westbound',
      TRUE ~ cardinal_direction))
#Shepherd's Bush
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  mutate(
    cardinal_direction = case_when(
      nr_id == 'SHPDSB' & platform_number == '2' ~ 'Eastbound',
      nr_id == 'SHPDSB' & platform_number == '1' ~ 'Westbound',
      TRUE ~ cardinal_direction))
#Upminster
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  mutate(
    cardinal_direction = case_when(
      nr_id == 'UPMNSP6' & platform_number == '6' ~ 'Westbound',
      TRUE ~ cardinal_direction))
#Peckham Rye
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  mutate(
    cardinal_direction = case_when(
      nr_id == 'PKHMRYC' & platform_number == '2' ~ 'Northbound',
      nr_id == 'PKHMRYC' & platform_number == '1' ~ 'Southbound',
      TRUE ~ cardinal_direction))
#Denmark Hill
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  mutate(
    cardinal_direction = case_when(
      nr_id == 'DENMRKH' & platform_number == '2' ~ 'Northbound',
      nr_id == 'DENMRKH' & platform_number == '1' ~ 'Southbound',
      TRUE ~ cardinal_direction))
#Clapham High Street
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  mutate(
    cardinal_direction = case_when(
      nr_id == 'CLPHHS' & platform_number == '2' ~ 'Northbound',
      nr_id == 'CLPHHS' & platform_number == '1' ~ 'Southbound',
      TRUE ~ cardinal_direction))
#Wandsworth Road
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  mutate(
    cardinal_direction = case_when(
      nr_id == 'WNDSWRD' & platform_number == '2' ~ 'Northbound',
      nr_id == 'WNDSWRD' & platform_number == '1' ~ 'Southbound',
      TRUE ~ cardinal_direction))

#Append direction onto stop times
#First, join directions to trip information
trip_id_lookup <- gtfs_nr$stop_times %>%
  group_by(trip_id) %>%
  arrange(stop_sequence) %>%
  summarise(first_stop = first(stop_id),
            last_stop = last(stop_id))
trip_id_lookup <- trip_id_lookup %>%
  left_join(trip_directions, by = c("first_stop", "last_stop"))

#Artificially remove duplicate platforms which we'll need to manually fix later
#Rerun duplicate test
direction_test <- gtfs_nr_stops_join %>%
  group_by(tfl_id, cardinal_direction) %>%
  summarise(platform_count = n_distinct(platform_id), .groups = "drop") %>%
  filter(platform_count > 1)
#Remove the first instance from the platform list
temp_gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  left_join(direction_test, by = c("tfl_id", "cardinal_direction"))%>%
  group_by(tfl_id, cardinal_direction)%>%
  filter(is.na(platform_count) | row_number() != 1) %>%
  ungroup()%>%
  select(-platform_count)

#Adding dictionary logic for stops at the start/end of line - reverse cardinal direction
directions <- c("Northbound"="Southbound", "Southbound"="Northbound", "Eastbound"="Westbound", "Westbound"="Eastbound")

gtfs_nr_stop_times <- gtfs_nr$stop_times
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  left_join(trip_id_lookup, by=c("trip_id"))%>%
  mutate(cardinal_direction = if_else(pickup_type == 1, #invert platform at the end of line - so it "stops" in the same place it starts
                              directions[cardinal_direction],
                              cardinal_direction))%>%
  left_join(temp_gtfs_nr_stops_join, by = c("stop_id" = "nr_id", "cardinal_direction"))

#Update logic for Battersea Park, which was excluded from platform dataset
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  mutate(platform_id = if_else(stop_id == 'BATRSPK', 'BATRSPK', platform_id))

#Manually updating further stations
#Liverpool Street: all one platform, randomly-chosen because they differ
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  mutate(tfl_id = if_else(stop_id == 'LIVST', 'HUBLST', tfl_id),
         stop_name = if_else(stop_id == 'LIVST', 'London Liverpool Street Rail Station', stop_name),
         stop_lon = if_else(stop_id == 'LIVST', -0.08143, stop_lon),
         stop_lat = if_else(stop_id == 'LIVST', 51.51799, stop_lat),
         platform_id = if_else(stop_id == 'LIVST', 'HUBLST-Plat01-EB-london-overground', platform_id),
         platform_number = if_else(stop_id == 'LIVST', '1', platform_number))

#Bethnal Green - what is actually Northbound has an Eastbound platform (2)
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  mutate(tfl_id = if_else(stop_id == 'BTHNLGR' & stop_sequence == 2, '910GBTHNLGR', tfl_id),
         stop_name = if_else(stop_id == 'BTHNLGR' & stop_sequence == 2, 'Bethnal Green Rail Station', stop_name),
         stop_lon = if_else(stop_id == 'BTHNLGR' & stop_sequence == 2, -0.05957, stop_lon),
         stop_lat = if_else(stop_id == 'BTHNLGR' & stop_sequence == 2, 51.52392, stop_lat),
         platform_id = if_else(stop_id == 'BTHNLGR' & stop_sequence == 2, '910GBTHNLGR-Plat02-EB-london-overground', platform_id),
         platform_number = if_else(stop_id == 'BTHNLGR' & stop_sequence == 2, '2', platform_number))
#And what is Southbound has a Westbound platform (1)
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  mutate(tfl_id = if_else(stop_id == 'BTHNLGR' & stop_sequence > 2, '910GBTHNLGR', tfl_id),
         stop_name = if_else(stop_id == 'BTHNLGR' & stop_sequence > 2, 'Bethnal Green Rail Station', stop_name),
         stop_lon = if_else(stop_id == 'BTHNLGR' & stop_sequence > 2, -0.05957, stop_lon),
         stop_lat = if_else(stop_id == 'BTHNLGR' & stop_sequence > 2, 51.52392, stop_lat),
         platform_id = if_else(stop_id == 'BTHNLGR' & stop_sequence > 2, '910GBTHNLGR-Plat01-WB-london-overground', platform_id),
         platform_number = if_else(stop_id == 'BTHNLGR' & stop_sequence > 2, '1', platform_number))

#Inspect remaining nulls
check_nulls <- gtfs_nr_stop_times %>%
  filter(is.na(platform_id))

#Some trips are clearly invalid - they jump in space between Mildmay and Lioness
#We will probably have to remove these entirely - a limitation of the data
problematic_trips <- check_nulls %>%
  filter(stop_id %in% c("KLBRNHR", "SHMPSTD", "KENSLG", "QPRK"))
problematic_trip_ids <- unique(problematic_trips$trip_id)
gtfs_nr$trips <- gtfs_nr$trips %>%
  filter(!trip_id %in% problematic_trip_ids)
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  filter(!trip_id %in% problematic_trip_ids)
#And some invalid trips jumping from the Suffragette to Mildmay
more_problematic_ids <- c(336006, 370144)
gtfs_nr$trips <- gtfs_nr$trips %>%
  filter(!trip_id %in% more_problematic_ids)
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  filter(!trip_id %in% more_problematic_ids)

#Manually sort remaining nulls
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  mutate(tfl_id = if_else((first_stop == 'CLPHMJC' & last_stop == 'DALS') | (first_stop == 'DALS' & last_stop == 'CLPHMJC'), 'HUBCLJ', tfl_id),
         stop_name = if_else((first_stop == 'CLPHMJC' & last_stop == 'DALS') | (first_stop == 'DALS' & last_stop == 'CLPHMJC'), 'Clapham Junction Rail Station', stop_name),
         stop_lon = if_else((first_stop == 'CLPHMJC' & last_stop == 'DALS') | (first_stop == 'DALS' & last_stop == 'CLPHMJC'), -0.17017, stop_lon),
         stop_lat = if_else((first_stop == 'CLPHMJC' & last_stop == 'DALS') | (first_stop == 'DALS' & last_stop == 'CLPHMJC'), 51.46415, stop_lat),
         platform_id = if_else((first_stop == 'CLPHMJC' & last_stop == 'DALS') | (first_stop == 'DALS' & last_stop == 'CLPHMJC'), 'HUBCLJ-Plat02-EB-london-overground', platform_id),
         platform_number = if_else((first_stop == 'CLPHMJC' & last_stop == 'DALS') | (first_stop == 'DALS' & last_stop == 'CLPHMJC'), '2', platform_number))
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  mutate(tfl_id = if_else((first_stop == 'CLPHMJC' & last_stop == 'HIGHBYA') | (first_stop == 'HIGHBYA' & last_stop == 'CLPHMJC'), 'HUBCLJ', tfl_id),
         stop_name = if_else((first_stop == 'CLPHMJC' & last_stop == 'HIGHBYA') | (first_stop == 'HIGHBYA' & last_stop == 'CLPHMJC'), 'Clapham Junction Rail Station', stop_name),
         stop_lon = if_else((first_stop == 'CLPHMJC' & last_stop == 'HIGHBYA') | (first_stop == 'HIGHBYA' & last_stop == 'CLPHMJC'), -0.17017, stop_lon),
         stop_lat = if_else((first_stop == 'CLPHMJC' & last_stop == 'HIGHBYA') | (first_stop == 'HIGHBYA' & last_stop == 'CLPHMJC'), 51.46415, stop_lat),
         platform_id = if_else((first_stop == 'CLPHMJC' & last_stop == 'HIGHBYA') | (first_stop == 'HIGHBYA' & last_stop == 'CLPHMJC'), 'HUBCLJ-Plat02-EB-london-overground', platform_id),
         platform_number = if_else((first_stop == 'CLPHMJC' & last_stop == 'HIGHBYA') | (first_stop == 'HIGHBYA' & last_stop == 'CLPHMJC'), '2', platform_number))

#Now we need to manually fix the stations with platforms in the same direction

#Gospel Oak Eastbound: 3 should be Suffragette, 2 should be Mildmay
platform_codes <- c("STFD", "CMDNRD")
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  mutate(platform_id = if_else((platform_id == "910GGOSPLOK-Plat03-EB-london-overground" & (first_stop %in% platform_codes |last_stop %in% platform_codes)), "910GGOSPLOK-Plat02-EB-london-overground", platform_id),
         platform_number = if_else(platform_id == '910GGOSPLOK-Plat02-EB-london-overground', "2", platform_number))

#Hackney Downs Northbound: platform 2 to Chingford, 4 to Enfield/Cheshunt
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  mutate(platform_id = if_else(platform_id == "910GHAKNYNM-Plat04-NB-london-overground" & last_stop == "CHINGFD", "910GHAKNYNM-Plat02-NB-london-overground", platform_id),
         platform_number = if_else(platform_id == '910GHAKNYNM-Plat02-NB-london-overground', "2", platform_number))

#Hackney Downs Southbound
#It looks like these all go to the same destination - platform 3 is more frequently used than 1, so let's keep it this way
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  filter(!platform_id == '910GHAKNYNM-Plat01-SB-london-overground')

#Clapham Junction Eastbound: platform 1 is Mildmay (Stratford), 2 is Windrush (Dalston)
platform_codes <- c("STFD", "WLSDJHL", "GOSPLOK", "CMDNRD", "SHPDSB")
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  mutate(platform_id = if_else((platform_id == "HUBCLJ-Plat02-EB-london-overground" & (first_stop %in% platform_codes |last_stop %in% platform_codes)), "HUBCLJ-Plat01-EB-london-overground", platform_id),
         platform_number = if_else(platform_id == 'HUBCLJ-Plat01-EB-london-overground', "1", platform_number))

#Stratford Westbound: 1 to Richmond/Camden, 2 to Clapham
platform_codes <- c("RICHNLL", "CMDNRD", "ACTNCTL")
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  mutate(platform_id = if_else((platform_id == "HUBSRA-Plat02-WB-london-overground" & (first_stop %in% platform_codes |last_stop %in% platform_codes)), "HUBSRA-Plat01-WB-london-overground", platform_id),
         platform_number = if_else(platform_id == 'HHUBSRA-Plat01-WB-london-overground', "1", platform_number))

#Willesden Junction Eastbound
#Looks like platform 2 is rarely used - let's remove it
gtfs_nr_stops_join <- gtfs_nr_stops_join %>%
  filter(!platform_id == 'HUBWIJ-Plat02-EB-london-overground')

#Highbury and Islington Southbound: 1 to West Croydon, 2 to Crystal Palace
platform_codes <- c("WCROYDN", "BATRSPK")
gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  mutate(platform_id = if_else((platform_id == "HUBHHY-Plat02-SB-london-overground" & (first_stop %in% platform_codes |last_stop %in% platform_codes)), "HUBHHY-Plat01-SB-london-overground", platform_id),
         platform_number = if_else(platform_id == 'HUBHHY-Plat01-SB-london-overground', "1", platform_number))

#Limitations: for routes terminating before one of these, it is unclear which platform it departs from (as I'm comparing to live departures)
#Another limitation was that because some of the TfL directions were incorrect or inconsistent, we don't know if they all joined correctly (we only found errors if there were nulls - what about multiple lines at the same station?)

#Reformatting stops.txt and stop_times.txt for reintegration into the GTFS object
final_gtfs_nr_stops <- gtfs_nr_stops_join %>%
  select(platform_id, tfl_id, stop_name, stop_lon, stop_lat) %>%
  rename("stop_id" = platform_id,
         "stop_code" = tfl_id)

final_gtfs_nr_stop_times <- gtfs_nr_stop_times %>%
  select(trip_id, arrival_time, departure_time, platform_id, stop_sequence)%>%
  rename("stop_id" = platform_id)%>%
  mutate(timepoint = 1)

gtfs_nr$stops <- final_gtfs_nr_stops
gtfs_nr$stop_times <- final_gtfs_nr_stop_times

#Check GTFS object
output_path <- tempfile("validation_result")
validator_path <- download_validator(tempdir())
validate_gtfs(gtfs_nr, output_path, validator_path)

summary(gtfs_nr)

#Export
gtfs_write(gtfs_nr, folder = "large_data", name = "gtfs_overground")

rm(check_nulls, direction_test, gtfs_nr_stops, problematic_trips, temp_gtfs_nr_stops_join, trip_directions, trip_id_lookup, directions, more_problematic_ids, platform_codes, problematic_trip_ids, output_path, validator_path, gtfs_nr_stops_join, gtfs_nr_stop_times, final_gtfs_nr_stops, final_gtfs_nr_stop_times, tfl_stations)
