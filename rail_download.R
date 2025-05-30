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

#Move Barking station onto street network
gtfs_nr$stops <- gtfs_nr$stops %>%
  mutate(stop_lon = if_else(stop_id == 'BARKING', 0.081114, stop_lon),
         stop_lat = if_else(stop_id == 'BARKING', 51.53926, stop_lat))

#Check GTFS object
output_path <- tempfile("validation_result")
validator_path <- download_validator(tempdir())
validate_gtfs(gtfs_nr, output_path, validator_path) #all looks good - note Clapham Junction is currently marked as two separate stations, when they should just be one

summary(gtfs_nr)

gtfs_write(gtfs_nr, folder = "large_data", name = "gtfs_overground")

#Next, we need to sort platform IDs
#Then we want to merge the two GTFS files and create a new r5r_core

# ------ Cleaning Stops and Matching IDs --------

#In order to join to accessibility data, we need to create a separate stop for each platform
#First, we need to match TIPLOC with NAPTAN codes

# #First, let's load in the TfL detailed station list - for matching via name
# tfl_stations <- read_csv("data/tfl_station_data_detailed/Stations.csv")%>%
#   clean_names()%>%
#   select(unique_id, name)%>%
#   filter(!str_starts(unique_id, "940")) #remove any underground stops
# 
# #Match GTFS_NR stops via name
# gtfs_nr_stops <- gtfs_nr$stops%>%
#   mutate(name_cleaned = gtfs_nr_stops$stop_name <- sub(" Rail Station$", "", gtfs_nr_stops$stop_name))
# gtfs_nr_stops <- gtfs_nr_stops %>%
#   left_join(tfl_stations, by = c("name_cleaned" = "name"))
# 
# #Manually joining the rest
# gtfs_nr_stops <- gtfs_nr_stops %>%
#   mutate(unique_id = if_else(stop_id == 'CAMHTH', '910GCAMHTH', unique_id),
#          unique_id = if_else(stop_id == 'EUSTON', 'HUBEUS', unique_id),
#          unique_id = if_else(stop_id == 'LIVST', 'HUBLST', unique_id),
#          unique_id = if_else(stop_id == 'NWCRELL', 'HUBNWX', unique_id),
#          unique_id = if_else(stop_id == 'QPRK', 'HUBQPW', unique_id),
#          unique_id = if_else(stop_id == 'RICHNLL', 'HUBRMD', unique_id),
#          unique_id = if_else(stop_id == 'SHPDSB', 'HUBSPB', unique_id),
#          unique_id = if_else(stop_id == 'STFD', 'HUBSRA', unique_id),
#          unique_id = if_else(stop_id == 'STJMSST', '910GSTJMSST', unique_id),
#          unique_id = if_else(stop_id == 'WLTHQRD', '910GWLTHQRD', unique_id),
#          unique_id = if_else(stop_id == 'BARKRIV', '910GBKRVS', unique_id),
#          unique_id = if_else(stop_id == 'WMBYDC', 'HUBWMB', unique_id))
# 
# #Battersea Park's ID can stay as it is - it won't affect the analysis as the Overground only stops here very early or late, and TfL lacks accessibility data for it
# gtfs_nr_stops <- gtfs_nr_stops %>%
#   mutate(unique_id = if_else(stop_id == 'BATRSPK', 'BATRSPK', unique_id))
# 
# #Clapham Junction as marked by 2 separate stations - a bit confusing!
# #I will keep it this way for now: one is Mildmay, one is Windrush
# 
# #Now load in TfL platform data - this will help us with directions
# platforms <- read_csv("data/tfl_station_data_detailed/Platforms.csv") %>%
#   clean_names() %>%
#   select(unique_id, station_unique_id, platform_number, cardinal_direction) %>%
#   filter(grepl("overground", unique_id, ignore.case = TRUE))
# 
# #Check all platform IDs are now in the station list
# gtfs_nr_stops_join <- gtfs_nr_stops %>%
#   left_join(platforms, by = c("unique_id" = "station_unique_id"))
# #All joined except for Battersea Park - looks good
# rm(gtfs_nr_stops_join)
# 
# #Now we need to join stops to platforms
# 
# #Work out trip directions:
# trip_directions <- gtfs_nr$stop_times %>%
#   group_by(trip_id) %>%
#   arrange(stop_sequence) %>%
#   summarise(
#     first_stop = first(stop_id),
#     last_stop = last(stop_id))
# trip_directions_unique_stations <- trip_directions %>%
#   distinct(first_stop, last_stop)
# #Next step:
# # - Manually append trip directions?
# # - But a problem is that the platform isn't always fixed

#Immediate steps:
# - Use this to work out which stops a route is actually on
# - Merge GTFS files

#Can we merge platforms to NAPTAN codes?

# Issues
# - Need to match TIPLOC and NaPTAN/TfL IDs
  # - Looks like we can actually use NAPTAN data, if we do 4900 + TIPLOC (or 910 + TIPLOC for general, non-platform-specific)
    # - NAPTAN missing in RStudio has these followed by 0 for a generic entrance
    # - NAPTAN CSV has these with platform-specific numbers
# - Only 1 "stop" per station, while the Traveline data seems to have one per platform
  # - Separating these into platform IDs
  # - Could I display individual routes, work out which direction it's going, and assign that as a stop type?
  # - Then use TfL API to assign actual platform name? Or do this manually - approx 240 times (2x 118 stops)