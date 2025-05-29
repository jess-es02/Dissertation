library(httr)
library(UK2GTFS)
library(jsonlite)
library(gtfstools)

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

#Check GTFS object
output_path <- tempfile("validation_result")
validator_path <- download_validator(tempdir())
validate_gtfs(gtfs_nr, output_path, validator_path) #all looks good

summary(gtfs_nr)

#Create a lookup table between TIPLOC and TfL codes

#Load in CORPUS data
corpus <- fromJSON("large_data/CORPUSExtract.json")$TIPLOCDATA%>%
  clean_names()%>%
  filter(tiploc %in% gtfs_nr$stops$stop_id)
#There is nothing here to link with NAPTAN!
#Open Rail Data Wiki says NAPTAN data contains CRS, but it doesn't

# Issues
# - Need to match TIPLOC and NaPTAN/TfL IDs
  # - Looks like we can actually use NAPTAN data, if we do 4900 + TIPLOC
    # - NAPTAN missing in RStudio has these followed by 0 for a generic entrance
    # - NAPTAN CSV has these with platform-specific numbers
# - Only 1 "stop" per station, while the Traveline data seems to have one per platform
  # - Separating these into platform IDs
  # - Could I display individual routes, work out which direction it's going, and assign that as a stop type?
  # - Then use TfL API to assign actual platform name? Or do this manually - approx 240 times (2x 118 stops)
# - 118 stations but only 113 according to TfL?

# To sort
# - If I do end up using this data, will need to add to README telling the user to add national rail username and password to .Renviron - and maybe pointing to zip command as that didn't work for me
# - User will need to add CORPUS data, unzipped, to large_data (requires Network Rail log-in) - https://wiki.openraildata.com/index.php?title=Reference_Data#CORPUS:_Location_Reference_Data
# - NR data for this analysis was downloaded 29/05/2025
# - Change file path (not gtfs_nr_test!)