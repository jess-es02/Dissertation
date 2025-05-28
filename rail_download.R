library(httr)
library(UK2GTFS)

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
unzip("large_data/national_rail_atoc.zip", exdir = "large_data/atoc_extracted")

# ------ Convert to GTFS format --------
unzipped_path <- "large_data/atoc_extracted"

#Change file types to lowercase, for compatibility with UK2GTFS package
#Note ChatGPT was used here and below to sort the files
files <- list.files(unzipped_path, full.names = TRUE)
for (file in files) {
 ext <- tools::file_ext(file)
 new_ext <- tolower(ext)
 if (ext != new_ext) {
   new_name <- sub(paste0("\\.", ext, "$"), paste0(".", new_ext), file)
   file.rename(file, new_name)
 }
}

#Rezip
zipped_path <- "large_data/national_rail_atoc.zip"
file.remove(zipped_path)
files_to_zip <- list.files(unzipped_path, full.names = TRUE)
zip(zipfile = zipped_path, files = files_to_zip, flags = "-j")

#Now we can convert to GTFS
gtfs_nr <- atoc2gtfs(path_in = zipped_path, ncores = 3, silent=FALSE)
#Conversion isn't working! Check format of ATOC data?

#If I do end up using this data, will need to add to README telling the user to add national rail username and password to .Renviron - and maybe pointing to zip command as that didn't work for me