library(opentripplanner)

#Temporarily switch from Java 21 (for r5r) to 17 (for OTP)
Sys.setenv(JAVA_HOME = Sys.getenv("JAVA_HOME17"))
Sys.setenv(PATH = paste0(Sys.getenv("JAVA_HOME"), "/bin;", Sys.getenv("PATH")))

#Set up the OTP directory
otp_path <- "otp"
dir.create(otp_path, recursive = TRUE)
path_otp <- otp_dl_jar(otp_path, cache = FALSE, version = "2.2.0")

#Create directory structure
dir.create("otp/graphs/standard", recursive = TRUE)
dir.create("otp/graphs/accessible", recursive = TRUE)

#Need to work out how to copy GTFS and PBF here XXX

#Build graphs
log1 <- otp_build_graph(otp = path_otp, dir = otp_path, router = "standard", quiet=FALSE, memory=9216)


#Downloading new osm.pbf, in case that works
library(osmextract)
bbox_combined <- st_bbox(bbox_combined, crs = 4326)

original <- getOption("timeout")
options(timeout = max(5000, original))
oe_get(st_as_sfc(bbox_combined), boundary=st_as_sfc(bbox_combined), download_directory="otp/graphs/standard")
options(timeout = original)

##Try other OTP versions!!

#To do:
# - Make directory setup reproducible
# - Set up OTP for London in general
# - Set up OTP for London accessible network and check whether it works
# - Trial query from origin to destination centroids
# - Then look into distribution of workforce population and check whether it's actually a good variable to use!
# - Classify stations into fully accessible, partially accessible, etc.