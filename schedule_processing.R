# 1) Processing Timetables

library(tidyverse)
library(UK2GTFS)
#Note when Traveline TransXChange data was downloaded (09/05/2025)

# ------ Get GTFS network ---------

#Convert London transport network to GTFS
path <- "london_traveline.zip"
gtfs <- transxchange2gtfs(path_in = path, ncores = 3)
gtfs <- gtfs_merge(gtfs, force = TRUE)
gtfs_write(gtfs, folder = getwd(), name = "gtfs_london")

# ------ QA GTFS network ------

#Count total underground stations - we need to check it contains all stations (including those outside London)
gtfs$stops %>%
  filter(str_detect(stop_name, "Underground Station")) %>%
  distinct(stop_name) %>%
  nrow()

#273, and there should be 272!
#Note that there are some stations (e.g. Edgware road) with multiple stop names
#Search for bracket to find these
#Could lead to inaccurate network representation
#But changing this could disrupt the network
#So next step is to see what TfL accessibility data looks like - need to compare to this