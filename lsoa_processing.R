#Processing socio-demographic data at LSOA-level

#In this file, we:
  # - Delineate the study area: all LSOAs in Greater London, and any within 2km of a tube stop outside of this area
  # - Prepare origins: population-weighted centroids
    # - We append information on total population, disabled population, and age bands
  # - Prepare destinations: workforce-weighted centroids (derived from OA-level statistics)
    # - We append working population as a proxy for local opportunities

library(tidyverse)
library(janitor)
options(java.parameters = "-Xmx2G")
library(r5r)

#1) ----- Prepare AOI ------

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

#2) ----- Origins: pop-weighted centroids -------

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

#Two centroids do not join to the r5r network - manually moving these

#a) Hillingdon 001E - in a gated community
#Moving it very slightly so it aligns with the next (non-private) road over
pop_centroids <- pop_centroids %>%
  mutate(lon = if_else(id == 'E01002482', -0.410789, lon),
         lat = if_else(id == 'E01002482', 51.61021, lat))

#b) Mole Valley 001B - on a private road (a school)
#Moving it to the closest non-private road
pop_centroids <- pop_centroids %>%
  mutate(lon = if_else(id == 'E01030508', -0.290414, lon),
         lat = if_else(id == 'E01030508', 51.30765, lat))

#3) ------ Destinations: workplace-weighted centroids -------
#The ONS does not release these, so we will estimate using OA-level data

#First, load in OA shapefile
oas <- st_read("large_data/OA_2021_EW_BFC_V8.shp")%>%
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

#Five centroids do not join to the r5r network - manually moving these

#a) Hillingdon 001E - on a gated road
#Moving to the closest non-gated road
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01002482', -0.41118, lon),
         lat = if_else(id == 'E01002482', 51.61036, lat))

#b) Kingston upon Thames 020C - inside Chessington World of Adventures!
#Move coordinates to the park entrance
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01002948', -0.314287, lon),
         lat = if_else(id == 'E01002948', 51.35000, lat))

#c) Hillingdon 031A - inside Heathrow
#Tricky to find an optimal place to move the centroid, because r5r does not support indoor routing
#Let's move to a spot very close to the PT stops and see if that works
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01002444', -0.45215, lon),
         lat = if_else(id == 'E01002444', 51.47121, lat))
#This is the only spot I found which works
#But is likely to overestimate accessibility to jobs as it is so close to the station entrance - in reality, people would have to walk more

#d) Three Rivers 011B - inside an RAF base
#Move coordinates to the entrance
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01023840', -0.408365, lon),
         lat = if_else(id == 'E01023840', 51.61951, lat))

#e) Buckinghamshire 064C - on a golf course
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01017832', -0.598008, lon),
         lat = if_else(id == 'E01017832', 51.53627, lat))

#4) ------ Origin Attributes --------

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

#5) ------ Destination Attributes: workforce pop -------
working_pop_lsoa <- read_csv("data/workforce_pop_lsoa.csv")%>%
  clean_names()%>%
  rename("id" = lower_layer_super_output_areas_code, 
         "working_pop" = count)%>%
  select(-lower_layer_super_output_areas_label)

#Join to workforce centroids
workforce_centroids <- workforce_centroids %>%
  left_join(., working_pop_lsoa, by="id")

rm(lsoas, oas, working_pop_lsoa, working_pop_oa, age, disability, london_lsoas, stop_buffers, stop_buffer_lsoas, stop_locations)

# -------- Basic r5r query -----------

#Set up r5r network
r5r_core <- setup_r5(data_path = "large_data", verbose=TRUE)
#There are some "invalid turn restriction" errors but nothing too serious
#Note that Heathrow stops are not reachable by foot, but are by PT
#Some stops and centroids had to be manually moved to make them reachable via the street/PT network (see above)
#And obviously note limitations with no elevation data, lack of consideration of road micro-geographies, etc.

access_test <- accessibility(r5r_core, 
                             pop_centroids, 
                             workforce_centroids,
                             opportunities_colnames = c("working_pop"),
                             mode = c("WALK", "TRANSIT"),
                             cutoffs= 45)
#Took approx 2 min to run
#E01030658 has 0 access - even takes 60 min to walk to own centroid, because it is on opposite side of motorway!

#Check access for LSOAs actually in London
access_test_london <- access_test %>%
  filter(id %in% london_codes$lsoa21cd)

# To do:
# - Maybe change AOI with Overground and Lizzie?
  # - Could do Greater London + 2km of extra tube stops only?
# - Create new r5r_core
# - Step-free network
# - Accessibility query
# - See how long each takes - then add to for loop?
# - Ensure RRS aren't running?

#Basic vis I will need:
# - public transport network
# - public transport accessible network
# - disability distribution
# - workplace pop dist (autocorrelation?)

stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)