#Travel time to the transport network

#In this file we:
  # - Use r5r to calculate travel time from each LSOA to the nearest accessible station
    # - For each non-disabled individual, this is simply the nearest station
    # - For PwMD, this is the nearest accessible station
  # - Calculate summary statistics and create maps

#Beforehand, ensure to run:
  # 1) lsoa_processing.R
  # 2) maps_summary_stats.R

library(tidyverse)
library(sf)
options(java.parameters = "-Xmx2G")
library(r5r)

# ---- Accessibility to step-free stations ------

#Extract all stations, format in r5r-compatible manner
all_stations <- tube_stations_main %>%
  select(-classification)%>%
  st_transform(4326) %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()%>%
  rename("id" = stop_id)

#Extract accessible stations, format in r5r-compatible manner
accessible_stations <- tube_stations_main %>%
  filter(classification == 'Fully Accessible')%>%
  select(-classification)%>%
  st_transform(4326) %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()%>%
  rename("id" = stop_id)
  
#Use normal network to work out time to any station
r5r_core <- setup_r5(data_path = "final_r5r", verbose=TRUE)

#Take average over three time periods
departure_times <- as.POSIXct(c(
  "2025-10-08 11:00:00",
  "2025-10-08 11:05:00",
  "2025-10-08 11:10:00"))

ttm_combined <- departure_times %>%
  lapply(function(dt) {
    travel_time_matrix(
      r5r_core,
      origins = pop_centroids,
      destinations = all_stations,
      mode = c("WALK", "TRANSIT"),
      departure_datetime = dt,
      walk_speed = 1.4,
      max_trip_duration = 180, 
      progress = TRUE
    ) %>%
      mutate(departure_time = dt)
  }) %>%
  bind_rows()

#Take the average time for each centroid-station pair, and then take the shortest time for each centroid
fastest_station <- ttm_combined %>%
  group_by(from_id, to_id) %>%
  summarise(mean_travel_time = mean(travel_time_p50, na.rm = TRUE), .groups = "drop")%>%
  group_by(from_id) %>%
  slice_min(mean_travel_time, with_ties = FALSE)

#Get centroids missing from the ttm
missing_centroids <- pop_centroids %>%
  filter(!id %in% fastest_station$from_id)
ttm_extra <- departure_times %>%
  lapply(function(dt) {
    travel_time_matrix(
      r5r_core,
      origins = missing_centroids,
      destinations = all_stations,
      mode = c("WALK", "TRANSIT"),
      departure_datetime = dt,
      walk_speed = 1.4,
      max_trip_duration = 500, 
      progress = TRUE
    ) %>%
      mutate(departure_time = dt)
  }) %>%
  bind_rows()
fastest_station2 <- ttm_extra %>%
  group_by(from_id, to_id) %>%
  summarise(mean_travel_time = mean(travel_time_p50, na.rm = TRUE), .groups = "drop")%>%
  group_by(from_id) %>%
  slice_min(mean_travel_time, with_ties = FALSE)

fastest_station <- rbind(fastest_station, fastest_station2)
summary(fastest_station$mean_travel_time)

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)

#To work out nearest time to accessible station, we need to modify the GTFS network to have no tube/Overground
#Otherwise the shortest route could involve taking one of these to get to the nearest accessible stop!
gtfs <- gtfstools::read_gtfs("final_r5r/gtfs.zip")


#Use modified GTFS to work out time to step-free station

#Summary statistics

#Calculate difference/ratio

#Map

#Need to consider the issue that detailed_itineraries provides more realistic travel times than travel_time_matrix
#Hence some LSOAs having unexpectedly long walks
