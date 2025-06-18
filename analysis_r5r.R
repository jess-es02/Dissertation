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
library(gtfstools)
library(UK2GTFS)
library(ggplot2)
library(extrafont)
library(RColorBrewer)
library(tmap)
library(tmaptools)

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

get_fastest_station <- function(origins,
                                destinations, 
                                walk_speed = 1.4, 
                                max_trip_duration = 180) {
  #Get travel times for each departure time, and combine
  ttm_combined <- departure_times %>%
    lapply(function(dt) {
      travel_time_matrix(
        r5r_core,
        origins = origins,
        destinations = destinations,
        mode = c("WALK", "TRANSIT"),
        departure_datetime = dt,
        walk_speed = walk_speed,
        max_trip_duration = max_trip_duration,
        progress = TRUE
      ) %>%
        mutate(departure_time = dt)
    }) %>%
    bind_rows()
  
  #Take the average time for each centroid-station pair, and then take the shortest time for each centroid
  fastest_station <- ttm_combined %>%
    group_by(from_id, to_id) %>%
    summarise(mean_travel_time = mean(travel_time_p50, na.rm = TRUE), .groups = "drop") %>%
    group_by(from_id) %>%
    slice_min(mean_travel_time, with_ties = FALSE)
  
  return(fastest_station)
}

fastest_station <- get_fastest_station(origins = pop_centroids, destinations = all_stations)

#Get centroids missing from the ttm
missing_centroids <- pop_centroids %>%
  filter(!id %in% fastest_station$from_id)
fastest_station2 <- get_fastest_station(
  origins = missing_centroids,
  destinations = all_stations,
  max_trip_duration=300)

fastest_station <- rbind(fastest_station, fastest_station2)
summary(fastest_station$mean_travel_time)

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)

#To work out nearest time to accessible station, we need to modify the GTFS network to have no tube/Overground
#Otherwise the shortest route could involve taking one of these to get to the nearest accessible stop!
#We could just select modes in r5r, but this would inadvertently exclude DLR
gtfs <- gtfstools::read_gtfs("final_r5r/gtfs.zip")
#Filter out tube (1) and rail (2), except for DLR
gtfs$routes <- gtfs$routes %>%
  filter(route_type != 1 & (route_type != 2 | agency_id == 'DLR'))
#Filter associated tables
gtfs$trips <- gtfs$trips %>%
  filter(route_id %in% gtfs$routes$route_id)
gtfs$stop_times <- gtfs$stop_times %>%
  filter(trip_id %in% gtfs$trips$trip_id)
dir.create("final_r5r_notube", recursive = TRUE)
gtfs_write(gtfs, folder = "final_r5r_notube", name = "gtfs_no_rail")

#Then paste in the OSM.pbf file

r5r_core <- setup_r5(data_path = "final_r5r_notube", verbose=TRUE)

#Use modified GTFS to work out time to step-free station:

#1) Ceteris paribus - walk speed the same
fastest_accessible_station1 <- get_fastest_station(origins = pop_centroids, destinations = accessible_stations)
missing_centroids <- pop_centroids %>%
  filter(!id %in% fastest_accessible_station1$from_id)
fastest_station2 <- get_fastest_station(
  origins = missing_centroids,
  destinations = all_stations,
  max_trip_duration=500)
fastest_accessible_station1 <- rbind(fastest_accessible_station1, fastest_station2)
summary(fastest_accessible_station1$mean_travel_time)

#2) Slower walking speed to reflect mobility constraints
fastest_accessible_station2 <- get_fastest_station(origins = pop_centroids, destinations = accessible_stations,
                                                   walk_speed = 0.43, max_trip_duration = 300)
missing_centroids <- pop_centroids %>%
  filter(!id %in% fastest_accessible_station2$from_id)
fastest_station2 <- get_fastest_station(
  origins = missing_centroids,
  destinations = all_stations,
  walk_speed = 0.43,
  max_trip_duration=1000)
fastest_accessible_station2 <- rbind(fastest_accessible_station2, fastest_station2)
summary(fastest_accessible_station2$mean_travel_time)

#Join to one dataframe
fastest_time_to_stations <- study_lsoas %>%
  left_join(., fastest_station, by = c("lsoa21cd" = "from_id"))%>%
  rename("mean_fastest_station" = mean_travel_time)%>%
  select(-to_id)%>%
  left_join(., fastest_accessible_station1, by = c("lsoa21cd" = "from_id"))%>%
  rename("mean_accessible_stationCP" = mean_travel_time)%>%
  select(-to_id)%>%
  left_join(., fastest_accessible_station2, by = c("lsoa21cd" = "from_id"))%>%
  rename("mean_accessible_stationSLOW" = mean_travel_time)%>%
  select(-to_id)

#Identify stations where fastest station is (not) accessible
fastest_time_to_stations <- fastest_time_to_stations %>%
  mutate(is_fastest_accessible = if_else(mean_fastest_station == mean_accessible_stationCP, TRUE, FALSE))
summary(fastest_time_to_stations$is_fastest_accessible)

#Calculate ratios
fastest_time_to_stations <- fastest_time_to_stations %>%
  mutate(ratioCP = mean_accessible_stationCP/mean_fastest_station,
         ratioSLOW = mean_accessible_stationSLOW/mean_fastest_station)
summary(fastest_time_to_stations$ratioCP)
summary(fastest_time_to_stations$ratioSLOW)

#Calculate differences
fastest_time_to_stations <- fastest_time_to_stations %>%
  mutate(diffCP = (mean_accessible_stationCP-mean_fastest_station)/(mean_accessible_stationCP+mean_fastest_station),
         diffSLOW = (mean_accessible_stationSLOW-mean_fastest_station)/(mean_accessible_stationSLOW+mean_fastest_station))
summary(fastest_time_to_stations$diffCP)
summary(fastest_time_to_stations$diffSLOW)

# ---- Display results -----

#Violin plot of time distributions
pivoted <- fastest_time_to_stations %>%
  st_drop_geometry() %>%
  select(mean_fastest_station, mean_accessible_stationCP, mean_accessible_stationSLOW) %>%
  rename(
    "Fastest Time\nto a Station" = mean_fastest_station,
    "Fastest Time to an\nAccessible Station,\nSpeed Unchanged" = mean_accessible_stationCP,
    "Fastest Time to an\nAccessible Station,\nSlower Walking Speed" = mean_accessible_stationSLOW
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "type",
               values_to = "value")
pivoted$type <- factor(pivoted$type, levels = c(
  "Fastest Time\nto a Station",
  "Fastest Time to an\nAccessible Station,\nSpeed Unchanged",
  "Fastest Time to an\nAccessible Station,\nSlower Walking Speed"
))

ggplot(pivoted, aes(x = type, y = value, fill = type)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Distribution of Travel Times to Stations",
       x = "Travel Type",
       y = "Time (minutes)") +
  ylim(0, 200) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    axis.title.x = element_text(margin = margin(t = 10)))
#Need to note that this is not the full range - upper bound actually extends beyond 200 min
rm(pivoted)

#Map - binary of whether fastest station is accessible or not

mapping <- c(
  "Fully Accessible" = "darkgreen",
  "Not Fully Accessible" = "red")
mapping2 <- c(
  "Fully Accessible" = "darkseagreen",
  "Not Fully Accessible" = "indianred")
tube_stations_main <- tube_stations_main %>%
  mutate(classification2 = if_else(classification != "Fully Accessible", "Not Fully Accessible", classification))
fastest_time_to_stations <- fastest_time_to_stations %>%
  mutate(is_fastest_accessible = if_else(is_fastest_accessible == TRUE, "Fully Accessible", "Not Fully Accessible"))

tmap_mode("plot")
tmap_options(component.autoscale = TRUE)
tmap_save(
tm_shape(fastest_time_to_stations) +
  tm_polygons(
    col = "is_fastest_accessible",
    palette=mapping,
    alpha=0.35,
    title = "Nearest Station Status",
    textNA = ""
  ) +
  tm_shape(tube_stations_main)+
  tm_dots(fill = "classification2", 
          fill.scale = tm_scale_categorical(values = mapping),
          fill.legend = tm_legend(title = "Station Accessibility"),
          shape=21,
          size=0.6)+
  tm_basemap("Esri.OceanBasemap") +
  tm_title("Fastest Rail Station by Accessibility Status") +
  tm_compass(type = "8star",
             size = 3,
             position = c(0.9, 0.22)) +
  tm_scalebar(
    position = c(0.82, 0.08),
    text.size = 0.7,
    breaks = c(0, 5, 10)
  ) +
  tm_layout(
    legend.position = c(0.01, 0.3),
    legend.bg.color = "white",
    legend.showNA = FALSE,
    title.fontfamily = "Segoe UI Semibold",
    title.size = 1.6,
    legend.text.fontfamily = "Segoe UI",
    legend.title.fontfamily = "Segoe UI Semibold",
    legend.text.size = 0.8,
    legend.title.size = 0.9),
filename = "maps/nearest_station_status.png",
dpi=300)

#Map ratios


#Need to consider the issue that detailed_itineraries provides more realistic travel times than travel_time_matrix
#Hence some LSOAs having unexpectedly long walks

#Note "fastest" may not actually be in practice - consider issues for PwMD on buses, e.g. no space, ramps

#To do:
#Map ratio
#Autocorrelation?
#Bivariate LISA with number of disabled??
#Or index of groups that could benefit?