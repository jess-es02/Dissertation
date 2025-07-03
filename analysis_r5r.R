#Travel time to the transport network

#In this file we:
  # - Use r5r to calculate travel time from each LSOA to the nearest accessible station
    # - For each non-disabled individual, this is simply the nearest station
    # - For PwMD, this is the nearest accessible station
  # - Calculate summary statistics and create maps
  # - Assess local and global spatial autocorrelation in time ratios
  # - Cluster LSOAs according to the disparity and presence of in-need population
  # - Assess not-fully-accessible stations according to their catchment properties, and compare to TfL's shortlisted stations

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
library(rcartocolor)
library(spdep)
library(biscale)
library(cowplot)
library(car)
library(factoextra)
library(cluster)
library(caret)
library(dendextend)
library(MASS)

# ---- Accessibility to step-free stations ------

#Extract all stations, format in r5r-compatible manner
all_stations <- tube_stations_main %>%
  dplyr::select(-classification, -upgrade_status, -fare_zones)%>%
  st_transform(4326) %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()%>%
  rename("id" = stop_id)

#Extract accessible stations, format in r5r-compatible manner
accessible_stations <- tube_stations_main %>%
  filter(classification == 'Fully Accessible')%>%
  dplyr::select(-classification, -upgrade_status, -fare_zones)%>%
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
                                max_trip_duration = 180,
                                mode = c("WALK", "TRANSIT"),
                                return_fastest_station = FALSE) {
  
  #Get travel times for each departure time, and combine
  ttm_combined <- departure_times %>%
    lapply(function(dt) {
      travel_time_matrix(
        r5r_core,
        origins = origins,
        destinations = destinations,
        mode = mode,
        departure_datetime = dt,
        walk_speed = walk_speed,
        max_trip_duration = max_trip_duration,
        progress = TRUE
      ) %>%
        mutate(departure_time = dt)
    }) %>%
    bind_rows()
  
  #Take average time for each centroid-station pair
  average_times <- ttm_combined %>%
    group_by(from_id, to_id) %>%
    summarise(mean_travel_time = mean(travel_time_p50, na.rm = TRUE), .groups = "drop")
  
  #Find the shortest time for each centroid
  fastest <- average_times %>%
    group_by(from_id) %>%
    slice_min(mean_travel_time, with_ties = FALSE) %>%
    ungroup()
  
  #Alter output based on return_fastest_station status
  if (return_fastest_station) {
    return(fastest)} 
  else {
    return(fastest %>% dplyr::select(from_id, mean_travel_time))}
}

fastest_station <- get_fastest_station(origins = pop_centroids, destinations = all_stations, return_fastest_station=TRUE)

#Get centroids missing from the ttm
missing_centroids <- pop_centroids %>%
  filter(!id %in% fastest_station$from_id)
fastest_station2 <- get_fastest_station(
  origins = missing_centroids,
  destinations = all_stations,
  max_trip_duration=300,
  return_fastest_station=TRUE)

fastest_station <- rbind(fastest_station, fastest_station2)
summary(fastest_station$mean_travel_time)

#Calculate fastest station, walking only
fastest_stationWALK <- get_fastest_station(origins = pop_centroids, destinations = all_stations, mode=c("WALK"))
missing_centroids <- pop_centroids %>%
  filter(!id %in% fastest_stationWALK$from_id)
fastest_station2 <- get_fastest_station(origins = missing_centroids, destinations = all_stations, mode=c("WALK"), max_trip_duration=800)
fastest_stationWALK <- rbind(fastest_stationWALK, fastest_station2)

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
fastest_accessible_station1 <- get_fastest_station(origins = pop_centroids, destinations = accessible_stations, return_fastest_station=TRUE)
missing_centroids <- pop_centroids %>%
  filter(!id %in% fastest_accessible_station1$from_id)
fastest_station2 <- get_fastest_station(
  origins = missing_centroids,
  destinations = accessible_stations,
  max_trip_duration=500,
  return_fastest_station=TRUE)
fastest_accessible_station1 <- rbind(fastest_accessible_station1, fastest_station2)
summary(fastest_accessible_station1$mean_travel_time)

#2) Slower walking speed to reflect mobility constraints
fastest_accessible_station2 <- get_fastest_station(origins = pop_centroids, destinations = accessible_stations,
                                                   walk_speed = 0.43, max_trip_duration = 300)
missing_centroids <- pop_centroids %>%
  filter(!id %in% fastest_accessible_station2$from_id)
fastest_station2 <- get_fastest_station(
  origins = missing_centroids,
  destinations = accessible_stations,
  walk_speed = 0.43,
  max_trip_duration=1000)
fastest_accessible_station2 <- rbind(fastest_accessible_station2, fastest_station2)
summary(fastest_accessible_station2$mean_travel_time)

#3) Walk-only - ceteris paribus
fastest_accessible_stationWALK_CP <- get_fastest_station(origins = pop_centroids, destinations = accessible_stations, max_trip_duration = 500, mode=c("WALK"))
missing_centroids <- pop_centroids %>%
  filter(!id %in% fastest_accessible_stationWALK_CP$from_id)
fastest_station2 <- get_fastest_station(origins = missing_centroids, destinations = accessible_stations, mode = c("WALK"), max_trip_duration=1000)
fastest_accessible_stationWALK_CP <- rbind(fastest_accessible_stationWALK_CP, fastest_station2)
summary(fastest_accessible_stationWALK_CP$mean_travel_time)

#Join to one dataframe
fastest_time_to_stations <- study_lsoas %>%
  left_join(., fastest_station, by = c("lsoa21cd" = "from_id"))%>%
  rename("mean_fastest_station" = mean_travel_time,
         "fastest_station" = to_id)%>%
  left_join(., fastest_accessible_station1, by = c("lsoa21cd" = "from_id"))%>%
  rename("mean_accessible_stationCP" = mean_travel_time,
         "fastest_accessible_station" = to_id)%>%
  left_join(., fastest_accessible_station2, by = c("lsoa21cd" = "from_id"))%>%
  rename("mean_accessible_stationSLOW" = mean_travel_time)%>%
  left_join(., fastest_stationWALK, by = c("lsoa21cd" = "from_id"))%>%
  rename("mean_fastest_stationWALK" = mean_travel_time)%>%
  left_join(., fastest_accessible_stationWALK_CP, by = c("lsoa21cd" = "from_id"))%>%
  rename("mean_accessible_stationWALK_CP" = mean_travel_time)%>%

#Calculate fastest accessible station by walking, with the slower walking distance
#This will be the same route as mean_accessible_stationWALK_CP, so we don't need to run the routing again
fastest_time_to_stations <- fastest_time_to_stations %>%
  mutate(mean_accessible_stationWALK_SLOW = round((mean_accessible_stationWALK_CP * 1.4) / 0.43, 1))

st_write(fastest_time_to_stations, "data_export_vis/fastest_time_to_stations2.gpkg")
#fastest_time_to_stations <- st_read("data_export_vis/fastest_time_to_stations2.gpkg")

# ---- Quick summary stats --------

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

#Find the average difference, weighted by number of disabled people
calculations <- pop_centroids %>%
  dplyr::select(id, total_disabled)%>%
  left_join(fastest_time_to_stations, by=c("id" = "lsoa21cd"))%>%
  mutate(diffCP = mean_accessible_stationCP-mean_fastest_station,
         diffSLOW = mean_accessible_stationSLOW-mean_fastest_station)
calculations <- calculations %>%
  mutate(diffCPmultiplied = diffCP * total_disabled,
         diffSLOWmultiplied = diffSLOW * total_disabled)
total_disabled <- sum(calculations$total_disabled)
avg_diffCP <- sum(calculations$diffCPmultiplied)/total_disabled
avg_diffSLOW <- sum(calculations$diffSLOWmultiplied)/total_disabled
print(avg_diffCP)
print(avg_diffSLOW)
#So bulk of difference is predicted to come from diff walking speeds
#Makes sense: lots of stations in central London so lots of choice, and stops tend to be accessible at the end of lines on the periphery

#Now do the same, but considering walking only
calculationsWALK <- pop_centroids %>%
  dplyr::select(id, total_disabled)%>%
  left_join(fastest_time_to_stations, by=c("id" = "lsoa21cd"))%>%
  mutate(diffCP = mean_accessible_stationWALK_CP-mean_fastest_stationWALK,
         diffSLOW = mean_accessible_stationWALK_SLOW-mean_fastest_stationWALK)
calculationsWALK <- calculationsWALK %>%
  mutate(diffCPmultiplied = diffCP * total_disabled,
         diffSLOWmultiplied = diffSLOW * total_disabled)
avg_diffCP_WALK <- sum(calculationsWALK$diffCPmultiplied)/total_disabled
avg_diffSLOW_WALK <- sum(calculationsWALK$diffSLOWmultiplied)/total_disabled
print(avg_diffCP_WALK)
print(avg_diffSLOW_WALK)
#So the rest of the PT network, e.g. bus, tram, DLR, is doing a lot of the heavy lifting
#Note this means that journeys for PwMD are more often multi-modal, complex, etc. - could be a deterrent

#Proportion of population living within 20 min of a station
pop_proportions <- fastest_time_to_stations %>%
  left_join(pop_centroids, by=c("lsoa21cd" = "id"))%>%
  dplyr::select(lsoa21cd, mean_fastest_station, mean_accessible_stationCP, mean_accessible_stationSLOW, total_pop, total_disabled)%>%
  mutate(total_non_disabled = total_pop-total_disabled)
total_non_disabled = sum(pop_proportions$total_non_disabled)
pct_under_20min_non_disabled = pop_proportions %>%
  filter(mean_fastest_station <= 20) %>%
  summarise(sum = sum(total_non_disabled)) %>%
  pull(sum)*100/total_non_disabled

total_disabled = sum(pop_proportions$total_disabled)
pct_under_20min_disabledCP = pop_proportions %>%
  filter(mean_accessible_stationCP <= 20) %>%
  summarise(sum = sum(total_disabled)) %>%
  pull(sum)*100/total_disabled
pct_under_20min_disabledSLOW = pop_proportions %>%
  filter(mean_accessible_stationSLOW <= 20) %>%
  summarise(sum = sum(total_disabled)) %>%
  pull(sum)*100/total_disabled

pct_under_20min_non_disabled
pct_under_20min_disabledCP
pct_under_20min_disabledSLOW
#Shows journey planner walking speed assumptions are bad

# ---- Display results -----

#Violin plot of time distributions
pivoted <- fastest_time_to_stations %>%
  st_drop_geometry() %>%
  dplyr::select(mean_fastest_station, mean_accessible_stationCP, mean_accessible_stationSLOW) %>%
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
#Horizontal equity: still a difference
#Vertical equity: needs considerable change

#Compare walking-only scenarios - probably won't use
pivoted <- fastest_time_to_stations %>%
  st_drop_geometry() %>%
  dplyr::select(mean_fastest_stationWALK, mean_accessible_stationWALK_CP, mean_accessible_stationWALK_SLOW) %>%
  rename(
    "Fastest Time\nto a Station" = mean_fastest_stationWALK,
    "Fastest Time to an\nAccessible Station,\nSpeed Unchanged" = mean_accessible_stationWALK_CP,
    "Fastest Time to an\nAccessible Station,\nSlower Walking Speed" = mean_accessible_stationWALK_SLOW
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
  labs(title = "Distribution of Travel Times to Stations, Walking Only",
       x = "Travel Type",
       y = "Time (minutes)") +
  ylim(0, 1000) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    axis.title.x = element_text(margin = margin(t = 10)))
#Note it extends beyond this

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
    title = "Fastest Station Status",
    textNA = "",
    border.alpha=0
  ) +
  tm_shape(boroughs)+
  tm_polygons(fill=NA, alpha=0, lwd=1.5)+
  tm_shape(tube_stations_main)+
  tm_dots(fill = "classification2", 
          fill.scale = tm_scale_categorical(values = mapping),
          fill.legend = tm_legend(title = "Station Accessibility"),
          shape=21,
          size=0.4)+
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

tmap_save(
  tm_shape(fastest_time_to_stations) +
    tm_polygons(
      col = "is_fastest_accessible",
      palette=mapping,
      alpha=0.5,
      title = "Fastest Station Status",
      textNA = "",
      border.alpha=0
    ) +
    tm_shape(boroughs)+
    tm_polygons(fill=NA, alpha=0, lwd=1.5)+
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
      legend.position = c(0.01, 0.17),
      legend.bg.color = "white",
      legend.showNA = FALSE,
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.6,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9),
  filename = "maps/nearest_station_statusNODOTS.png",
  dpi=300)

#Map ratios
#Ceteris paribus
breaks <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 2, 5, 30)
tmap_save(
  tm_shape(fastest_time_to_stations) +
    tm_polygons(
      col = "ratioCP",
      style="fixed",
      breaks=breaks,
      palette="rd_pu",
      alpha=0.9,
      title = "Ratio",
      textNA = "",
      border.alpha=0
    ) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    # tm_shape(tube_stations_main)+
    # tm_dots(fill = "classification2", 
    #         fill.scale = tm_scale_categorical(values = mapping),
    #         fill.legend = tm_legend(title = "Station Accessibility"),
    #         shape=21,
    #         size=0.4)+
    #tm_basemap("Esri.OceanBasemap") +
    tm_title("Travel Time Ratio: Fastest Station versus Fastest Step-Free Station") +
    tm_compass(type = "8star",
               size = 3,
               position = c(0.9, 0.22)) +
    tm_scalebar(
      position = c(0.82, 0.08),
      text.size = 0.7,
      breaks = c(0, 5, 10)
    ) +
    tm_layout(
      bg.color = "grey80",
      legend.outside = TRUE,
      legend.outside.position = "right",
      legend.bg.color = "white",
      legend.showNA = FALSE,
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.2,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9),
  filename = "maps/nearest_speed_ratio.png",
  dpi=300)

breaks <- c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 82)
tmap_save(
  tm_shape(fastest_time_to_stations) +
    tm_polygons(
      col = "ratioSLOW",
      style="fixed",
      breaks=breaks,
      palette="rd_pu",
      alpha=0.9,
      title = "Ratio",
      textNA = "",
      border.alpha=0
    ) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    # tm_shape(tube_stations_main)+
    # tm_dots(fill = "classification2", 
    #         fill.scale = tm_scale_categorical(values = mapping),
    #         fill.legend = tm_legend(title = "Station Accessibility"),
    #         shape=21,
    #         size=0.4)+
    # tm_basemap("Esri.OceanBasemap") +
    tm_title("Travel Time Ratio: Fastest Station versus Fastest Step-Free Station, \nSlower Walking Speed") +
    tm_compass(type = "8star",
               size = 3,
               position = c(0.9, 0.22)) +
    tm_scalebar(
      position = c(0.82, 0.08),
      text.size = 0.7,
      breaks = c(0, 5, 10)
    ) +
    tm_layout(
      bg.color = "grey80",
      legend.outside = TRUE,
      legend.outside.position = "right",
      legend.bg.color = "white",
      legend.showNA = FALSE,
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.2,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9),
  filename = "maps/nearest_speed_ratioSLOW.png",
  dpi=300)
#Note some ratios tend to be larger in areas with larger LSOAs - likely a bias due to MAUP

#Display ratio between PT and walking accessibility
fastest_time_to_stations <- fastest_time_to_stations %>%
  mutate(modal_accessibility_ratioCP = mean_accessible_stationWALK_CP/mean_accessible_stationCP,
         modal_accessibility_ratioSLOW = mean_accessible_stationWALK_SLOW/mean_accessible_stationSLOW)
summary(fastest_time_to_stations$modal_accessibility_ratioCP)
summary(fastest_time_to_stations$modal_accessibility_ratioSLOW)

breaks <- 1:8
tmap_save(
  tm_shape(fastest_time_to_stations) +
    tm_polygons(
      col = "modal_accessibility_ratioCP",
      style="fixed",
      breaks=breaks,
      palette="blue_green_sequential",
      alpha=0.9,
      title = "Ratio",
      textNA = "",
      border.alpha=0
    ) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_title("Travel Time Ratio: Time to Step-Free Stations by Walking and \nPublic Transport, versus Walking Only") +
    tm_compass(type = "8star",
               size = 3,
               position = c(0.9, 0.22)) +
    tm_scalebar(
      position = c(0.82, 0.08),
      text.size = 0.7,
      breaks = c(0, 5, 10)) +
    tm_layout(
      bg.color = "grey80",
      legend.outside = TRUE,
      legend.outside.position = "right",
      legend.bg.color = "white",
      legend.showNA = FALSE,
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.2,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9),
  filename = "maps/mode_ratioCP.png",
  dpi=300)
#Make sense that ratio is higher on periphery, because I am likely excluding non-TfL services
#But this doesn't actually show us much, because higher ratio=bus is present, which is a good thing

#Redefining this: PT time minus walking time? Absolute benefit
fastest_time_to_stations <- fastest_time_to_stations %>%
  mutate(PT_to_accessible_station_benefitCP = mean_accessible_stationWALK_CP - mean_accessible_stationCP,
         PT_to_accessible_station_benefitSLOW = mean_accessible_stationWALK_SLOW - mean_accessible_stationSLOW)
summary(fastest_time_to_stations$PT_to_accessible_station_benefitCP)
summary(fastest_time_to_stations$PT_to_accessible_station_benefitSLOW)

#Need to look for areas with low current benefit but high accessibility disparity

#Code adapted from: https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html
bi_data <- bi_class(fastest_time_to_stations, x = ratioCP, y = mean_accessible_stationWALK_CP, style = "fisher", dim = 3)
pal <- bi_pal("GrPink", dim = 3, preview = FALSE)
bi_classes <- names(pal)
tmap_save(
tm_shape(bi_data) +
  tm_polygons("bi_class",
              palette = pal,
              border.alpha = 0,
              legend.show = FALSE) +
  tm_shape(boroughs)+
  tm_polygons(lwd=1, fill=NA, alpha=0)+
  tm_compass(type = "8star",
             size = 3,
             position = c(0.9, 0.22)) +
  tm_scalebar(
    position = c(0.82, 0.08),
    text.size = 0.7,
    breaks = c(0, 5, 10)) +
  tm_title("Step-Free Accessibility Disparity versus Step-Free Benefit From Non-Rail PT")+
  tm_layout(
    title.fontfamily = "Segoe UI Semibold",
    title.size = 1.2,
    bg.color = "grey70"),
filename = "maps/bivariate_choropleth_PThelp.png",
dpi = 300)
legend <- bi_legend(
  pal = "GrPink",
  dim = 3,
  xlab = "Higher Accessibility Disparity",
  ylab = "Higher Non-Rail PT Benefit",
  size = 8)+
  theme(
    text = element_text(family = "Segoe UI", size = 8.5))
ggsave(filename = "maps/bivariate_legend_PThelp.png",
  plot = legend, dpi = 300, bg = "white")
#Need to manually combine with legend
#We are looking for the light pink/purple
#Confirms feeder buses are needed near bottom of Northern Line?

# ----- Global spatial autocorrelation -------

#Create spatial weights matrix
area_nb <- fastest_time_to_stations %>% #Queen's case
  poly2nb(., queen=T)
summary(area_nb)
area.lw <- area_nb %>% #row standardisation
  nb2listw(., style="W")

#Global Moran's I
morans_i_global_ratioCP <- fastest_time_to_stations %>%	
  pull(ratioCP) %>%
  as.vector() %>%
  moran.test(., area.lw)
morans_i_global_ratioSLOW <- fastest_time_to_stations %>%	
  pull(ratioSLOW) %>%
  as.vector() %>%
  moran.test(., area.lw)

morans_i_global_ratioCP
morans_i_global_ratioSLOW

#Geary's C
gearys_c_ratioCP <- fastest_time_to_stations %>%
  pull(ratioCP)%>%
  as.vector() %>%
  geary.test(., area.lw)
gearys_c_ratioSLOW <- fastest_time_to_stations %>%
  pull(ratioSLOW)%>%
  as.vector() %>%
  geary.test(., area.lw)

gearys_c_ratioCP
gearys_c_ratioSLOW

#Getis Ord
getis_ord_global_ratioCP <- fastest_time_to_stations %>%
  pull(ratioCP) %>%
  as.vector() %>%
  globalG.test(., area.lw)
getis_ord_global_ratioSLOW <- fastest_time_to_stations %>%
  pull(ratioSLOW) %>%
  as.vector() %>%
  globalG.test(., area.lw)

getis_ord_global_ratioCP
getis_ord_global_ratioSLOW

# ----- Local spatial autocorrelation ------

#Local Moran's I
local_morans_i_ratioCP <- fastest_time_to_stations %>%
  pull(ratioCP)%>%
  as.vector()%>%
  localmoran(., area.lw)%>%
  as_tibble
local_morans_i_ratioSLOW <- fastest_time_to_stations %>%
  pull(ratioSLOW)%>%
  as.vector()%>%
  localmoran(., area.lw)%>%
  as_tibble

#Copy the I- and z scores to the sf:
fastest_time_to_stations <- fastest_time_to_stations %>%
  mutate(density_I_ratioCP = as.numeric(local_morans_i_ratioCP$Ii))%>%
  mutate(density_Iz_ratioCP = as.numeric(local_morans_i_ratioCP$Z.Ii))%>%
  mutate(density_I_ratioSLOW = as.numeric(local_morans_i_ratioSLOW$Ii))%>%
  mutate(density_Iz_ratioSLOW= as.numeric(local_morans_i_ratioSLOW$Z.Ii))

#Plot the z scores
tmap_mode("plot")
breaks<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))
break_labels <- c("-1000 to -2.58", "-2.58 to -1.96", "-1.96 to -1.65", "-1.65 to 1.65", "1.65 to 1.96", "1.96 to 2.58", "2.58 to 1000")
fastest_time_to_stations <- fastest_time_to_stations %>%
  rename('"Standard" Walking Speed' = density_Iz_ratioCP,
         "Slower Walking Speed" = density_Iz_ratioSLOW)

tmap_save(
  tm_shape(fastest_time_to_stations) +
    tm_polygons(
      fill = c('"Standard" Walking Speed', "Slower Walking Speed"),
      style="fixed",
      breaks=breaks,
      palette=MoranColours,
      midpoint=NA,
      fill.free = FALSE,
      legend.show = FALSE,
      textNA = "",
      border.alpha=0
    ) +
    tm_facets(ncol = 2) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    #tm_add_legend(type = "fill", labels = break_labels, col = MoranColours, title="Z Score") +
    #tm_basemap("Esri.OceanBasemap") +
    tm_title("Local Moran's I Score, Travel Time Ratios") +
    tm_layout(
      legend.outside = TRUE,
      legend.outside.position = "left",
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.5,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      panel.label.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9
    ),
  filename = "maps/travel_time_local_morans.png",
  dpi = 300
)
#Combine manually with legend

#Getting old column names back - ChatGPT helped as tidyverse was having issues with it
names(fastest_time_to_stations)[
  names(fastest_time_to_stations) == '"Standard" Walking Speed'] <- "density_Iz_ratioCP"
names(fastest_time_to_stations)[
  names(fastest_time_to_stations) == "Slower Walking Speed"] <- "density_Iz_ratioSLOW"

#Local Getis Ord Gi*
Gi_local_density_ratioCP <- fastest_time_to_stations %>%
  pull(ratioCP)%>%
  as.vector()%>%
  localG(., area.lw)
Gi_local_density_ratioSLOW <- fastest_time_to_stations %>%
  pull(ratioSLOW)%>%
  as.vector()%>%
  localG(., area.lw)

fastest_time_to_stations <- fastest_time_to_stations %>%
  mutate(density_G_ratioCP = as.numeric(Gi_local_density_ratioCP),
         density_G_ratioSLOW = as.numeric(Gi_local_density_ratioSLOW))

#Plot the z-scores
GIColours<- rev(brewer.pal(8, "RdBu"))
fastest_time_to_stations <- fastest_time_to_stations %>%
  rename('"Standard" Walking Speed' = density_G_ratioCP,
         "Slower Walking Speed" = density_G_ratioSLOW)

names(fastest_time_to_stations)[
  names(fastest_time_to_stations) == "density_G_ratioCP"] <- '"Standard" Walking Speed'
names(fastest_time_to_stations)[
  names(fastest_time_to_stations) == "density_G_ratioSLOW"] <- "Slower Walking Speed"


tmap_save(
  tm_shape(fastest_time_to_stations) +
    tm_polygons(
      fill = c('"Standard" Walking Speed', "Slower Walking Speed"),
      style="fixed",
      breaks=breaks,
      palette=GIColours,
      midpoint=NA,
      fill.free = FALSE,
      legend.show = FALSE,
      textNA = "",
      border.alpha=0
    ) +
    tm_facets(ncol = 2) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    #tm_add_legend(type = "fill", labels = break_labels, col = GIColours, title="Z Score") +
    #tm_basemap("Esri.OceanBasemap") +
    tm_title("Local Getis Ord Gi* Score, Travel Time Ratios") +
    tm_layout(
      legend.outside = TRUE,
      legend.outside.position = "left",
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.5,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      panel.label.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9
    ),
  filename = "maps/travel_time_local_getis.png",
  dpi = 300
)
#Combine manually with legend

names(fastest_time_to_stations)[
  names(fastest_time_to_stations) == '"Standard" Walking Speed'] <- "density_G_ratioCP"
names(fastest_time_to_stations)[
  names(fastest_time_to_stations) == "Slower Walking Speed"] <- "density_G_ratioSLOW"

# ------ Bivariate Spatial Autocorrelaton ------

#Join benefit index
fastest_time_to_stations <- fastest_time_to_stations %>%
  left_join(., (pop_centroids %>% dplyr::select(id, step_free_benefit_indexW)), by = c("lsoa21cd" = "id"))

#Find association (not accounting for autocorrelation)
cor.test(fastest_time_to_stations$ratioCP, fastest_time_to_stations$step_free_benefit_indexW)
cor.test(fastest_time_to_stations$ratioSLOW, fastest_time_to_stations$step_free_benefit_indexW)
#Weak negative linear association (statistically significant)

#Bivariate LISA
set.seed(10)
bv_moranCP <- localmoran_bv(fastest_time_to_stations$step_free_benefit_indexW, fastest_time_to_stations$ratioCP, area.lw, nsim = 999)
bv_moranSLOW <- localmoran_bv(fastest_time_to_stations$step_free_benefit_indexW, fastest_time_to_stations$ratioSLOW, area.lw, nsim = 999)

fastest_time_to_stations$hs_CP <- hotspot(bv_moranCP, Prname="Pr(folded) Sim", cutoff=0.05,
                       quadrant.type="pysal", p.adjust="none")
fastest_time_to_stations$hs_SLOW <- hotspot(bv_moranSLOW, Prname="Pr(folded) Sim", cutoff=0.05,
                                          quadrant.type="pysal", p.adjust="none")

fastest_time_to_stations <- fastest_time_to_stations %>%
  mutate(hs_CP = if_else(is.na(hs_CP), "Not Significant", hs_CP),
         hs_SLOW = if_else(is.na(hs_SLOW), "Not Significant", hs_SLOW))%>%
  rename('"Standard" Walking Speed' = hs_CP,
         "Slower Walking Speed" = hs_SLOW)

bivariate_cols <- c("High-High" = "#d7191c", "Low-Low" = "#2c7bb6", "High-Low" = "#ffdd94", "Low-High" = "#abd9e9", "Not Significant" = "#f0f0f0")
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant")

tmap_save(
  tm_shape(fastest_time_to_stations) +
    tm_polygons(
      fill = c('"Standard" Walking Speed', "Slower Walking Speed"),
      palette=bivariate_cols,
      midpoint=NA,
      fill.free = FALSE,
      legend.show = FALSE,
      textNA = "",
      border.alpha=0
    ) +
    tm_facets(nrow = 2) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    #tm_add_legend(type = "fill", labels = labels, col = bivariate_cols, title="Classification") +
    #tm_basemap("Esri.OceanBasemap") +
    tm_title("In-Need Population Against Travel Time Ratio") +
    tm_layout(
      bg.color = "grey70",
      legend.outside = TRUE,
      legend.outside.position = "left",
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.2,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      panel.label.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9
    ),
  filename = "maps/time_bivariate_morans_i.png",
  dpi = 300
)

names(fastest_time_to_stations)[
  names(fastest_time_to_stations) == "\"Standard\" Walking Speed"] <- "hs_CP"
names(fastest_time_to_stations)[
  names(fastest_time_to_stations) == "Slower Walking Speed"] <- "hs_SLOW"

#Bivariate choropleth: accessibility ratio versus proportion

#Need to add some slight noise to the data so we can add quantiles
bivariate_data <- fastest_time_to_stations %>%
  dplyr::select(lsoa21cd, ratioCP, step_free_benefit_indexW)%>%
  mutate(ratioCP_jitter = jitter(ratioCP, amount = 1e-6)) #ChatGPT helped!

bi_data <- bi_class(bivariate_data, x = ratioCP_jitter, y = step_free_benefit_indexW, style = "quantile", dim = 4)
pal <- bi_pal("GrPink2", dim = 4, preview = FALSE)
bi_classes <- names(pal)
tmap_save(
  tm_shape(bi_data) +
    tm_polygons("bi_class",
                palette = pal,
                border.alpha = 0,
                legend.show = FALSE) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_compass(type = "8star",
               size = 3,
               position = c(0.9, 0.22)) +
    tm_scalebar(
      position = c(0.82, 0.08),
      text.size = 0.7,
      breaks = c(0, 5, 10)) +
    tm_title("Step-Free Accessibility Disparity versus Presence of In-Need Population")+
    tm_layout(
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.2,
      bg.color = "grey70"),
  filename = "maps/bivariate_choropleth_disparity_pop.png",
  dpi = 300)
legend <- bi_legend(
  pal = "GrPink2",
  dim = 4,
  xlab = "Higher Accessibility Disparity",
  ylab = "Higher In-Need Population",
  size = 5)+
  theme(
    text = element_text(family = "Segoe UI"))
ggsave(filename = "maps/bivariate_legend_disparity_pop.png",
       plot = legend, dpi = 300, bg = "white")
#Need to manually combine with legend
#We are looking for higher colours!
#Difficult to compare with SLOW values because we cannot do much about changed accessibility from slower walking speeds

# ---- Clustering ------
#https://www.datacamp.com/tutorial/hierarchical-clustering-R

#We will do hierarchal clustering as k-means/medoids assumes circular clusters
#Clustering on ratioSLOW as this is more representative of real-world conditions

cluster_vars <- fastest_time_to_stations %>%
  dplyr::select(lsoa21cd, ratioSLOW, step_free_benefit_indexW)

#Plot association between variables
ggplot(cluster_vars, aes(ratioSLOW, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25)

#Explore distributions before scaling
hist(cluster_vars$ratioSLOW, 
     main = "Distribution of ratioSLOW", 
     col = "lightblue", 
     border = "black",
     breaks=100) #Positive skew - needs to be scaled
hist(cluster_vars$step_free_benefit_indexW, 
     main = "Distribution of pop index", 
     col = "red", 
     border = "black",
     breaks=100) #Relatively normal dist

symbox(~as.numeric(ratioSLOW), cluster_vars, na.rm=T, powers=seq(-3, 3, by=.5))
#None are great

#Try box-cox transformation (chatGPT helped here)
x <- as.numeric(cluster_vars$ratioSLOW)
model <- lm(x ~ 1)
boxcox_result <- boxcox(model, lambda = seq(-5, 5, 0.01))
best_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
x_trans <- (x^best_lambda - 1) / best_lambda
hist(x_trans, 
     main = "Box-Cox Transformed ratioSLOW", 
     col = "lightblue", 
     border = "black",
     breaks=100)
cluster_vars$ratioSLOW_boxcox <- x_trans

#Now both are relatively normally distributed, let's standardise them so the scales are the same
cluster_vars_numeric_scaled <- cluster_vars %>%
  dplyr::select(where(is.numeric))%>%
  st_drop_geometry()%>%
  scale()
cluster_vars_scaled <- cluster_vars %>% #Reattach to ID
  dplyr::select(lsoa21cd)%>%
  st_drop_geometry()%>%
  bind_cols(as.data.frame(cluster_vars_numeric_scaled))%>%
  dplyr::select(-ratioSLOW)
rm(cluster_vars_numeric_scaled)

ggplot(cluster_vars_scaled, aes(ratioSLOW_boxcox, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25)

#Calculate distances and cluster
set.seed(10)
dist_mat <- dist(cluster_vars_scaled %>% dplyr::select(where(is.numeric)), method = "euclidean")
hc <- hclust(dist_mat, method = 'complete') #only way to get defined clusters

#Silhouette scores
max_k <- 15
avg_sil <- numeric(max_k)

for (k in 2:max_k) {
  clusters <- cutree(hc, k)
  sil <- silhouette(clusters, dist_mat)
  avg_sil[k] <- mean(sil[, 3])}

#Plot
sil_data <- data.frame(
  k = 2:max_k,
  avg_sil = avg_sil[2:max_k])
ggplot(sil_data, aes(x = k, y = avg_sil)) +
  geom_point(color = "deeppink3", size = 2) +        
  geom_line(color = "deeppink3", size=0.5, linetype="dashed") + 
  scale_x_continuous(breaks = 2:max_k)+
  labs(
    title = "Cluster Silhouette Analysis",
    x = "Number of clusters",
    y = "Average silhouette width") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust = 0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size = 12),
    axis.text = element_text(family = "Segoe UI", size = 10))

tree_cut <- cutree(hc, k = 6)

#Plot dendrogram
hc_obj <- as.dendrogram(hc)
dend_plot <- color_branches(hc_obj, k=6)
plot(dend_plot)

#Add back to data
cluster_vars <- mutate(cluster_vars, cluster = tree_cut)

#Plot transformed clusters
cols <- brewer.pal(6, "Dark2")
ggplot(cluster_vars, aes(x=ratioSLOW_boxcox, y=step_free_benefit_indexW, color = factor(cluster)))+
  geom_point()+
  labs(title = "Transformed Cluster Output",
       x = "Box-Cox Transformed Travel Time Ratio, Slower Walking Speed",
       y = "In-Need Population Index",
       color = "Cluster")+
  theme_minimal() +
  scale_color_manual(values = cols) +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9))
  
#Plot untransformed output
ggplot(cluster_vars, aes(x=ratioSLOW, y=step_free_benefit_indexW, color = factor(cluster)))+
  geom_point()+
  labs(title = "Cluster Output, Untransformed",
       x = "Travel Time Ratio, Slower Walking Speed",
       y = "In-Need Population Index",
       color = "Cluster")+
  theme_minimal() +
  scale_color_manual(values = cols) +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9))

#Map
cluster_vars$cluster <- factor(cluster_vars$cluster)
tmap_save(
  tm_shape(cluster_vars) +
    tm_polygons(
      col = "cluster",
      palette=cols,
      title = "Cluster",
      textNA = "",
      alpha=0.8,
      border.alpha=0) +
    tm_shape(boroughs)+
    tm_polygons(fill=NA, alpha=0, lwd=1.5)+
    tm_basemap("Esri.OceanBasemap") +
    tm_title("LSOAs by Accessibility Need Cluster") +
    tm_compass(type = "8star",
               size = 3,
               position = c(0.9, 0.22)) +
    tm_scalebar(
      position = c(0.82, 0.08),
      text.size = 0.7,
      breaks = c(0, 5, 10)
    ) +
    tm_layout(
      legend.position = c(0.01, 0.33),
      legend.bg.color = "white",
      legend.showNA = FALSE,
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.6,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9),
  filename = "maps/ratio_pop_clusters.png",
  dpi=300)

# cluster_vars <- cluster_vars %>%
#   dplyr::select(-cluster)

#Note I haven't done it with ratioCP as I couldn't create enough distance between the ratios of 1 and other values

# ---- Station Catchment Analysis ------

#Join data
station_catchments <- fastest_time_to_stations %>%
  dplyr::select(lsoa21cd, lsoa21nm, fastest_station, ratioCP, ratioSLOW) %>%
  left_join(cluster_vars %>% st_drop_geometry() %>% dplyr::select(lsoa21cd, cluster), by = "lsoa21cd")%>%
  left_join(pop_centroids %>% dplyr::select(id, total_pop, total_under_5, total_65_plus, total_disabled), by = c("lsoa21cd" = "id"))%>%
  mutate(total_in_need_pop = total_under_5 + total_65_plus + total_disabled)%>%
  dplyr::select(-total_under_5, -total_65_plus, -total_disabled)%>%
  left_join(tube_stations_main %>% dplyr::select(stop_id, stop_name, classification, upgrade_status)%>%st_drop_geometry(), by=c("fastest_station"="stop_id"))
  
station_catchments_summary <- station_catchments %>%
  group_by(fastest_station, stop_name, classification, upgrade_status) %>%
  summarise(
    total_population = sum(total_pop),
    total_in_need_population = sum(total_in_need_pop),
    total_in_need_cluster_5 = sum(total_in_need_pop[cluster == 5]),
    total_in_need_cluster_4_or_5 = sum(total_in_need_pop[cluster %in% c(4, 5)]),
    mean_ratioCP = mean(ratioCP),
    mean_ratioSLOW = mean(ratioSLOW))%>%
  mutate(pct_in_need = 100*total_in_need_population/total_population)%>%
  filter(classification != "Fully Accessible")

#We can see that the stations TfL are currently exploring typically have larger catchment populations
#But not necessarily high disparities

#Compare stations TfL are exploring to the rest
station_catchments_summary <- station_catchments_summary %>%
  mutate(upgrade_status2 = if_else(upgrade_status == "No Plans", "No Plans", "Potential Upgrade"))

#Pivot data for faceted violin plot
labels <- c(
  total_population = "Total Population",
  total_in_need_population = "In-Need Population",
  pct_in_need = "Proportion of In-Need Population",
  total_in_need_cluster_5 = "In-Need Population, Cluster 5",
  total_in_need_cluster_4_or_5 = "In-Need Population, Clusters 4 & 5",
  mean_ratioCP = "Mean Accessibility Ratio",
  mean_ratioSLOW = "Mean Accessibility Ratio, \nSlower Walking Speed")
numeric_vars <- station_catchments_summary %>%
  st_drop_geometry() %>%
  dplyr::select(fastest_station, upgrade_status2, where(is.numeric)) %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "variable",
    values_to = "value")%>%
  mutate(
    variable = factor(variable, levels = names(labels), labels = labels))

#Create violin plot faceted by variable, split by upgrade_status2
ggplot(numeric_vars, aes(x = upgrade_status2, y = value, fill = upgrade_status2)) +
  geom_violin(trim = FALSE) +
  stat_summary(aes(color = upgrade_status2),
               fun = median,
               geom = "crossbar",
               linetype = "dashed",
               size = 0.2,
               show.legend = FALSE) +
  facet_wrap(~ variable, scales = "free_y", ncol=2) +
  scale_fill_brewer(palette = "Set1")+
  guides(color = "none") +
  theme_minimal() +
  labs(
    title = "Catchment Properties of Non-Fully-Accessible Stations",
    x = NULL,
    y = "Value",
    fill = "Upgrade Status",
    caption = "Dashed lines represent distribution medians.") +
  theme(
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    legend.position = c(0.7, 0.13),
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9),
    strip.text = element_text(family = "Segoe UI", size = 9),
    plot.caption = element_text(family = "Segoe UI Light", size = 8, hjust=0))

#Quick cluster map with stations
tmap_mode("view")
tm_shape(cluster_vars)+
  tm_polygons("cluster", alpha=0.8, border.alpha=0)+
  tm_shape(boroughs)+
  tm_polygons(lwd=0.5, fill=NA, alpha=0)+
  tm_shape(tube_stations_main %>% filter(classification != "Fully Accessible"))+
  tm_dots(col="upgrade_status", palette="Dark2")
  
# ---- Prioritise stations -----
#XXX multi-criteria analysis - many variables, ranked or z-score weighted?

# #To explore bivariate choropleth manually:
# tmap_mode("view")
# tm_shape(bi_data) +
#   tm_polygons("bi_class",
#               palette = pal,
#               border.alpha = 0,
#               legend.show = FALSE) +
#   tm_shape(boroughs)+
#   tm_polygons(lwd=1, fill=NA, alpha=0)+
#   tm_shape(tube_stations_main)+
#   tm_dots(fill="classification")


#To do:
# - Identify ideal stations
  # - Could do a multi-criteria analysis
# - Run OTP directly in Java?!
  #  java -Xmx2G -jar otp-2.2.0-shaded.jar --load graphs/accessible --serve
  # - Or a for-loop, but unlikely to run on time
  # - Or healthcare access? But dataset issues
# - Or basic r5r where all non-fully-accessible stations are totally removed?
# - New scenarios?
  # - Compare shortlisted
  # - Top tube stations by usage
  # - Compare centrality measure (simplified network - e.g. no buses, accessibility binary)
  # - Top catchment - clusters
  # - How to assess?
#Clustering
  # - Could I do an easier variable?
  # - Find areas with a high proportion of in-need population which might be missed by TfL's approach
# - Redo upgrade status map with borough boundaries instead of LSOAs

#Need to consider the issue that some travel times are really unrealistic
#Links to very long walks due to remote centroids, a lack of non-TfL PT, and certain roads which are non-pedestrian-accessible

#Note "fastest" may not actually be in practice - consider issues for PwMD on buses, e.g. no space, ramps
#Or closest accessible may not actually be ideal - e.g. further away from Zone 1
