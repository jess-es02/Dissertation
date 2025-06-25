#General maps and summary statistics on station accessibility

#In this file, we:
  # - Categorise stations into accessibility types according to pathways.txt
  # - Map stations and the study area
  # - Find some general summary statistics on station accessibility type

# Note that other maps are located in other files:
# 1) job_comparison
  # - Job distribution
# 2) analysis_r5r
  # - Binary of nearest station
  # - Accessibility ratio (CP and slower walking speed)
  # - Local spatial autocorrelation
  # - Bivariate LISA

library(tidyverse)
library(sf)
library(gtfstools)
library(tmap)
library(tmaptools)
library(maptiles)
library(igraph)
library(extrafont)
library(ggplot2)

gtfs <- gtfstools::read_gtfs("final_r5r/gtfs_accessibleBAT1.zip")
summary(gtfs)

# ----- Stop classification --------

#For visualising the network, we want to classify stops as fully, partially, or not accessible

#Extract stops on the tube/Overground/Lizzie line
tube_stations_main <- gtfs$stops %>%
  filter(location_type == 1)
tube_stops <- gtfs$stops %>%
  filter(stop_id %in% tube_stations_main$stop_id | parent_station %in% tube_stations_main$stop_id)

#Build graph from pathways.txt
edges <- gtfs$pathways %>%
  rename("from" = from_stop_id, 
         "to" = to_stop_id) %>%
  select(from, to, is_bidirectional)
all_edges <- bind_rows(
  edges,
  edges %>% filter(is_bidirectional == 1) %>%
    transmute(from = to, to = from, is_bidirectional))
G <- graph_from_data_frame(all_edges, directed = TRUE)

station_groups <- tube_stops %>%
  filter(location_type != 1) %>%  #exclude parent station ids, as these aren't included in pathways.txt
  mutate(station_group = parent_station) %>%
  group_by(station_group) %>%
  summarise(stops_in_station = list(stop_id), .groups = 'drop')
  
components <- components(G)
  
connectivity_results <- station_groups %>%
  rowwise() %>%
  mutate(
    stops_in_graph = list(intersect(stops_in_station, V(G)$name)),
    classification = {
      if (length(stops_in_graph) == 0) {
        "Inaccessible"
      } else if (length(stops_in_graph) < length(stops_in_station)) {
        "Partially Accessible"
      } else {
        stop_components <- components$membership[stops_in_graph]
        if (length(unique(stop_components)) == 1) {
          "Fully Accessible"
        } else {
          "Partially Accessible"
        }
      }
    }
  ) %>%
  ungroup()%>%
  select(station_group, classification)

tube_stations_main <- tube_stations_main %>%
  left_join(connectivity_results, by = c("stop_id" = "station_group"))%>%
  st_as_sf(., coords = c("stop_lon", "stop_lat"), crs=4326)%>%
  st_transform(., 27700)%>%
  select(stop_id, stop_name, classification, geometry)
#Note that classifications are an overstatement - not accounting for gap between platform and train, length of interchange, different entrances, etc.
#Also note that some classifications (e.g. Peckham Rye) seem to differ to TfL data on its step-free map - however, this looks like it's due to problems in the pathways data they provided, rather than computational problems

#Manually class Battersea Park as inaccessible - it is completely missing from TfL data, but online it says no platforms are accessible
tube_stations_main <- tube_stations_main %>%
  mutate(classification = if_else(stop_id == 'BATRSPK', 'Inaccessible', classification))

rm(all_edges, components, connectivity_results, edges, G, station_groups, tube_stops)

#Add stations TfL is considering for upgrades
#https://tfl.gov.uk/travel-information/improvements-and-projects/step-free-access
#https://tfl.gov.uk/info-for/media/press-releases/2024/august/tfl-confirms-the-next-12-tube-stations-to-be-prioritised-for-step-free-access

ongoing_work <- c("940GZZLUALP", "940GZZLUASG", "940GZZLUEAE", "940GZZLUNAN", "910GWHMDSTD", "940GZZLUNHT", "940GZZLUCND", "940GZZLULYN", "910GSURREYQ")
upgrades_stalled <- c("910GBRBY", "910GHAKNYNM", "910GPCKHMRY", "HUBSVS")
under_evaluation <- c("940GZZLUCXY", "940GZZLUEFY", "940GZZLUNDN", "940GZZLUNOW", "940GZZLUTNG", "940GZZLUCSD", "940GZZLUTBY")
tube_stations_main <- tube_stations_main %>%
  mutate(upgrade_status = case_when(
    stop_id %in% ongoing_work ~ "Project Underway",
    stop_id %in% under_evaluation ~ "Under Evaluation",
    stop_id %in% upgrades_stalled ~ "Project Stalled",
    classification == "Fully Accessible" ~ "Already Accessible",
    TRUE ~ "No Plans"))
rm(ongoing_work, under_evaluation, upgrades_stalled)

#Join to fare zone data
tfl_fare_zones <- read_csv("data/tfl_station_data_detailed/Stations.csv")%>%
  clean_names()%>%
  select(unique_id, fare_zones)
tube_stations_main <- tube_stations_main %>%
  left_join(tfl_fare_zones, by=c("stop_id" = "unique_id"))%>%
  mutate(fare_zones = if_else(stop_id == 'BATRSPK', "2", fare_zones))
rm(tfl_fare_zones)

# ----- Plot classifications -----

#Load GLA boundary for map
boundary <- st_read("data/London_GLA_Boundary.shp")%>%
  st_simplify()

bbox_stations <- st_bbox(tube_stations_main)
bbox_boundary <- st_bbox(boundary)
bbox_combined <- st_as_sfc(st_bbox(c(
  xmin = min(bbox_stations["xmin"], bbox_boundary["xmin"]),
  ymin = min(bbox_stations["ymin"], bbox_boundary["ymin"]),
  xmax = max(bbox_stations["xmax"], bbox_boundary["xmax"]),
  ymax = max(bbox_stations["ymax"], bbox_boundary["ymax"])
), crs = st_crs(27700)))

#Plot
mapping <- c(
  "Fully Accessible" = "darkgreen",
  "Partially Accessible" = "gold",
  "Inaccessible" = "red")

tmap_mode("plot")
tmap_options(component.autoscale = TRUE)

tmap_save(
tm_shape(boundary, bbox = bbox_combined) +
  tm_lines()+
  tm_shape(tube_stations_main)+
  tm_dots(size=0.45,
          shape=21,
          fill = "classification", 
          fill.scale = tm_scale_categorical(values = mapping),
          fill.legend = tm_legend(title = "Accessibility Status"),
          lwd=0.2)+
  tm_basemap("Esri.OceanBasemap")+
  tm_title("Step-Free Accessibility of TfL Underground and Rail Services")+
  tm_compass(type="8star", size=3, position = c(0.9, 0.2))+
  tm_scalebar(position = c(0.25, 0.11), text.size=0.7, breaks=c(0, 5, 10, 15))+
  tm_layout(legend.position = c("left", "bottom"), 
            legend.bg.color="white",
            title.fontfamily = "Segoe UI Semibold",
            legend.text.fontfamily = "Segoe UI",
            legend.title.fontfamily = "Segoe UI Semibold",
            legend.text.size = 1,   
            legend.title.size = 1.1),
  filename = "maps/stations.png",
  dpi=300)
#Could also use Segoe UI Light for other features

#Quick map without Lizzie line (potentially more reflective of study focus)
#Extract only tube/Overground stops
to_trips <- gtfs$routes %>%
  filter(route_type == 1 | #tube
         (route_type == 2 & agency_id == 'LO'))%>% #overground
  left_join(., gtfs$trips, by ="route_id")%>%
  distinct()
stops_no_lizzie <- to_trips %>%
  select(trip_id)%>%
  left_join(., gtfs$stop_times, by="trip_id")%>%
  select(stop_id)%>%
  distinct()%>%
  left_join(., gtfs$stops, by="stop_id")%>%
  mutate(parent_station = if_else(stop_id == 'BATRSPK', 'BATRSPK', parent_station))%>% #fix battersea park
  distinct(parent_station)%>%
  left_join(., tube_stations_main, by=c("parent_station" = "stop_id"))%>%
  st_as_sf()
bbox_stations <- st_bbox(stops_no_lizzie)
bbox_combined <- st_as_sfc(st_bbox(c(
  xmin = min(bbox_stations["xmin"], bbox_boundary["xmin"]),
  ymin = min(bbox_stations["ymin"], bbox_boundary["ymin"]),
  xmax = max(bbox_stations["xmax"], bbox_boundary["xmax"]),
  ymax = max(bbox_stations["ymax"], bbox_boundary["ymax"])
), crs = st_crs(27700)))
#Plot
tmap_save(
  tm_shape(boundary, bbox=bbox_combined) +
    tm_lines()+
    tm_shape(stops_no_lizzie)+
    tm_dots(size=0.45,
            shape=21,
            fill = "classification", 
            fill.scale = tm_scale_categorical(values = mapping),
            fill.legend = tm_legend(title = "Accessibility Status"))+
    tm_basemap("Esri.OceanBasemap")+
    tm_title("Step-Free Accessibility of TfL Underground and Overground Services")+
    tm_compass(type="8star", size=3, position = c(0.9, 0.22))+
    tm_scalebar(position = c(0.75, 0.08), text.size=0.7, breaks=c(0, 5, 10, 15))+
    tm_layout(legend.position = c("left", "bottom"), 
              legend.bg.color="white",
              title.fontfamily = "Segoe UI Semibold",
              legend.text.fontfamily = "Segoe UI",
              legend.title.fontfamily = "Segoe UI Semibold",
              legend.text.size = 1,   
              legend.title.size = 1.1),
  filename = "maps/stations_nolizzie.png",
  dpi=300)

rm(to_trips, bbox_boundary, bbox_stations)

# ------ Study area map -------
#This assumes lsoa_processing has been run first

tmap_save(
tm_shape(study_lsoas) +
  tm_polygons(col = "cadetblue", 
              alpha=0.3,
              border.col = "bisque4") +
  tm_add_legend(type = "polygons",
                labels = "LSOAs",
                fill = "cadetblue",
                col = "bisque4") +
  tm_shape(stops_no_lizzie) +
  tm_dots(col = "white", 
          border.col="black",
          col.legend = tm_legend(title = ""),
          shape=21,
          size=0.6) +
  tm_add_legend(type = "dots",
                labels = "Stations",
                col="black") +
  tm_basemap("Esri.OceanBasemap") +
  tm_title("Research Area of Interest") +
  tm_compass(type = "8star", 
             size = 3, 
             position = c(0.9, 0.22)) +
  tm_scalebar(position = c(0.82, 0.08), 
              text.size = 0.7, 
              breaks = c(0, 5, 10))+
  tm_layout(legend.position = c("left", "bottom"), 
            legend.bg.color="white",
            title.fontfamily = "Segoe UI Semibold",
            title.size = 1.6,
            legend.text.fontfamily = "Segoe UI",
            legend.title.fontfamily = "Segoe UI Semibold",
            legend.text.size = 1.1,   
            legend.title.size = 1.1),
  filename = "maps/study_area.png",
  dpi=300)

#The same, but mapped according to station classification
tmap_save(
  tm_shape(study_lsoas) +
    tm_polygons(col = "cadetblue", 
                alpha=0.3,
                border.col = "bisque4") +
    tm_add_legend(type = "polygons",
                  labels = "LSOAs",
                  fill = "cadetblue",
                  col = "bisque4") +
    tm_shape(stops_no_lizzie) +
    tm_dots(fill = "classification", 
            fill.scale = tm_scale_categorical(values = mapping),
            fill.legend = tm_legend(title = "Station"),
            shape=21,
            size=0.6) +
    tm_basemap("Esri.OceanBasemap") +
    tm_title("Research Area of Interest") +
    tm_compass(type = "8star", 
               size = 3, 
               position = c(0.9, 0.22)) +
    tm_scalebar(position = c(0.82, 0.08), 
                text.size = 0.7, 
                breaks = c(0, 5, 10))+
    tm_layout(legend.position = c("left", "bottom"), 
              legend.bg.color="white",
              title.fontfamily = "Segoe UI Semibold",
              title.size = 1.6,
              legend.text.fontfamily = "Segoe UI",
              legend.title.fontfamily = "Segoe UI Semibold",
              legend.text.size = 0.9,   
              legend.title.size = 1),
  filename = "maps/study_area_station_classified.png",
  dpi=300)

# ----- Map Upgrade Plans -------
mapping <- c("Project Underway" = "#9f13eb", 
             "Project Stalled" = "#f5b642",
             "Under Evaluation" = "#e0e33b", 
             "No Plans" = "#d41e11", 
             "Already Accessible" = "#f0f0f0")

#Turn column to a factor for plotting
tube_stations_main <- tube_stations_main %>%
  mutate(upgrade_status = factor(
    upgrade_status,
    levels = c("Project Underway", "Project Stalled", "Under Evaluation", "No Plans", "Already Accessible")))

tmap_save(
  tm_shape(study_lsoas) +
    tm_polygons(col = "cadetblue", 
                alpha=0.2,
                border.col = "bisque4") +
    tm_shape(tube_stations_main) +
    tm_dots(fill = "upgrade_status", 
            fill.scale = tm_scale_categorical(values = mapping),
            fill.legend = tm_legend(title = "Upgrade Status"),
            shape=21,
            size=0.6) +
    tm_basemap("Esri.OceanBasemap") +
    tm_title("Stations by TfL Step-Free Upgrade Status") +
    tm_compass(type = "8star", 
               size = 3, 
               position = c(0.9, 0.22)) +
    tm_scalebar(position = c(0.82, 0.08), 
                text.size = 0.7, 
                breaks = c(0, 5, 10))+
    tm_layout(legend.position = c("left", "bottom"), 
              legend.bg.color="white",
              title.fontfamily = "Segoe UI Semibold",
              title.size = 1.6,
              legend.text.fontfamily = "Segoe UI",
              legend.title.fontfamily = "Segoe UI Semibold",
              legend.text.size = 0.9,   
              legend.title.size = 1),
  filename = "maps/tfl_upgrade_plans.png",
  dpi=300)
rm(mapping)

# ------- Station Classification Summary Statistics ------

#1) Upgrades by fare zone
tube_stations_main %>% filter(upgrade_status=="Project Underway") %>% select(fare_zones)
tube_stations_main %>% filter(upgrade_status=="Project Stalled") %>% select(fare_zones)
tube_stations_main %>% filter(upgrade_status=="Under Evaluation") %>% select(fare_zones)

#2) Work out accessibility split by line
gtfs_stops <- gtfs$stops %>%
  mutate(parent_station = na_if(parent_station, "")) %>%
  filter(location_type == 0 & !is.na(parent_station))
#Note Battersea Park is not accessible - need to manually add to statistics

#Join accessibility status
gtfs_stops <- gtfs_stops %>%
  left_join(tube_stations_main, by=c("parent_station" = "stop_id"))

#Extract line
gtfs_stops <- gtfs_stops %>%
  mutate(
    all_lines = str_extract(stop_id, "[^-]+$"),
    lines = str_split(all_lines, "\\|"))%>%
  unnest(lines) %>% #if multiple lines, add new row
  distinct(parent_station, lines, .keep_all = TRUE)%>%
  select(stop_name.x, lines, classification, upgrade_status, geometry)%>%
  rename("stop_name" = stop_name.x)

gtfs_stops <- gtfs_stops %>%
  mutate(lines = str_to_title(lines))

gtfs_stops <- gtfs_stops %>%
  filter(lines != "Rail")
  
#Create stacked bar chart
mapping <- c(
  "Fully Accessible" = "darkgreen",
  "Partially Accessible" = "gold",
  "Inaccessible" = "red")
gtfs_stops <- gtfs_stops %>%
  mutate(classification = factor(classification, levels = c(
    "Inaccessible",
    "Partially Accessible",
    "Fully Accessible")))
ggplot(gtfs_stops, aes(x = lines, fill = classification)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = mapping) +
  labs(
    x = "Line",
    y = "Number of Stops",
    fill = "Accessibility Status",
    title = "Station Accessibility Status by Line") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
        axis.title = element_text(family = "Segoe UI Semibold", size=11),
        axis.text = element_text(family = "Segoe UI", size=9),
        legend.title = element_text(family = "Segoe UI Semibold", size=11),
        legend.text = element_text(family = "Segoe UI", size=9))
#Add note that it's about overall station classification, not the platforms for that line
rm(mapping, gtfs_stops)
