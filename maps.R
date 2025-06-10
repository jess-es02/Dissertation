library(tidyverse)
library(sf)
library(gtfstools)
library(tmap)
library(tmaptools)
library(maptiles)
library(igraph)
library(extrafont)

gtfs <- gtfstools::read_gtfs("final_r5r/gtfs_accessible.zip")
summary(gtfs)

# ----- Categorising tube stops --------

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
  mutate(station_group = ifelse(location_type == 1, stop_id, parent_station)) %>%
  group_by(station_group) %>%
  summarise(stops_in_station = list(stop_id), .groups = 'drop')
  
components <- components(G)
  
connectivity_results <- station_groups %>%
  rowwise() %>%
  mutate(
    stops_in_graph = list(intersect(stops_in_station, V(G)$name)),
    classification = ifelse(length(stops_in_graph) == 0, "Inaccessible", {
      #Check if all stops are in the same component
      stop_components <- components$membership[stops_in_graph]
      if (length(unique(stop_components)) == 1) {
        "Fully Accessible"
      } else {
        "Partially Accessible"
      }
    })
  ) %>%
  ungroup()%>%
  select(station_group, classification)

tube_stations_main <- tube_stations_main %>%
  left_join(connectivity_results, by = c("stop_id" = "station_group"))%>%
  st_as_sf(., coords = c("stop_lon", "stop_lat"), crs=4326)%>%
  st_transform(., 27700)%>%
  select(stop_id, stop_name, classification, geometry)
#Note that classifications are an overstatement - not accounting for gap between platform and train, length of interchange, different entrances, etc.
#Also note that some classifications seem to differ to TfL data on its step-free map - however, this looks like it's due to problems in the pathways data they provided, rather than computational problems

rm(all_edges, components, connectivity_results, edges, G, station_groups, tube_stops)

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

#Re-check classification - why does 910GHTCHEND differ from web?