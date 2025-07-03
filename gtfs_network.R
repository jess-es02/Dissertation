#Network analyis on non-step-free stations

#In this file we:
  # - Simplify the TfL network to tube/rail only, and represent it as a graph
  # - Remove non-step-free stations
  # - Iteratively add stations and calculate centrality/network efficiency measures

#Note considerable simplifications:
  # - Only considering tube, Overground, and Lizzie lines (when we know buses play a key role in reducing disparities)
  # - Depicting accessibility as a binary

#Beforehand, ensure to run:
  # 1) lsoa_processing.R
  # 2) maps_summary_stats.R
  # Alongside loading packages from analysis_r5r.R

# ---- Prepare Simplified GTFS -----

#Load in GTFS
gtfs <- gtfstools::read_gtfs("final_r5r/gtfs.zip")
summary(gtfs)

#Filter by modes and study date (not filtering by time as some journeys are v long)
gtfs <- filter_by_weekday(gtfs, c("wednesday"))%>%
  filter_by_route_type(c(1, 2)) #tube, rail

#Filter out DLR (classed as rail)
gtfs$stops <- gtfs$stops %>%
  filter(stop_code!="")
gtfs$stop_times <- gtfs$stop_times %>%
  filter(stop_id %in% gtfs$stops$stop_id)
gtfs$trips <- gtfs$trips %>%
  filter(trip_id %in% gtfs$stop_times$trip_id)
gtfs$routes <- gtfs$routes %>%
  filter(route_id %in% gtfs$trips$route_id)

#Re-simplify GTFS stops, as we will probably just be representing station accessibility as a binary
#Update stops.txt and stop_times.txt with stop codes, rather than platform IDs
gtfs$stop_times <- gtfs$stop_times %>%
  left_join(gtfs$stops %>% dplyr::select(stop_id, stop_code), by = "stop_id") %>%
  mutate(stop_id = stop_code) %>%
  dplyr::select(-stop_code)
gtfs$stops <- gtfs$stops %>%
  mutate(stop_id = stop_code) %>%
  group_by(stop_id) %>%
  slice(1) %>%
  ungroup()
  
output_path <- tempfile("validation_result")
validator_path <- download_validator(tempdir())
gtfstools::validate_gtfs(gtfs, output_path, validator_path) #all looks good
rm(output_path, validator_path)

# ----- Convert to Graph ------

#Import Raphael Pereira function - source code https://github.com/rafapereirabr/gtfs_to_igraph
source("gtfs_to_igraph.R")

#Add necessary GTFS columns for function
gtfs$stops <- gtfs$stops %>%
  mutate(location_type = 0,
         parent_station = stop_id)
gtfs$trips <- gtfs$trips %>%
  mutate(direction_id = 0)

#Export GTFS to a zip file
gtfs_write(gtfs, folder = "final_r5r", name = "gtfs_tube_only")
my_gtfs_feeds <- list("final_r5r/gtfs_tube_only.zip")

G <- gtfs_to_igraph(list_gtfs = my_gtfs_feeds, dist_threshold=0, save_muxviz=FALSE)
rm(my_gtfs_feeds)

vertex_attr_names(G)
edge_attr_names(G)

#The function sometimes has duplicate edges between stops, which can occur if they are on the same tube line but different "routes"
#We want to combine these: keeping avg_travel_time the same but summing "weight" (i.e. frequency)
edges_df <- as_data_frame(G, what = "edges")
edges_combined <- edges_df %>%
  group_by(from, to, avg_travel_time) %>%
  summarise(weight = sum(weight), .groups = "drop")

#Combine "weight" (i.e. frequency) and avg_travel_time into a single weight
edges_combined <- edges_combined %>%
  mutate(weight_combined = weight / avg_travel_time)%>%
  filter(from != to)

#Add accessibility status to vertices
vertices <- as_data_frame(G, what = "vertices")%>%
  left_join(tube_stations_main %>% dplyr::select(stop_id, classification)%>%st_drop_geometry(), by=c("name"="stop_id"))%>%
  mutate(step_free=if_else(classification=="Fully Accessible", TRUE, FALSE))%>%
  dplyr::select(-classification)

#Remake graph
G <- graph_from_data_frame(edges_combined, directed = TRUE, vertices = vertices)

# ----- Centrality Measures ------

#When removing nodes, ensure to keep edges!!