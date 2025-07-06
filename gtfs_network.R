#Network analyis on non-step-free stations

#In this file we:
  # - Simplify the TfL network to tube/rail only, and represent it as a graph
  # - Remove non-step-free stations (while ensuring that links between step-free stations are preserved)
  # - Iteratively add stations to the base graph and calculate betweenness/information centrality

#Note considerable simplifications:
  # - Only considering tube, Overground, and Lizzie lines (when we know buses play a key role in reducing disparities)
  # - Depicting accessibility as a binary
  # - And issue with step-free representation, where it allows you to switch between multiple branches on the same line (when the interchange is not necessarily step-free)

#Beforehand, ensure to run:
  # 1) lsoa_processing.R
  # 2) maps_summary_stats.R
  # Alongside loading packages from analysis_r5r.R

library(future)
library(furrr)

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

#Import Rafael Pereira function - source code https://github.com/rafapereirabr/gtfs_to_igraph
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

#Add accessibility status to nodes
vertices <- as_data_frame(G, what = "vertices")%>%
  left_join(tube_stations_main %>% dplyr::select(stop_id, classification)%>%st_drop_geometry(), by=c("name"="stop_id"))%>%
  mutate(step_free=if_else(classification=="Fully Accessible", TRUE, FALSE))%>%
  dplyr::select(-classification)

#Add lines to nodes (to prevent non-step-free line switching)
lines_joining <- gtfs_stops %>%
  left_join(tube_stations_main %>% dplyr::select(stop_id, stop_name), by = "geometry") %>%
  dplyr::select(stop_id, lines) %>%
  group_by(stop_id) %>%
  summarise(lines = list(unique(lines)), .groups = "drop")
vertices <- vertices %>%
  left_join(lines_joining, by=c("name"="stop_id"))
#Manual fixes
vertices <- vertices %>%
  rowwise() %>%
  mutate(
    lines = list(if (name %in% c('910GDENMRKH', 'BATRSPK')) c('Overground') else lines)
  ) %>%
  ungroup()
rm(lines_joining)

#Remake graph
G <- graph_from_data_frame(edges_combined, directed = TRUE, vertices = vertices)

# ----- Remove all Non-Step-Free Stations ------

#Identify all non-step-free nodes
non_step_free_nodes <- V(G)[!V(G)$step_free]$name

#Create a new graph with only step-free stations, but preserving links between (i.e. through) inaccessible stations as well
remove_inaccessible_stations <- function(graph, to_remove) {
  G2 <- graph
  
  #Identify step-free nodes to keep
  step_free_nodes <- V(graph)$name[!V(graph)$name %in% to_remove]
  
  #Add shortcut edges between step-free endpoints
  for (source in step_free_nodes) {
    for (target in step_free_nodes) {
      if (source == target) next
      
      #Consider all paths
      paths <- all_simple_paths(graph, from = source, to = target, mode = "out", cutoff = 12)
      
      for (path in paths) {
        nodes <- names(path)
        
        #Skip connected stations
        if (length(nodes) < 3) next
        
        #Ensure all intermediate nodes are not step-free
        intermediates <- nodes[-c(1, length(nodes))]
        if (!all(intermediates %in% to_remove)) next
        
        #Skip paths that would require line-switching
        line_sets <- V(graph)$lines[match(nodes, V(graph)$name)]
        shared_lines <- Reduce(intersect, line_sets)
        if (length(shared_lines) == 0) next
        
        #Get edge IDs along path
        edge_ids <- get_edge_ids(graph, c(nodes[-length(nodes)], nodes[-1]), directed = TRUE, error = FALSE)
        if (any(edge_ids == 0)) next
        
        #Compute attributes
        travel_times <- E(graph)$avg_travel_time[edge_ids]
        frequencies  <- E(graph)$weight[edge_ids]
        
        new_time <- sum(travel_times) #add up travel time
        new_freq <- mean(frequencies) #mean of frequency - should stay similar
        new_comb <- new_freq/new_time
        
        #Add edge between endpoints
        G2 <- add_edges(
          G2,
          c(source, target),
          attr = list(
            avg_travel_time = new_time,
            weight = new_freq,
            weight_combined = new_comb))}}}
  
  #Remove all non-step-free stations
  G2 <- delete_vertices(G2, to_remove)
  
  #Clean up parallel edges, recalculate combined weight
  G2 <- simplify(G2,
                 remove.multiple = TRUE,
                 remove.loops = TRUE,
                 edge.attr.comb = list(
                   avg_travel_time = "min",
                   weight = "mean",
                   weight_combined = function(x) NA_real_))
  E(G2)$weight_combined <- E(G2)$weight/E(G2)$avg_travel_time
  
  #Return graph
  G2}

#Run the function
G_base <- remove_inaccessible_stations(G, non_step_free_nodes)

# #Inspect attributes
# vertex_attr_names(G_base)
# edge_attr_names(G_base)
# edges_dfBASE <- as_data_frame(G_base, what = "edges")
# #It is not a perfect representation, because it lets you switch between branches on the same line (e.g. Camden Town), when these might be inaccessible

# ---- Simulate Station Additions ----

#Function to create unique base graphs for all non-step-free stations
make_test_graph <- function(station) {
  remove_inaccessible_stations(G, setdiff(non_step_free_nodes, station))
  }

#Set up multi-session processing due to the number of base graphs which need building
plan(multisession, workers = parallel::detectCores() - 3)

#Looks like I have to manually iterate in small batches because otherwise my computer crashes
test_1_10 <- future_map(non_step_free_nodes[1:10], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[1:10])
test_11_20 <- future_map(non_step_free_nodes[11:20], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[11:20])
test_21_30 <- future_map(non_step_free_nodes[21:30], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[21:30])
test_31_40 <- future_map(non_step_free_nodes[31:40], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[31:40])
test_41_50 <- future_map(non_step_free_nodes[41:50], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[41:50])
test_51_60 <- future_map(non_step_free_nodes[51:60], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[51:60])
test_61_70 <- future_map(non_step_free_nodes[61:70], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[61:70])
test_71_80 <- future_map(non_step_free_nodes[71:80], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[71:80])
test_81_90 <- future_map(non_step_free_nodes[81:90], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[81:90])
test_91_100 <- future_map(non_step_free_nodes[91:100], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[91:100])
test_101_110 <- future_map(non_step_free_nodes[101:110], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[101:110])
test_111_120 <- future_map(non_step_free_nodes[111:120], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[111:120])
test_121_130 <- future_map(non_step_free_nodes[121:130], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[121:130])
test_131_140 <- future_map(non_step_free_nodes[131:140], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[131:140])
test_141_150 <- future_map(non_step_free_nodes[141:150], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[141:150])
test_151_160 <- future_map(non_step_free_nodes[151:160], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[151:160])
test_161_170 <- future_map(non_step_free_nodes[161:170], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[161:170])
test_171_180 <- future_map(non_step_free_nodes[171:180], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[171:180])
test_181_190 <- future_map(non_step_free_nodes[181:190], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[181:190])
test_191_200 <- future_map(non_step_free_nodes[191:200], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[191:200])
test_201_210 <- future_map(non_step_free_nodes[201:210], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[201:210])
test_211_220 <- future_map(non_step_free_nodes[211:220], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[211:220])
test_221 <- future_map(non_step_free_nodes[221], ~ make_test_graph(.x)) %>%
  set_names(non_step_free_nodes[221])

#Combine into one and export for safekeeping
all_graphs <- c(test_1_10, test_11_20, test_21_30, test_31_40, test_41_50, test_51_60, test_61_70, test_71_80, test_81_90, test_91_100,
                test_101_110, test_111_120, test_121_130, test_131_140, test_141_150, test_151_160, test_161_170, test_171_180, test_181_190, test_191_200,
                test_201_210, test_211_220, test_221)
saveRDS(all_graphs, file = "data_export_vis/all_graphs.rds")

setdiff(non_step_free_nodes, names(all_graphs)) #all included

rm(test_1_10, test_11_20, test_21_30, test_31_40, test_41_50, test_51_60, test_61_70, test_71_80, test_81_90, test_91_100,
   test_101_110, test_111_120, test_121_130, test_131_140, test_141_150, test_151_160, test_161_170, test_171_180, test_181_190, test_191_200,
   test_201_210, test_211_220, test_221)

# ----- Centrality Measures -----

#Function to invert edge weights
cost_weights <- function(G) {1/E(G)$weight_combined}

#First, let's find betweenness centrality of all the new nodes added
new_nodes <- names(all_graphs)
node_betweenness <- map2_dbl(all_graphs, new_nodes,
                             ~ betweenness(.x,
                                           v = V(.x)[name == .y],
                                           directed = TRUE,
                                           normalized=TRUE, 
                                           weights  = cost_weights(.x)))

#Now let's find global efficiency of all the new graphs
total_efficiency <- map2_dbl(all_graphs, new_nodes,
                             ~ global_efficiency(
                               .x,
                               weights = cost_weights(.x)))

#Find initial efficiency with only step-free networks
initial_efficiency <- global_efficiency(G_base, weights = cost_weights(G_base))

#Combine measures, calculate efficiency change
graph_node_change <- tibble(
  node = new_nodes,
  node_betweenness = node_betweenness,
  global_efficiency = total_efficiency)%>%
  mutate(efficiency_change = initial_efficiency-global_efficiency)

#write.csv(graph_node_change, file="data_export_vis/graph_node_change.csv")

#To do:
  # - Consider what metrics to include
    # - Is global efficiency actually relevant, if we don't know where info still isn't going because we're not accounting for the removed nodes?
  # - Rank/choose top stations
  # - Map