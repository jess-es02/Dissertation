#9) Assessment of station upgrade scenarios

#In this file, we:
  #Build the new GTFS files for each station upgrade scenario
  #Rerun travel time and cumulative opportunities measures for each
  #Assess impacts

#Scenarios:
  #1: TfL Project Underway
  #2) TfL Under Evaluation (only 7!!)
  #3) Catchment Prioritisation - Equity Focus
  #4) Network Prioritisation - Efficiency Focus
#Note TfL's stations have been updated since starting this project - these were correct as of June 2025

#A limitation is that we have to run all 7/8 stations for each scenario together, rather than one at a time - this is due to computational/time constraints

#Beforehand, ensure to run files 3-8

# ---- Map Upgrades -----

#Join rank comparison to coordinates
top_stations <- rank_comparison %>%
  left_join(tube_stations_main%>%dplyr::select(stop_id, fare_zones, classification, geometry), by=c("node"="stop_id"))%>%
  dplyr::select(node, stop_name, classification, upgrade_status, fare_zones, equity_rank, network_rank, geometry)%>%
  st_as_sf()
rm(rank_comparison)

#Upgrade categories
top_stations <- top_stations %>%
  mutate(scenario = case_when(
    upgrade_status == "Project Underway" ~ "1",
    upgrade_status == "Under Evaluation" & equity_rank > 8 ~ "2",
    upgrade_status == "Under Evaluation" & equity_rank < 9 ~ "2 and 3",
    equity_rank < 9 & network_rank > 8 & upgrade_status == "No Plans" ~ "3",
    (equity_rank > 8|is.na(equity_rank)) & network_rank < 9 ~ "4",
    equity_rank < 9 & network_rank < 9 ~ "3 and 4",
    TRUE ~ NA))

mapping <- c("1" = "#9f13eb", 
             "2" = "#4287f5",
             "2 and 3" = "#26c71e",
             "3" = "#dbb13b",
             "3 and 4" = "#75ecf0", 
             "4" = "#f0b1d7")

tmap_mode("plot")
tmap_options(component.autoscale = TRUE)
tmap_save(
  tm_shape(boroughs)+
    tm_polygons(fill=NA, alpha=0, lwd=1.5)+
    # tm_shape(tube_stations_main)+
    # tm_dots(col = "#d9d9d9", 
    #         shape=21,
    #         size=0.4,
    #         alpha=1,
    #         border.alpha=0.5)+
    tm_shape(top_stations%>%filter(!is.na(scenario)))+
    tm_dots(fill = "scenario", 
            fill.scale = tm_scale_categorical(values = mapping),
            fill.legend = tm_legend(title = "Scenario"),
            shape=21,
            size=0.6)+
    tm_basemap("Esri.OceanBasemap") +
    tm_title("Final Station Upgrade Scenarios") +
    tm_compass(type = "8star",
               size = 3,
               position = c(0.88, 0.22)) +
    tm_scalebar(
      position = c(0.80, 0.08),
      text.size = 0.7,
      breaks = c(0, 5, 10)
    ) +
    tm_layout(
      legend.position = c(0.01, 0.29),
      legend.bg.color = "white",
      legend.showNA = FALSE,
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.6,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9),
  filename = "maps/final_scenarios.png",
  dpi=300)
rm(mapping)

# ------ Build new GTFS files ------

gtfs_original <- gtfstools::read_gtfs("final_r5r/gtfs.zip")

scenario_ids <- list(
  scenario1 = top_stations %>% filter(scenario == 1) %>% pull(node),
  scenario2 = top_stations %>% filter(scenario %in% c("2", "2 and 3")) %>% pull(node),
  scenario3 = top_stations %>% filter(scenario %in% c("3", "2 and 3", "3 and 4")) %>% pull(node),
  scenario4 = top_stations %>% filter(scenario %in% c("4", "3 and 4")) %>% pull(node))

#Extract inaccessible stations
base_stops_to_remove <- tube_stations_main %>%
  filter(classification != 'Fully Accessible') %>%
  pull(stop_id)

#Loop through each scenario, removing inaccessible stops but re-adding stops for each scenario
for (scenario in names(scenario_ids)) {
  
  #Get original GTFS
  gtfs <- gtfs_original

  #Remove only the non-step-free stations not in this scenario
  stops_to_remove <- setdiff(base_stops_to_remove, scenario_ids[[scenario]])

  #Filter GTFS
  gtfs$stops <- gtfs$stops %>%
    filter(!stop_code %in% stops_to_remove)
  gtfs$stop_times <- gtfs$stop_times %>%
    filter(stop_id %in% gtfs$stops$stop_id)

  #Reorder stop times for compatibility
  gtfs$stop_times <- gtfs$stop_times %>%
    group_by(trip_id) %>%
    arrange(arrival_time, .by_group = TRUE) %>%
    mutate(stop_sequence = row_number()) %>%
    ungroup()

  #If there are any trips/routes with no stops at all, remove these
  gtfs$trips <- gtfs$trips %>%
    filter(trip_id %in% gtfs$stop_times$trip_id)
  gtfs$routes <- gtfs$routes %>%
    filter(route_id %in% gtfs$trips$route_id)

  #Export
  out_folder <- paste0("final_r5r_", scenario)
  dir.create(out_folder, recursive = TRUE, showWarnings = FALSE)
  gtfstools::write_gtfs(gtfs, path = file.path(out_folder, "gtfs.zip"))
}

#Then manually paste OSM.pbf into each folder

# ------- Calculate New Travel Times ------

#We don't use the new GTFS files, but instead the file without tube/rail services from script 5
r5r_core <- setup_r5(data_path = "final_r5r_notube", verbose=TRUE)

results_CP <- list()
results_SLOW <- list()

for (scenario in names(scenario_ids)) {
 
  #Find accessible stations in this scenario
  accessible_stations <- tube_stations_main %>%
    filter(classification == 'Fully Accessible'
           | stop_id %in% scenario_ids[[scenario]])%>%
    dplyr::select(-classification, -upgrade_status, -fare_zones)%>%
    st_transform(4326) %>%
    mutate(lon = st_coordinates(.)[, 1],
           lat = st_coordinates(.)[, 2]) %>%
    st_drop_geometry()%>%
    rename("id" = stop_id)
  
  #We already have times to individuals' nearest non-step-free stations - these won't have changed
  
  #Find time to fastest step-free station, CP
  fastest_accessible_stationCP <- get_fastest_station(origins = pop_centroids, destinations = accessible_stations, return_fastest_station=FALSE)
  missing_centroids <- pop_centroids %>%
    filter(!id %in% fastest_accessible_stationCP$from_id)
  fastest_station2 <- get_fastest_station(
    origins = missing_centroids,
    destinations = accessible_stations,
    max_trip_duration=500,
    return_fastest_station=FALSE)
  fastest_accessible_stationCP <- rbind(fastest_accessible_stationCP, fastest_station2)

  #Slower walking speed
  fastest_accessible_stationSLOW <- get_fastest_station(origins = pop_centroids, destinations = accessible_stations,
                                                     walk_speed = 0.43, max_trip_duration = 300)
  missing_centroids <- pop_centroids %>%
    filter(!id %in% fastest_accessible_stationSLOW$from_id)
  fastest_station2 <- get_fastest_station(
    origins = missing_centroids,
    destinations = accessible_stations,
    walk_speed = 0.43,
    max_trip_duration=1000)
  fastest_accessible_stationSLOW <- rbind(fastest_accessible_stationSLOW, fastest_station2)
  
  #Save results
  results_CP[[scenario]] <- fastest_accessible_stationCP
  results_SLOW[[scenario]] <- fastest_accessible_stationSLOW
}

#Combine results and export
fastest_station_scenarios <- fastest_time_to_stations %>%
  dplyr::select(lsoa21cd, lsoa21nm, mean_fastest_station, mean_accessible_stationCP, mean_accessible_stationSLOW, ratioCP, ratioSLOW)%>%
  rename("time_no_constraints" = mean_fastest_station,
         "original_timeCP" = mean_accessible_stationCP,
         "original_timeSLOW" = mean_accessible_stationSLOW,
         "original_ratioCP" = ratioCP,
         "original_ratioSLOW" = ratioSLOW)%>%
  left_join(results_CP[["scenario1"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario1_timeCP" = mean_travel_time)%>%
  left_join(results_SLOW[["scenario1"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario1_timeSLOW" = mean_travel_time)%>%
  mutate(scenario1_ratioCP = scenario1_timeCP/time_no_constraints,
         scenario1_ratioSLOW = scenario1_timeSLOW/time_no_constraints)%>%
  left_join(results_CP[["scenario2"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario2_timeCP" = mean_travel_time)%>%
  left_join(results_SLOW[["scenario2"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario2_timeSLOW" = mean_travel_time)%>%
  mutate(scenario2_ratioCP = scenario2_timeCP/time_no_constraints,
         scenario2_ratioSLOW = scenario2_timeSLOW/time_no_constraints)%>%
  left_join(results_CP[["scenario3"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario3_timeCP" = mean_travel_time)%>%
  left_join(results_SLOW[["scenario3"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario3_timeSLOW" = mean_travel_time)%>%
  mutate(scenario3_ratioCP = scenario3_timeCP/time_no_constraints,
         scenario3_ratioSLOW = scenario3_timeSLOW/time_no_constraints)%>%
  left_join(results_CP[["scenario4"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario4_timeCP" = mean_travel_time)%>%
  left_join(results_SLOW[["scenario4"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario4_timeSLOW" = mean_travel_time)%>%
  mutate(scenario4_ratioCP = scenario4_timeCP/time_no_constraints,
         scenario4_ratioSLOW = scenario4_timeSLOW/time_no_constraints)
st_write(fastest_station_scenarios, "data_export_vis/fastest_station_scenarios.gpkg")

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
rm(accessible_stations, fastest_accessible_stationCP, fastest_accessible_stationSLOW, fastest_station2, gtfs_original, gtfs, missing_centroids, base_stops_to_remove, out_folder, scenario, stops_to_remove, get_fastest_station)

# ----- Calculate New Cumulative Opportunities ------

#Re-initialise result lists
results_CP <- list()
results_SLOW <- list()

for (scenario in names(scenario_ids)) {
  
  r5r_core <- setup_r5(data_path = paste0("final_r5r_", scenario), verbose=TRUE)
  jobs_accessibleCP <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids)
  jobs_accessibleSLOW <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, walk_speed = 0.43)
  
  #Save results
  results_CP[[scenario]] <- jobs_accessibleCP
  results_SLOW[[scenario]] <- jobs_accessibleSLOW
    
  r5r::stop_r5(r5r_core)
  rJava::.jgc(R.gc = TRUE)
}

#Combine results and export
job_access_scenarios <- jobs_in_45_min %>%
  dplyr::select(lsoa21nm, lsoa21cd, jobs_standard, jobs_accessibleCP, jobs_accessible_SLOW, ratioCP, ratioSLOW)%>%
  rename("jobs_no_constraints" = jobs_standard,
         "original_jobs_CP" = jobs_accessibleCP, 
         "original_jobs_SLOW" = jobs_accessible_SLOW,
         "original_ratio_CP" = ratioCP,
         "original_ratio_SLOW" = ratioSLOW)%>%
  left_join(results_CP[["scenario1"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario1_jobsCP" = jobs_45_min)%>%
  left_join(results_SLOW[["scenario1"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario1_jobsSLOW" = jobs_45_min)%>%
  left_join(results_CP[["scenario2"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario2_jobsCP" = jobs_45_min)%>%
  left_join(results_SLOW[["scenario2"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario2_jobsSLOW" = jobs_45_min)%>%
  left_join(results_CP[["scenario3"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario3_jobsCP" = jobs_45_min)%>%
  left_join(results_SLOW[["scenario3"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario3_jobsSLOW" = jobs_45_min)%>%
  left_join(results_CP[["scenario4"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario4_jobsCP" = jobs_45_min)%>%
  left_join(results_SLOW[["scenario4"]], by=c("lsoa21cd" = "from_id"))%>%
  rename("scenario4_jobsSLOW" = jobs_45_min)
st_write(job_access_scenarios, "data_export_vis/job_access_scenarios.gpkg")
job_access_scenarios <- st_read("data_export_vis/job_access_scenarios.gpkg")

rm(jobs_accessibleCP, jobs_accessibleSLOW, results_CP, results_SLOW, scenario_ids, departure_times, scenario, cumulative_opportunities)

#Coalesce zeroes, calculate job ratios
job_access_scenarios <- job_access_scenarios %>%
  mutate(across(matches("^scenario.*jobs"), ~ coalesce(., 0)))%>%
  mutate(scenario1_ratioCP = scenario1_jobsCP/jobs_no_constraints,
         scenario1_ratioSLOW = scenario1_jobsSLOW/jobs_no_constraints,
         scenario2_ratioCP = scenario2_jobsCP/jobs_no_constraints,
         scenario2_ratioSLOW = scenario2_jobsSLOW/jobs_no_constraints,
         scenario3_ratioCP = scenario3_jobsCP/jobs_no_constraints,
         scenario3_ratioSLOW = scenario3_jobsSLOW/jobs_no_constraints,
         scenario4_ratioCP = scenario4_jobsCP/jobs_no_constraints,
         scenario4_ratioSLOW = scenario4_jobsSLOW/jobs_no_constraints)%>%
  mutate(across(matches("^scenario.*ratio"), ~ replace(., is.nan(.), 1)))

#Calculate ratios over original CP
job_access_scenarios <- job_access_scenarios %>%
  mutate(scenario1_ratioCP_change = scenario1_jobsCP/original_jobs_CP,
         scenario2_ratioCP_change = scenario2_jobsCP/original_jobs_CP,
         scenario3_ratioCP_change = scenario3_jobsCP/original_jobs_CP,
         scenario4_ratioCP_change = scenario4_jobsCP/original_jobs_CP)

# ------- Assess Network Efficiencies -------

#Remind ourselves of G_base's initial attributes:
#global efficiency: 13.15386
#apl: 0.1932217
#average betweenness: 1445.037

ge <- global_efficiency(G_base, weights = cost_weights(G_base), directed=TRUE)
apl <- mean_distance(G_base, weights=cost_weights(G_base), directed=TRUE)
btw <- betweenness(G_base, directed = TRUE, weights = cost_weights(G_base), normalized = FALSE)
ab <- mean(btw, na.rm = TRUE) 

#Create results dataframe
scenario_network_efficiencies <- data.frame(
  scenario = character(),
  global_efficiency = numeric(),
  average_path_length = numeric(),
  avg_betweenness = numeric())

for (i in 1:4) {

  scenario_name <- paste0("final_r5r_scenario", i)
  gtfs_path <- file.path(scenario_name, "gtfs.zip")

  #We need to make some adjustments to the GTFS files for the function to run
  gtfs <- gtfstools::read_gtfs(gtfs_path)
  gtfs <- filter_by_weekday(gtfs, c("wednesday"))%>% #Simplify time-wise
    filter_by_route_type(c(1, 2)) 
  gtfs$stops <- gtfs$stops %>%
    filter(stop_code!="") #Remove DLR
  gtfs$stop_times <- gtfs$stop_times %>%
    filter(stop_id %in% gtfs$stops$stop_id)
  gtfs$trips <- gtfs$trips %>%
    filter(trip_id %in% gtfs$stop_times$trip_id)
  gtfs$routes <- gtfs$routes %>%
    filter(route_id %in% gtfs$trips$route_id)
  gtfs$stop_times <- gtfs$stop_times %>% #Represent stops by stops, rather than platforms
    left_join(gtfs$stops %>% dplyr::select(stop_id, stop_code), by = "stop_id") %>%
    mutate(stop_id = stop_code) %>%
    dplyr::select(-stop_code)
  gtfs$stops <- gtfs$stops %>%
    mutate(stop_id = stop_code) %>%
    group_by(stop_id) %>%
    slice(1) %>%
    ungroup()
  gtfs$stops <- gtfs$stops %>% #Add necessary columns for function
    mutate(location_type = 0,
           parent_station = stop_id)
  gtfs$trips <- gtfs$trips %>%
    mutate(direction_id = 0)
  gtfs_write(gtfs, folder = scenario_name, name = "gtfs_tube_only")
  
  my_gtfs_feeds <- list(file.path(scenario_name, "gtfs_tube_only.zip"))
  G <- gtfs_to_igraph(list_gtfs = my_gtfs_feeds, dist_threshold=0, save_muxviz=FALSE)
  
  #Calculate edge weights, recreate graph
  edges_df <- as_data_frame(G, what = "edges")
  edges_combined <- edges_df %>%
    group_by(from, to, avg_travel_time) %>%
    summarise(weight = sum(weight), .groups = "drop")
  edges_combined <- edges_combined %>%
    mutate(weight_combined = weight / avg_travel_time)%>%
    filter(from != to)
  vertices <- as_data_frame(G, what = "vertices")
  G <- graph_from_data_frame(edges_combined, directed = TRUE, vertices = vertices)
  
  #Calculate global efficiency, average path length, and average global betwenness
  ge <- global_efficiency(G, weights = cost_weights(G), directed=TRUE)
  apl <- mean_distance(G, weights=cost_weights(G), directed=TRUE)
  btw <- betweenness(G, directed = TRUE, weights = cost_weights(G), normalized = FALSE)
  ab <- mean(btw, na.rm = TRUE) 

  #Append to results
  scenario_network_efficiencies <- scenario_network_efficiencies %>%
    add_row(
      scenario = paste0("scenario", i),
      global_efficiency = ge,
      average_path_length = apl,
      avg_betweenness = ab)
}
write.csv(scenario_network_efficiencies, "data_export_vis/scenario_network_efficiencies.csv")
#Obviously scenario 4 is the most "efficient", but striking how similar scenario 3 is to scenario 2
#(Except obviously remember that scenario 2 is only 7 upgrades vs 8)

#Do results indicate that efficiency and APL aren't necessarily metrics to optimise? "Inefficiency" as inevitable if we are essentially expanding the network?
#Betweenness seems more useful, except obviously practically harder to upgrade zone 1/2 spots

rm(ab, apl, btw, ge, gtfs_path, i, scenario_name, cost_weights, gtfs_to_igraph, G, edges_df, edges_combined)

# ----- Assess Job Ratios ------
summary(job_access_scenarios$original_ratio_CP)
summary(job_access_scenarios$scenario1_ratioCP)
summary(job_access_scenarios$scenario2_ratioCP)
summary(job_access_scenarios$scenario3_ratioCP)
summary(job_access_scenarios$scenario4_ratioCP) #Prioritising more central stations as a more utilitarian approach than scenario 3? Greater total increase

summary(job_access_scenarios$original_ratio_SLOW)
summary(job_access_scenarios$scenario1_ratioSLOW)
summary(job_access_scenarios$scenario2_ratioSLOW)
summary(job_access_scenarios$scenario3_ratioSLOW)
summary(job_access_scenarios$scenario4_ratioSLOW) #In reality, ratio change so small that it is futile??

summary(job_access_scenarios$scenario1_ratioCP_change)
summary(job_access_scenarios$scenario2_ratioCP_change)
summary(job_access_scenarios$scenario3_ratioCP_change)
summary(job_access_scenarios$scenario4_ratioCP_change) #Again, greatest overall increase in scenario 4 (rather than 3)
#Numbers are still very small! Mean of 1.004 indicates an average of 0.4% increase in jobs, etc.

#Overall changes
total_original_jobs <- sum(job_access_scenarios$jobs_no_constraints)
total_original_accessible_jobs <- sum(job_access_scenarios$original_jobs_CP)
total_accessible_jobs_scenario1 <- sum(job_access_scenarios$scenario1_jobsCP)
total_accessible_jobs_scenario2 <- sum(job_access_scenarios$scenario2_jobsCP)
total_accessible_jobs_scenario3 <- sum(job_access_scenarios$scenario3_jobsCP)
total_accessible_jobs_scenario4 <- sum(job_access_scenarios$scenario4_jobsCP)

total_original_accessible_jobs/total_original_jobs #originally, only 81.47% of jobs accessible

total_accessible_jobs_scenario1/total_original_jobs #scenario1: 81.55%
total_accessible_jobs_scenario1/total_original_accessible_jobs #1.000986

total_accessible_jobs_scenario2/total_original_jobs #scenario2: 81.53% (note only 7/8)
total_accessible_jobs_scenario2/total_original_accessible_jobs #1.000677

total_accessible_jobs_scenario3/total_original_jobs #scenario3: 82.11% - big jump from TfL scenarios (almost 8x gain)
total_accessible_jobs_scenario3/total_original_accessible_jobs #1.007791

total_accessible_jobs_scenario4/total_original_jobs #scenario4: 82.90% - big jump from TfL scenarios (approx 18x gain - but far less feasible)
total_accessible_jobs_scenario4/total_original_accessible_jobs #1.017513

rm(total_accessible_jobs_scenario1, total_accessible_jobs_scenario2, total_accessible_jobs_scenario3, total_accessible_jobs_scenario4, total_original_accessible_jobs, total_original_jobs)

#Violin plot of changes
pivoted <- job_access_scenarios %>%
  st_drop_geometry() %>%
  dplyr::select(original_jobs_CP, scenario1_jobsCP, scenario2_jobsCP, scenario3_jobsCP, scenario4_jobsCP) %>%
  rename(
    "Pre-Upgrades" = original_jobs_CP,
    "Scenario 1" = scenario1_jobsCP,
    "Scenario 2" = scenario2_jobsCP,
    "Scenario 3" = scenario3_jobsCP,
    "Scenario 4" = scenario4_jobsCP) %>%
  pivot_longer(cols = everything(),
               names_to = "type",
               values_to = "value")
pivoted$type <- factor(pivoted$type, levels = c("Pre-Upgrades", "Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"))

ggplot(pivoted, aes(x = type, y = value, fill = type)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Distribution of Accessible Jobs Within 45 Minutes",
       x = "Scenario",
       y = "Jobs") +
  ylim(0, 600000) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    axis.title.x = element_text(margin = margin(t = 10)))
#Ultimately, all scenarios seem pretty similar! Although 3 and 4 have higher extremes

#Display spread of increase among clusters:

#Join to cluster info
job_access_scenarios <- job_access_scenarios %>%
  left_join(cluster_vars %>% st_drop_geometry %>% dplyr::select(lsoa21cd, cluster), by="lsoa21cd")

#Calculate change per scenario
job_access_scenarios <- job_access_scenarios %>%
  mutate(scenario1_increase = scenario1_jobsCP-original_jobs_CP,
         scenario2_increase = scenario2_jobsCP-original_jobs_CP,
         scenario3_increase = scenario3_jobsCP-original_jobs_CP,
         scenario4_increase = scenario4_jobsCP-original_jobs_CP)

#Pivot data
pivoted <- job_access_scenarios %>%
  st_drop_geometry() %>%
  dplyr::select(scenario1_increase, scenario2_increase, scenario3_increase, scenario4_increase, cluster) %>%
  rename(
    "Scenario 1" = scenario1_increase,
    "Scenario 2" = scenario2_increase,
    "Scenario 3" = scenario3_increase,
    "Scenario 4" = scenario4_increase) %>%
  pivot_longer(cols = -cluster,
               names_to = "scenario",
               values_to = "job_increase")
pivoted$scenario <- factor(pivoted$scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"))

ggplot(pivoted, aes(x = scenario, y = job_increase, fill = cluster)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cols_changed) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Additional Accessible Jobs by Origin LSOA Type",
       x = NULL,
       y = "Additional Jobs",
       fill = "Cluster") +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9))
#Shows scenario 4 would actually be ideal, because benefits in clusters 1 and 3 are the same as in scenario 3 - but obviously less feasible
#Note this will be double-counting lots of additional jobs! Rather than looking at raw numbers, it's more a useful indicator of who benefits most

#We will want to compare locations of increases - particularly in scenarios 3 and 4 (and consider presence of in-need pop)
#Then do travel time analysis