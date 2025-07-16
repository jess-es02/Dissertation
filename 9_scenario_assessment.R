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
#fastest_station_scenarios <- st_read("data_export_vis/fastest_station_scenarios.gpkg")

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

#Total changes, weighted by numbers of disabled and non-disabled people
calculations <- pop_centroids %>%
  dplyr::select(id, total_disabled, total_pop)%>%
  mutate(total_non_disabled = total_pop-total_disabled)%>%
  left_join(job_access_scenarios, by=c("id" = "lsoa21cd"))
calculations <- calculations %>%
  mutate(jobs_non_disabled_multiplied = jobs_no_constraints * total_non_disabled,
         jobs_original_CP_multiplied = original_jobs_CP * total_disabled,
         scenario1_jobs_CP_multiplied = scenario1_jobsCP * total_disabled,
         scenario2_jobs_CP_multiplied = scenario2_jobsCP * total_disabled,
         scenario3_jobs_CP_multiplied = scenario3_jobsCP * total_disabled,
         scenario4_jobs_CP_multiplied = scenario4_jobsCP * total_disabled)
total_disabled <- sum(calculations$total_disabled)
total_non_disabled <- sum(calculations$total_non_disabled)
avg_jobs_non_disabled <- sum(calculations$jobs_non_disabled_multiplied)/total_non_disabled
avg_original_jobs_disabled_CP <- sum(calculations$jobs_original_CP_multiplied)/total_disabled
avg_scenario1_jobs_disabled_CP <- sum(calculations$scenario1_jobs_CP_multiplied)/total_disabled
avg_scenario2_jobs_disabled_CP <- sum(calculations$scenario2_jobs_CP_multiplied)/total_disabled
avg_scenario3_jobs_disabled_CP <- sum(calculations$scenario3_jobs_CP_multiplied)/total_disabled
avg_scenario4_jobs_disabled_CP <- sum(calculations$scenario4_jobs_CP_multiplied)/total_disabled

print(avg_jobs_non_disabled) #70672.32
print(avg_original_jobs_disabled_CP) #58189.51
print(avg_scenario1_jobs_disabled_CP) #58245.04
print(avg_scenario2_jobs_disabled_CP) #58226.56 - considering it's 7/8, slightly better
print(avg_scenario3_jobs_disabled_CP) #58641.41
print(avg_scenario4_jobs_disabled_CP) #59142.47

#As percentages
round(100 * avg_original_jobs_disabled_CP/avg_jobs_non_disabled, 2) #CP: 82.3%
round(100 * avg_scenario1_jobs_disabled_CP/avg_jobs_non_disabled, 2) #1: 82.4%
round(100 * avg_scenario2_jobs_disabled_CP/avg_jobs_non_disabled, 2) #2: 82.4%
round(100 * avg_scenario3_jobs_disabled_CP/avg_jobs_non_disabled, 2) #3: 83.0%
round(100 * avg_scenario4_jobs_disabled_CP/avg_jobs_non_disabled, 2) #4: 83.7%

rm(calculations, total_disabled, total_non_disabled, avg_jobs_non_disabled, avg_original_jobs_disabled_CP, avg_scenario1_jobs_disabled_CP, avg_scenario2_jobs_disabled_CP, avg_scenario3_jobs_disabled_CP, avg_scenario4_jobs_disabled_CP)

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

#Changed correlation between in-need population and disparity
job_access_scenarios <- job_access_scenarios %>%
  left_join(pop_centroids %>% dplyr::select(id, step_free_benefit_indexW), by=c("lsoa21cd"="id"))
#Original: 0.122
cor.test(job_access_scenarios$original_ratio_CP, job_access_scenarios$step_free_benefit_indexW)
ggplot(job_access_scenarios, aes(original_ratio_CP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25)
#Scenarios
cor.test(job_access_scenarios$scenario1_ratioCP, job_access_scenarios$step_free_benefit_indexW)
ggplot(job_access_scenarios, aes(scenario1_ratioCP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25) #1: 0.120 - lower
cor.test(job_access_scenarios$scenario2_ratioCP, job_access_scenarios$step_free_benefit_indexW)
ggplot(job_access_scenarios, aes(scenario2_ratioCP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25) #2: 0.121 - lower
cor.test(job_access_scenarios$scenario3_ratioCP, job_access_scenarios$step_free_benefit_indexW)
ggplot(job_access_scenarios, aes(scenario3_ratioCP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25) #3: 0.125 - higher
cor.test(job_access_scenarios$scenario4_ratioCP, job_access_scenarios$step_free_benefit_indexW)
ggplot(job_access_scenarios, aes(scenario4_ratioCP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25) #4: 0.120 - lowest
#So scenario 3 is best at reducing the overall disparity
#Disparity overall grows in others because disparities are improving in areas with lower in-need pops
#But this presumably isn't a problem if scenario 4 is benefitting the same number of in-need? (see stacked bar chart)

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
rm(pivoted)
#Shows scenario 4 would actually be ideal, because benefits in clusters 1 and 3 are the same as in scenario 3 - but obviously less feasible
#Note this will be double-counting lots of additional jobs! Rather than looking at raw numbers, it's more a useful indicator of who benefits most
#Extra jobs are expected in 3 and 4, because highest disparity = highest potential job gains

#Catchment-wide comparison: this is useful to account for catchment sizes in TfL
station_catchments <- fastest_time_to_stations %>%
  dplyr::select(lsoa21cd, lsoa21nm, fastest_station) %>%
  left_join(job_access_scenarios %>% st_drop_geometry() %>% dplyr::select(lsoa21cd, scenario1_increase, scenario2_increase, scenario3_increase, scenario4_increase), by = "lsoa21cd")%>%
  left_join(pop_centroids %>% dplyr::select(id, total_pop, total_under_5, total_65_plus, total_disabled), by = c("lsoa21cd" = "id"))%>%
  mutate(total_in_need_pop = total_under_5 + total_65_plus + total_disabled)%>%
  dplyr::select(-total_under_5, -total_65_plus, -total_disabled)%>%
  left_join(tube_stations_main %>% dplyr::select(stop_id, stop_name, classification, upgrade_status)%>%st_drop_geometry(), by=c("fastest_station"="stop_id"))
job_access_catchments <- station_catchments %>%
  group_by(fastest_station, stop_name, classification, upgrade_status) %>%
  summarise(
    total_population = sum(total_pop),
    total_in_need_population = sum(total_in_need_pop),
    scenario1_job_increase = sum(scenario1_increase),
    scenario2_job_increase = sum(scenario2_increase),
    scenario3_job_increase = sum(scenario3_increase),
    scenario4_job_increase = sum(scenario4_increase))%>%
  mutate(pct_in_need = 100*total_in_need_population/total_population)

#Assess job increases x in-need population
job_access_catchments <- job_access_catchments %>%
  mutate(scenario1_impact = total_in_need_population*scenario1_job_increase,
         scenario2_impact = total_in_need_population*scenario2_job_increase,
         scenario3_impact = total_in_need_population*scenario3_job_increase,
         scenario4_impact = total_in_need_population*scenario4_job_increase)
sum(job_access_catchments$scenario1_impact) #3280833310
sum(job_access_catchments$scenario2_impact) #4153822469
sum(job_access_catchments$scenario3_impact) #23413417023
sum(job_access_catchments$scenario4_impact) #33440720628

sum(job_access_catchments$scenario3_impact)/sum(job_access_catchments$scenario1_impact) #7x more "impactful"
sum(job_access_catchments$scenario4_impact)/sum(job_access_catchments$scenario1_impact) #10x more "impactful"
#So we find that despite TfL's stations having larger catchment sizes, the impact on overall job accessibility is less pronounced

#Proportion of LSOAs where there was some "impact"
100 * sum(job_access_catchments$scenario1_impact != 0)/nrow(job_access_catchments) #16.1%
100 * sum(job_access_catchments$scenario2_impact != 0)/nrow(job_access_catchments) #9.88%
100 * sum(job_access_catchments$scenario3_impact != 0)/nrow(job_access_catchments) #38.42%
100 * sum(job_access_catchments$scenario4_impact != 0)/nrow(job_access_catchments) #30.51%
#So 3 and 4 as more dispersed
#3 as more dispersed than 4? Will need to check this

rm(station_catchments)

#Plot absolute differences - manually change column
breaks <- c(0, 1, 500, 5000, 10000, 20000, 50000, 100000, 200000, 400000)
tmap_save(
  tm_shape(job_access_scenarios) +
    tm_polygons(
      col = "scenario4_increase",
      style="fixed",
      breaks=breaks,
      palette="rd_pu",
      alpha=0.9,
      title = "Difference",
      textNA = "",
      border.alpha=0) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_title("Absolute Difference in Accessible Jobs, Scenario 4") +
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
      title.size = 1.5,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9),
  filename = "maps/scenario4_absolute_diffCP.png",
  dpi=300)

#Tried a bivariate choropleth, but this didn't work because globally, scenario 4 outweighed the rest

#Hatching any LSOAs in the top 10% and 25% of in-need population
threshold75 <- quantile(job_access_scenarios$step_free_benefit_indexW, 0.75)
threshold90 <- quantile(job_access_scenarios$step_free_benefit_indexW, 0.90)
job_access_scenarios <- job_access_scenarios %>%
  mutate(in_need25 = if_else(step_free_benefit_indexW>=threshold75, TRUE, FALSE),
         in_need10 = if_else(step_free_benefit_indexW>=threshold90, TRUE, FALSE))
tmap_save(
  tm_shape(job_access_scenarios) +
    tm_polygons(
      col = "scenario4_increase",
      style="fixed",
      breaks=breaks,
      palette="rd_pu",
      alpha=0.9,
      title = "Difference",
      textNA = "",
      border.alpha=0) +
    tm_shape(job_access_scenarios %>% filter(scenario4_increase>0 & in_need25)) +
    tm_borders(col = "blue", lwd = 1) +
    tm_shape(job_access_scenarios %>% filter(scenario4_increase>0 & in_need10)) +
    tm_borders(col = "red", lwd = 1) +
    tm_add_legend(
      type = "line",
      labels = "Top 10% In-Need",
      col = "red")+
    tm_add_legend(
      type = "line",
      labels = "Top 25% In-Need",
      col = "blue")+
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_title("Absolute Difference in Accessible Jobs, Scenario 4") +
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
      title.size = 1.5,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9),
  filename = "maps/scenario4_absolute_diffCP_hatched.png",
  dpi=300)
#Shows scenario 3 greatest gains are more concentrated in in-need areas, scenario 4 aren't really

# ----- Assess Travel Time Results ------

#Overall summaries
summary(fastest_station_scenarios$time_no_constraints)
summary(fastest_station_scenarios$original_timeCP)
summary(fastest_station_scenarios$scenario1_timeCP)
summary(fastest_station_scenarios$scenario2_timeCP)
summary(fastest_station_scenarios$scenario3_timeCP)
summary(fastest_station_scenarios$scenario4_timeCP) #Differences as fairly negligible, but scenario 3 "best"
#Scenario 4 the "worst", likely due to geographic concentration of stations

summary(fastest_station_scenarios$original_ratioCP)
summary(fastest_station_scenarios$scenario1_ratioCP)
summary(fastest_station_scenarios$scenario2_ratioCP)
summary(fastest_station_scenarios$scenario3_ratioCP)
summary(fastest_station_scenarios$scenario4_ratioCP)

summary(fastest_station_scenarios$original_ratioSLOW)
summary(fastest_station_scenarios$scenario1_ratioSLOW)
summary(fastest_station_scenarios$scenario2_ratioSLOW)
summary(fastest_station_scenarios$scenario3_ratioSLOW)
summary(fastest_station_scenarios$scenario4_ratioSLOW)

#Calculate absolute time savings
fastest_station_scenarios <- fastest_station_scenarios %>%
  mutate(scenario1_time_saving = original_timeCP-scenario1_timeCP,
         scenario2_time_saving = original_timeCP-scenario2_timeCP,
         scenario3_time_saving = original_timeCP-scenario3_timeCP,
         scenario4_time_saving = original_timeCP-scenario4_timeCP)
summary(fastest_station_scenarios$scenario1_time_saving)
summary(fastest_station_scenarios$scenario2_time_saving)
summary(fastest_station_scenarios$scenario3_time_saving)
summary(fastest_station_scenarios$scenario4_time_saving)
#Again, most pronounced for scenario 3

#Overall changes
total_original_time <- sum(fastest_station_scenarios$time_no_constraints)
total_original_accessible_time <- sum(fastest_station_scenarios$original_timeCP)

total_time_scenario1 <- sum(fastest_station_scenarios$scenario1_timeCP)
total_time_scenario2 <- sum(fastest_station_scenarios$scenario2_timeCP)
total_time_scenario3 <- sum(fastest_station_scenarios$scenario3_timeCP)
total_time_scenario4 <- sum(fastest_station_scenarios$scenario4_timeCP)

total_original_accessible_time/total_original_time #Originally, total travel to accessible stations was 15.6% higher

total_time_scenario1/total_original_time #1: 14.2%
total_time_scenario2/total_original_time #2: 13.3%
total_time_scenario3/total_original_time #1: 13.1%
total_time_scenario4/total_original_time #1: 14.8%

#Total improvements over accessible baseline
1- total_time_scenario1/total_original_accessible_time #1: 1.2%
1-total_time_scenario2/total_original_accessible_time #2: 2.0%
1-total_time_scenario3/total_original_accessible_time #3: 2.2%
1-total_time_scenario4/total_original_accessible_time #4: 0.71%

rm(total_original_time, total_original_accessible_time, total_time_scenario1, total_time_scenario2, total_time_scenario3, total_time_scenario4)

#Total changes, weighted by numbers of disabled and non-disabled people
calculations <- pop_centroids %>%
  dplyr::select(id, total_disabled, total_pop)%>%
  mutate(total_non_disabled = total_pop-total_disabled)%>%
  left_join(fastest_station_scenarios, by=c("id" = "lsoa21cd"))%>%
  mutate(scenario1_diffCP = scenario1_timeCP-time_no_constraints,
         scenario1_diffSLOW = scenario1_timeSLOW-time_no_constraints,
         scenario2_diffCP = scenario2_timeCP-time_no_constraints,
         scenario2_diffSLOW = scenario2_timeSLOW-time_no_constraints,
         scenario3_diffCP = scenario3_timeCP-time_no_constraints,
         scenario3_diffSLOW = scenario3_timeSLOW-time_no_constraints,
         scenario4_diffCP = scenario4_timeCP-time_no_constraints,
         scenario4_diffSLOW = scenario4_timeSLOW-time_no_constraints)
calculations <- calculations %>%
  mutate(scenario1_diffCP_multiplied = scenario1_diffCP * total_disabled,
         scenario1_diffSLOW_multiplied = scenario1_diffSLOW * total_disabled,
         scenario2_diffCP_multiplied = scenario2_diffCP * total_disabled,
         scenario2_diffSLOW_multiplied = scenario2_diffSLOW * total_disabled,
         scenario3_diffCP_multiplied = scenario3_diffCP * total_disabled,
         scenario3_diffSLOW_multiplied = scenario3_diffSLOW * total_disabled,
         scenario4_diffCP_multiplied = scenario4_diffCP * total_disabled,
         scenario4_diffSLOW_multiplied = scenario4_diffSLOW * total_disabled)
total_disabled <- sum(calculations$total_disabled)

#Average CP differences: originally 5.060888
sum(calculations$scenario1_diffCP_multiplied)/total_disabled #1: 4.61 min
sum(calculations$scenario2_diffCP_multiplied)/total_disabled #2: 4.38 min
sum(calculations$scenario3_diffCP_multiplied)/total_disabled #3: 4.22 min
sum(calculations$scenario4_diffCP_multiplied)/total_disabled #4: 4.79 min

#Find average improvement over baseline
baseline <- 5.060888
(baseline - sum(calculations$scenario1_diffCP_multiplied)/total_disabled)/baseline * 100 #1: 8.92% improvement
(baseline - sum(calculations$scenario2_diffCP_multiplied)/total_disabled)/baseline * 100 #2: 13.5% improvement
(baseline - sum(calculations$scenario3_diffCP_multiplied)/total_disabled)/baseline * 100 #3: 16.6% improvement
(baseline - sum(calculations$scenario4_diffCP_multiplied)/total_disabled)/baseline * 100 #4: 5.31% improvement

#Average slow differences: originally 49.27623
sum(calculations$scenario1_diffSLOW_multiplied)/total_disabled #1: 48.7 min
sum(calculations$scenario2_diffSLOW_multiplied)/total_disabled #2: 48.0 min
sum(calculations$scenario3_diffSLOW_multiplied)/total_disabled #3: 47.8 min
sum(calculations$scenario4_diffSLOW_multiplied)/total_disabled #4: 48.9 min

#Find average improvement over baseline
baseline <- 49.27623
(baseline - sum(calculations$scenario1_diffSLOW_multiplied)/total_disabled)/baseline * 100 #1: 1.22% improvement
(baseline - sum(calculations$scenario2_diffSLOW_multiplied)/total_disabled)/baseline * 100 #2: 2.61% improvement
(baseline - sum(calculations$scenario3_diffSLOW_multiplied)/total_disabled)/baseline * 100 #3: 3.09% improvement
(baseline - sum(calculations$scenario4_diffSLOW_multiplied)/total_disabled)/baseline * 100 #4: 0.99% improvement

rm(baseline, calculations, total_disabled)

#Violin plot wasn't really able to differentiate time savings between scenarios!

#Changes in correlation between disparity and in-need population
fastest_station_scenarios <- fastest_station_scenarios %>%
  left_join(pop_centroids %>% dplyr::select(id, step_free_benefit_indexW), by=c("lsoa21cd"="id"))

#Original: -0.0617
cor.test(fastest_station_scenarios$original_ratioCP, fastest_station_scenarios$step_free_benefit_indexW)
ggplot(fastest_station_scenarios, aes(original_ratioCP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25)

#Scenarios
cor.test(fastest_station_scenarios$scenario1_ratioCP, fastest_station_scenarios$step_free_benefit_indexW)
ggplot(fastest_station_scenarios, aes(scenario1_ratioCP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25) #1: -0.059697
cor.test(fastest_station_scenarios$scenario2_ratioCP, fastest_station_scenarios$step_free_benefit_indexW)
ggplot(fastest_station_scenarios, aes(scenario2_ratioCP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25) #2: -0.05125878
cor.test(fastest_station_scenarios$scenario3_ratioCP, fastest_station_scenarios$step_free_benefit_indexW)
ggplot(fastest_station_scenarios, aes(scenario3_ratioCP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25) #3: -0.06735296
cor.test(fastest_station_scenarios$scenario4_ratioCP, fastest_station_scenarios$step_free_benefit_indexW)
ggplot(fastest_station_scenarios, aes(scenario4_ratioCP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25) #4: -0.05231448
#So scenario 3 as the only which reduces extent of overall disparity - others benefit less in-need areas more
#But does this matter if we consider overall change? Let's consider spread of time savings among groups

#Join to cluster info
fastest_station_scenarios <- fastest_station_scenarios %>%
  left_join(cluster_vars %>% st_drop_geometry %>% dplyr::select(lsoa21cd, cluster), by="lsoa21cd")

#Pivot data
pivoted <- fastest_station_scenarios %>%
  st_drop_geometry() %>%
  dplyr::select(scenario1_time_saving, scenario2_time_saving, scenario3_time_saving, scenario4_time_saving, cluster) %>%
  rename(
    "Scenario 1" = scenario1_time_saving,
    "Scenario 2" = scenario2_time_saving,
    "Scenario 3" = scenario3_time_saving,
    "Scenario 4" = scenario4_time_saving) %>%
  pivot_longer(cols = -cluster,
               names_to = "scenario",
               values_to = "time_saving")
pivoted$scenario <- factor(pivoted$scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"))

ggplot(pivoted, aes(x = scenario, y = time_saving, fill = cluster)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cols_changed) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Cumulative Time Savings by Origin LSOA Type",
       x = NULL,
       y = "Reduction in Minutes to an Accessible Station",
       fill = "Cluster") +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9))
rm(pivoted)

#Catchment-wide comparison: this is useful to account for catchment sizes in TfL
station_catchments <- fastest_time_to_stations %>%
  dplyr::select(lsoa21cd, lsoa21nm, fastest_station) %>%
  left_join(fastest_station_scenarios %>% st_drop_geometry() %>% dplyr::select(lsoa21cd, scenario1_time_saving, scenario2_time_saving, scenario3_time_saving, scenario4_time_saving), by = "lsoa21cd")%>%
  left_join(pop_centroids %>% dplyr::select(id, total_pop, total_under_5, total_65_plus, total_disabled), by = c("lsoa21cd" = "id"))%>%
  mutate(total_in_need_pop = total_under_5 + total_65_plus + total_disabled)%>%
  dplyr::select(-total_under_5, -total_65_plus, -total_disabled)%>%
  left_join(tube_stations_main %>% dplyr::select(stop_id, stop_name, classification, upgrade_status)%>%st_drop_geometry(), by=c("fastest_station"="stop_id"))
time_saving_catchments <- station_catchments %>%
  group_by(fastest_station, stop_name, classification, upgrade_status) %>%
  summarise(
    total_population = sum(total_pop),
    total_in_need_population = sum(total_in_need_pop),
    scenario1_time_savings = sum(scenario1_time_saving),
    scenario2_time_savings = sum(scenario2_time_saving),
    scenario3_time_savings = sum(scenario3_time_saving),
    scenario4_time_savings = sum(scenario4_time_saving))%>%
  mutate(pct_in_need = 100*total_in_need_population/total_population)

#Assess job increases x in-need population
time_saving_catchments <- time_saving_catchments %>%
  mutate(scenario1_impact = total_in_need_population*scenario1_time_savings,
         scenario2_impact = total_in_need_population*scenario2_time_savings,
         scenario3_impact = total_in_need_population*scenario3_time_savings,
         scenario4_impact = total_in_need_population*scenario4_time_savings)
sum(time_saving_catchments$scenario1_impact) #25601528
sum(time_saving_catchments$scenario2_impact) #47588740
sum(time_saving_catchments$scenario3_impact) #60289551
sum(time_saving_catchments$scenario4_impact) #7737384
#Scenario 4 least impactful by far: smaller catchments = smaller populations and time savings

sum(time_saving_catchments$scenario3_impact)/sum(time_saving_catchments$scenario1_impact) #2.4x more "impactful" than current upgrades
sum(time_saving_catchments$scenario3_impact)/sum(time_saving_catchments$scenario2_impact) #1.3x more "impactful" than considered stations
#So savings as still more pronounced for scenario 3 than 1 or 2 - but 4 as less so

#Proportion of LSOAs where there was some "impact"
100 * sum(time_saving_catchments$scenario1_impact != 0)/nrow(time_saving_catchments) #10.2%
100 * sum(time_saving_catchments$scenario2_impact != 0)/nrow(time_saving_catchments) #8.47%
100 * sum(time_saving_catchments$scenario3_impact != 0)/nrow(time_saving_catchments) #11.30%
100 * sum(time_saving_catchments$scenario4_impact != 0)/nrow(time_saving_catchments) #11.58%
#So 3 and 4 as more dispersed

rm(station_catchments)

#Plot absolute differences
breaks <- c(0, 1, 10, 20, 30, 40, 50, 75, 100, 125, 150)
tmap_save(
  tm_shape(fastest_station_scenarios) +
    tm_polygons(
      col = "scenario4_time_saving",
      style="fixed",
      breaks=breaks,
      palette="rd_pu",
      alpha=0.9,
      title = "Difference",
      textNA = "",
      border.alpha=0) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_title("Absolute Difference in Time to Nearest Accessible Station, Scenario 4") +
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
  filename = "maps/scenario4_absolute_diffCP_time.png",
  dpi=300)

#Emphasise in-need LSOAs
fastest_station_scenarios <- fastest_station_scenarios %>%
  mutate(in_need25 = if_else(step_free_benefit_indexW>=threshold75, TRUE, FALSE),
         in_need10 = if_else(step_free_benefit_indexW>=threshold90, TRUE, FALSE))
tmap_save(
  tm_shape(fastest_station_scenarios) +
    tm_polygons(
      col = "scenario4_time_saving",
      style="fixed",
      breaks=breaks,
      palette="rd_pu",
      alpha=0.9,
      title = "Difference",
      textNA = "",
      border.alpha=0) +
    tm_shape(fastest_station_scenarios %>% filter(scenario4_time_saving>0 & in_need25)) +
    tm_borders(col = "blue", lwd = 1) +
    tm_shape(fastest_station_scenarios %>% filter(scenario4_time_saving>0 & in_need10)) +
    tm_borders(col = "red", lwd = 1) +
    tm_add_legend(
      type = "line",
      labels = "Top 10% In-Need",
      col = "red")+
    tm_add_legend(
      type = "line",
      labels = "Top 25% In-Need",
      col = "blue")+
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_title("Absolute Difference in Time to Nearest Accessible Station, Scenario 4") +
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
  filename = "maps/scenario4_absolute_diffCP_TIMEhatched.png",
  dpi=300)

rm(breaks, threshold75, threshold90)
