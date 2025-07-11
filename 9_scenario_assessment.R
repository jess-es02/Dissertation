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

rm(jobs_accessibleCP, jobs_accessibleSLOW, results_CP, results_SLOW, scenario_ids, departure_times, scenario, cumulative_opportunities)

#Need to calculate job ratios!
#And beforehand coalesce nulls to zeroes
#Then analysis: totals, averages, numbers in clusters, network efficiencies?