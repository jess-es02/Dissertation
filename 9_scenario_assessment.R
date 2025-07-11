#9) Assessment of station upgrade scenarios

#In this file, we:
  #Build the new r5r cores for each station upgrade scenario
  #Simulate accessibility changes
  #Assess impact

#Scenarios:
  #1: TfL Project Underway
  #2) TfL Under Evaluation (only 7!!)
  #3) Equity - Catchment Prioritisation
  #4) Network
#Note TfL's stations have been updated since starting this project - these were correct as of June 2025

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

# ------ Build new r5r cores ------