#Travel time to the transport network

#In this file we:
  # - Use r5r to calculate travel time from each LSOA to the nearest accessible station
    # - For each non-disabled individual, this is simply the nearest station
    # - For PwMD, this is the nearest accessible station
  # - Calculate summary statistics and create maps
  # - Assess local and global spatial autocorrelation in time ratios

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

#Find the average difference, weighted by number of disabled people
calculations <- pop_centroids %>%
  select(id, total_disabled)%>%
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
#Horizontal equity: still a difference
#Vertical equity: needs considerable change

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
  ) +
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

breaks <- c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 50)
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
  left_join(., (pop_centroids %>% select(id, step_free_benefit_indexW)), by = c("lsoa21cd" = "id"))

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

# ---- Overlaps with stations ------

#Extract high-high areas
HH_CP <- fastest_time_to_stations %>% filter(hs_CP == "High-High")
HH_SLOW <- fastest_time_to_stations %>% filter(hs_SLOW == "High-High")

tmap_mode("view")
tm_shape(HH_SLOW)+
  tm_polygons()+
  tm_shape(tube_stations_main %>% filter(classification != "Fully Accessible"))+
  tm_dots(col="upgrade_status", palette="Dark2")
#How to choose? Lots of stations are close by but not in the HH areas
#Could do a buffer?

#To do:
# - Extra r5r scenarios - no interchanges? Walking only?
  # - Considering whether the bus network is the thing mitigating the disparity (context of LA cuts)
  # - What does this mean contextually during a time of cuts?
# - Identify overlaps with inaccessible stations
  # - Compare to TfL list?
  # - Could I add vehicle ownership as a factor in the index?
# - RUN OTP DIRECTLY IN JAVA!!
  # - Or a for-loop, but unlikely to run on time

#Need to consider the issue that detailed_itineraries provides more realistic travel times than travel_time_matrix
#Hence some LSOAs having unexpectedly long walks

#Note "fastest" may not actually be in practice - consider issues for PwMD on buses, e.g. no space, ramps
#Or closest accessible may not actually be ideal - e.g. further away from Zone 1
