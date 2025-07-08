#Job access through the transport network

#In this file we:
  # - Conduct EDA on the census job data
  # - Modify the r5r file to exclude non-step-free stations
  # - Compare accessibility to jobs for disabled versus non-disabled individuals

#Beforehand, ensure to run:
# 1) lsoa_processing.R
# 2) maps_summary_stats.R
# 3) analysis_r5r.R

library(tmap)
library(tmaptools)
library(maptiles)
library(extrafont)
library(rcartocolor)
library(spdep)
library(tidyverse)

# ------- Job EDA ---------

#First, let's examine the distribution of jobs as opportunities
#lsoa_processing and maps_summary_stats.R should already have been run

#Join workforce population to LSOA sf, get jobs per km^2
study_lsoas_work <- study_lsoas %>%
  left_join(., workforce_centroids, by=c("lsoa21cd" = "id"))%>%
  dplyr::select(-lat, -lon)%>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6,
         jobs_per_km2 = working_pop/area_km2)
summary(study_lsoas_work$jobs_per_km2) #some crazy values in small LSOAs

tmap_mode("plot")
tmap_options(component.autoscale = TRUE)
breaks=c(0, 5000, 10000, 20000, 30000, 40000, 600000)
tmap_save(
  tm_shape(study_lsoas_work) +
    tm_polygons(col = "jobs_per_km2",
                style="fixed",
                breaks=breaks,
                #border.col = "bisque4",
                border.alpha = 0,
                title = "Workers per km\u00B2",
                palette="Peach",
                textNA = "") +
    tm_shape(boroughs)+
    tm_polygons(alpha=0, fill=NA)+
    tm_basemap("Esri.OceanBasemap") +
    tm_title("Distribution of Jobs Across Study LSOAs") +
    tm_compass(type = "8star", 
               size = 3, 
               position = c(0.9, 0.22)) +
    tm_scalebar(position = c(0.82, 0.08), 
                text.size = 0.7, 
                breaks = c(0, 5, 10))+
    tm_layout(legend.position = c(0.01, 0.3), 
              legend.bg.color="white",
              legend.showNA = FALSE,
              title.fontfamily = "Segoe UI Semibold",
              title.size = 1.6,
              legend.text.fontfamily = "Segoe UI",
              legend.title.fontfamily = "Segoe UI Semibold",
              legend.text.size = 0.7,   
              legend.title.size = 0.8),
  filename = "maps/job_dist.png",
  dpi=300)
#We can see CBDs in Central London and Canary Wharf, alongside many jobs in Croydon, Watford
#Interestingly, high concentration of jobs around healthcare locations, e.g. Royal Free, Watford General (maybe less dispersion from WFH?)

#Create spatial weights matrix
#Two possibilities:
  #1) Queen's case with normal centroids
  #2) kNN with workforce centroids

#Queen's Case with geometric centroids
area_nb <- study_lsoas %>%
  poly2nb(., queen=T)
summary(area_nb)
#We could manually join footbridges if needed
#I tried kNN 6 but this rarely crossed the river either - Queen's seemed a better representation
area.lw <- area_nb %>%
  nb2listw(., style="W") 

#Global spatial autocorrelation
morans_i <- study_lsoas_work %>%	
  pull(jobs_per_km2) %>%
  as.vector() %>%
  moran.test(., area.lw)
morans_i

#Potential extensions:
  #Check for local spatial autocorrelation
  #Distribution and LISA facet map?
  #Associations with hospital POI?

# ------- Cumulative Opportunities: All Stations ------
r5r_core <- setup_r5(data_path = "final_r5r", verbose=TRUE)

#For some reason the accessibility function is not working properly, so we will instead manually define a function using travel_time_matrix

#Use same departure times as previously
departure_times <- as.POSIXct(c(
  "2025-10-08 11:00:00",
  "2025-10-08 11:05:00",
  "2025-10-08 11:10:00"))

cumulative_opportunities <- function(origins,
                                     destinations, 
                                     walk_speed = 1.4, 
                                     max_trip_duration = 46, #i.e. stops at 45
                                     mode = c("WALK", "TRANSIT")) {
  
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
        max_rides = 10, #unrealistic, but we just want to see the max!
        progress = TRUE
      ) %>%
        mutate(departure_time = dt)
    }) %>%
    bind_rows()
  
  #Take the average time for each centroid pair, join to job data, find total job access
  average_times <- ttm_combined %>%
    group_by(from_id, to_id) %>%
    summarise(count_under_45 = n(), .groups = "drop") %>%
    left_join(workforce_centroids %>% dplyr::select(id, working_pop), by=c("to_id"="id"))%>%
    mutate(job_lsoa_avg = working_pop*(count_under_45/3)) %>% #if only reached 1/3 times, divide jobs by 3
    group_by(from_id) %>%
    summarise(jobs_45_min = sum(job_lsoa_avg))
  
  return(average_times)
}

#Run with and without PT
jobs_standard <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids)
jobs_standardWALK <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, mode=c("WALK"))
#Note that some of the more rural LSOAs have no jobs within 45 min (even within the LSOA)
#Means ratios may be misleading - will be 0 diff

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)

# ------- Set Up Step-Free r5r -------

#We need to create a new r5r graph, with non-step-free stations removed (this is imperfect, but the only feasible approach given OTP multi-routing won't work)

gtfs <- gtfstools::read_gtfs("final_r5r/gtfs.zip")
summary(gtfs)

#Remove all non/partially accessible stations from GTFS
stops_to_remove <- tube_stations_main %>%
  filter(classification != 'Fully Accessible')%>%
  pull(stop_id)
gtfs$stops <- gtfs$stops %>%
  filter(!stop_code %in% stops_to_remove)
gtfs$stop_times <- gtfs$stop_times %>%
  filter(stop_id %in% gtfs$stops$stop_id)

#Recalculate stop_sequence in gtfs$stop_times
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

# output_path <- tempfile("validation_result")
# validator_path <- download_validator(tempdir())
# gtfstools::validate_gtfs(gtfs, output_path, validator_path) #looks fine, though some trips only have 1 stop - looks like r5r can deal with this manually

#Write new GTFS
dir.create("final_r5r_accessible", recursive = TRUE)
gtfs_write(gtfs, folder = "final_r5r_accessible", name = "gtfs_accessible")

#Move OSM.pbf file there too

#Create r5r_core
r5r_core <- setup_r5(data_path = "final_r5r_accessible", verbose=TRUE)

#Test it seems right
#Checking non-step-free stations aren't included
test_o <- data.frame(id = "o1", lon = -0.12811, lat = 51.51145) #Leicester Square
test_d <- data.frame(id = "d1", lon = -0.142915, lat = 51.53929) #Camden Town
test <- detailed_itineraries(r5r_core,
                             test_o,
                             test_d,
                             mode = c("WALK", "TRANSIT"))
#Checking step-free stations are included
test_o <- data.frame(id = "o1", lon = -0.1943191, lat = 51.65037) #High Barnet
test_d <- data.frame(id = "d1", lon = -0.130031, lat = 51.51641) #TCR
test <- detailed_itineraries(r5r_core,
                             test_o,
                             test_d,
                             mode = c("WALK", "TRANSIT"))

rm(stops_to_remove, output_path, validator_path, test_o, test_d, test)

# ------- Cumulative Opportunities: Step-Free Stations Only ------

#Ceteris paribus; walk speed the same
jobs_accessibleCP <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids)
jobs_accessibleCP_WALK <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, mode=c("WALK")) #this will be the same as walking above!

#Slower walking speed
jobs_accessibleSLOW <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, walk_speed = 0.43)
jobs_accessibleSLOW_WALK <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, walk_speed = 0.43, mode=c("WALK"))

#Combine dataframes
jobs_in_45_min <- study_lsoas %>%
  left_join(jobs_standard, by=c("lsoa21cd"="from_id"))%>%
  rename("jobs_standard" = jobs_45_min)%>%
  left_join(jobs_accessibleCP, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_accessibleCP" = jobs_45_min)%>%
  left_join(jobs_accessibleSLOW, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_accessible_SLOW" = jobs_45_min)%>%
  left_join(jobs_standardWALK, by=c("lsoa21cd"="from_id"))%>%
  rename("jobs_standardWALK" = jobs_45_min)%>%
  left_join(jobs_accessibleCP_WALK, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_accessibleCP_WALK" = jobs_45_min)%>%
  left_join(jobs_accessibleSLOW_WALK, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_accessibleSLOW_WALK" = jobs_45_min)%>%
  mutate(across(starts_with("jobs_"), ~ coalesce(., 0)))

st_write(jobs_in_45_min, "data_export_vis/jobs_in_45_min.gpkg")
rm(jobs_standard, jobs_accessibleCP, jobs_accessibleSLOW,
   jobs_standardWALK, jobs_accessibleCP_WALK, jobs_accessibleSLOW_WALK)

# ----- Summary Statistics ------

#Could alter max_trips?
