#6) Job access through the transport network

#In this file we:
  # - Conduct EDA on the census job data
  # - Modify the r5r file to exclude non-step-free stations
  # - Compare accessibility to jobs for disabled versus non-disabled individuals

#Beforehand, ensure to run files 3-5

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
                                     mode = c("WALK", "TRANSIT"),
                                     max_rides = 10, #unrealistic, but we just want to see the max!
                                     max_walk_time = Inf #not perfect, because this is calculated separately for each leg of a trip
                                     ) {
  
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
        max_rides = max_rides,
        max_walk_time = max_walk_time,
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

#Restrict transfers
jobs_standard_TR <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, max_rides = 1)

#Quick run for 20 min only
jobs_standard20 <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, max_trip_duration=21)

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

#Restrict transfers
jobs_accessibleCP_TR <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, max_rides=1)
jobs_accessibleSLOW_TR <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, walk_speed = 0.43, max_rides=1)

#Restrict walking time
jobs_accessibleCP_WR <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, max_walk_time=10)
jobs_accessibleSLOW_WR <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, walk_speed = 0.43, max_walk_time=10)

#All constraints: speed, restrict transfers, restrict walking time
jobs_accessibleSLOW_WRTR <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, walk_speed = 0.43, max_rides=1, max_walk_time = 10)

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
  left_join(jobs_standard_TR, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_standard_TR" = jobs_45_min)%>%
  left_join(jobs_accessibleCP_TR, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_accessibleCP_TR" = jobs_45_min)%>%
  left_join(jobs_accessibleSLOW_TR, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_accessibleSLOW_TR" = jobs_45_min)%>%
  left_join(jobs_accessibleCP_WR, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_accessibleCP_WR" = jobs_45_min)%>%
  left_join(jobs_accessibleSLOW_WR, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_accessibleSLOW_WR" = jobs_45_min)%>%
  left_join(jobs_accessibleSLOW_WRTR, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_accessibleSLOW_WRTR" = jobs_45_min)%>%
  mutate(across(starts_with("jobs_"), ~ coalesce(., 0)))

st_write(jobs_in_45_min, "data_export_vis/jobs_in_45_min2.gpkg")
#jobs_in_45_min <- st_read("data_export_vis/jobs_in_45_min2.gpkg")

rm(jobs_standard, jobs_accessibleCP, jobs_accessibleSLOW,
   jobs_standardWALK, jobs_accessibleCP_WALK, jobs_accessibleSLOW_WALK,
   jobs_standard_TR, jobs_accessibleCP_TR, jobs_accessibleSLOW_TR,
   jobs_accessibleCP_WR, jobs_accessibleSLOW_WR, jobs_accessibleSLOW_WRTR)

#Quick run for 20 min only
jobs_accessibleCP_20 <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, max_trip_duration = 21)
jobs_accessibleSLOW_20 <- cumulative_opportunities(origins = pop_centroids, destinations = workforce_centroids, max_trip_duration = 21, walk_speed = 0.43)

#Combine 
jobs_in_20_min <- study_lsoas %>%
  left_join(jobs_standard20, by=c("lsoa21cd"="from_id"))%>%
  rename("jobs_standard" = jobs_45_min)%>%
  left_join(jobs_accessibleCP_20, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_accessibleCP" = jobs_45_min)%>%
  left_join(jobs_accessibleSLOW_20, by = c("lsoa21cd"="from_id"))%>%
  rename("jobs_accessible_SLOW" = jobs_45_min)%>%
  mutate(across(starts_with("jobs_"), ~ coalesce(., 0)))

st_write(jobs_in_20_min, "data_export_vis/jobs_in_20_min.gpkg")
rm(jobs_standard20, jobs_accessibleCP_20, jobs_accessibleSLOW_20)

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)

# ----- Summary Statistics ------

#Calculate ratios
jobs_in_45_min <- jobs_in_45_min %>%
  mutate(ratioCP = jobs_accessibleCP/jobs_standard,
         ratioSLOW = jobs_accessible_SLOW/jobs_standard)%>%
  mutate(across(starts_with("ratio"), ~ replace(., is.nan(.), 1))) #if both are null, there is no change!
summary(jobs_in_45_min$ratioCP)
summary(jobs_in_45_min$ratioSLOW)

#Calculate absolute differences
jobs_in_45_min <- jobs_in_45_min %>%
  mutate(ABSdiff_CP = jobs_standard-jobs_accessibleCP,
         ABSdiff_SLOW = jobs_standard-jobs_accessible_SLOW)
summary(jobs_in_45_min$ABSdiff_CP)
summary(jobs_in_45_min$ABSdiff_SLOW)

#Calculate normalised differences
jobs_in_45_min <- jobs_in_45_min %>%
  mutate(diffCP = (jobs_accessibleCP-jobs_standard)/(jobs_accessibleCP+jobs_standard),
         diffSLOW = (jobs_accessible_SLOW-jobs_standard)/(jobs_accessible_SLOW+jobs_standard))%>%
  mutate(across(starts_with("diff"), ~ replace(., is.nan(.), 0))) #if both are null, there is no change!
summary(jobs_in_45_min$diffCP)
summary(jobs_in_45_min$diffSLOW)

#Overview
summary(jobs_in_45_min$jobs_standard)
summary(jobs_in_45_min$jobs_standardWALK)
summary(jobs_in_45_min$jobs_accessibleCP)
summary(jobs_in_45_min$jobs_accessibleCP_WALK)
summary(jobs_in_45_min$jobs_accessible_SLOW)
summary(jobs_in_45_min$jobs_accessibleSLOW_WALK)

#Transfer-restricted
summary(jobs_in_45_min$jobs_standard_TR)
summary(jobs_in_45_min$jobs_accessibleCP_TR)
summary(jobs_in_45_min$jobs_accessibleSLOW_TR)

#Walk-time restricted
summary(jobs_in_45_min$jobs_accessibleCP_WR)
summary(jobs_in_45_min$jobs_accessibleSLOW_WR)

#All restrictions
summary(jobs_in_45_min$jobs_accessibleSLOW_WRTR)
#WR as a greater constraint than TR

#Find average figures, weighted by number of non-disabled and disabled people
calculations <- pop_centroids %>%
  dplyr::select(id, total_disabled, total_pop)%>%
  mutate(total_non_disabled = total_pop-total_disabled)%>%
  left_join(jobs_in_45_min, by=c("id" = "lsoa21cd"))
calculations <- calculations %>%
  mutate(jobs_non_disabled_multiplied = jobs_standard * total_non_disabled,
         jobs_CP_multiplied = jobs_accessibleCP * total_disabled,
         jobs_SLOW_multiplied = jobs_accessible_SLOW * total_disabled)
total_disabled <- sum(calculations$total_disabled)
total_non_disabled <- sum(calculations$total_non_disabled)
avg_jobs_non_disabled <- sum(calculations$jobs_non_disabled_multiplied)/total_non_disabled
avg_jobs_disabled_CP <- sum(calculations$jobs_CP_multiplied)/total_disabled
avg_jobs_disabled_SLOW <- sum(calculations$jobs_SLOW_multiplied)/total_disabled
print(avg_jobs_non_disabled) #70672.32
print(avg_jobs_disabled_CP) #58189.51
print(avg_jobs_disabled_SLOW) #1766.916

#Average differences: CP and slow
print(avg_jobs_non_disabled - avg_jobs_disabled_CP) #CP: 12482.81
print(avg_jobs_non_disabled - avg_jobs_disabled_SLOW) #Slow: 68905.4

#As percentages
round(100 * avg_jobs_disabled_CP/avg_jobs_non_disabled, 2) #CP: 82.3%
round(100 * avg_jobs_disabled_SLOW/avg_jobs_non_disabled, 2) #Slow: 2.5%

#Extra benefit from PT
jobs_in_45_min <- jobs_in_45_min %>%
  mutate(standard_PTbenefit = jobs_standard-jobs_standardWALK,
         CP_PTbenefit = jobs_accessibleCP-jobs_accessibleCP_WALK,
         SLOW_PTbenefit = jobs_accessible_SLOW - jobs_accessibleSLOW_WALK)
summary(jobs_in_45_min$standard_PTbenefit)
summary(jobs_in_45_min$CP_PTbenefit)
summary(jobs_in_45_min$SLOW_PTbenefit)

#Proportions of jobs accessed through PT
median(jobs_in_45_min$standard_PTbenefit)/median(jobs_in_45_min$jobs_standard)
median(jobs_in_45_min$CP_PTbenefit)/median(jobs_in_45_min$jobs_accessibleCP)
median(jobs_in_45_min$SLOW_PTbenefit)/median(jobs_in_45_min$jobs_accessibleSLOW)

#Quick 20 min comparison
summary(jobs_in_20_min$jobs_standard)
summary(jobs_in_20_min$jobs_accessibleCP)
summary(jobs_in_20_min$jobs_accessible_SLOW)

#Find average figures, weighted by number of non-disabled and disabled people
calculations <- pop_centroids %>%
  dplyr::select(id, total_disabled, total_pop)%>%
  mutate(total_non_disabled = total_pop-total_disabled)%>%
  left_join(jobs_in_20_min, by=c("id" = "lsoa21cd"))
calculations <- calculations %>%
  mutate(jobs_non_disabled_multiplied = jobs_standard * total_non_disabled,
         jobs_CP_multiplied = jobs_accessibleCP * total_disabled,
         jobs_SLOW_multiplied = jobs_accessible_SLOW * total_disabled)
avg_jobs_non_disabled <- sum(calculations$jobs_non_disabled_multiplied)/total_non_disabled
avg_jobs_disabled_CP <- sum(calculations$jobs_CP_multiplied)/total_disabled
avg_jobs_disabled_SLOW <- sum(calculations$jobs_SLOW_multiplied)/total_disabled
print(avg_jobs_non_disabled) #2085.214
print(avg_jobs_disabled_CP) #2023.299
print(avg_jobs_disabled_SLOW) #460.6448

#Average differences: CP and slow
print(avg_jobs_non_disabled - avg_jobs_disabled_CP) #CP: 61.91485
print(avg_jobs_non_disabled - avg_jobs_disabled_SLOW) #Slow: 1624.569

#As percentages
round(100 * avg_jobs_disabled_CP/avg_jobs_non_disabled, 2) #CP: 97.03%
round(100 * avg_jobs_disabled_SLOW/avg_jobs_non_disabled, 2) #Slow: 22.09%

#Walking seems to be responsible for bulk of job access!

#Are patterns more pronounced in areas which are more served by public transport?
#I tried filtering to London LSOAs but proportions were actually very similar (82.39% and 2.48%)
#What about filtering out certain boroughs?

boroughs_to_remove <- c("Bexley", "Bromley", "Greenwich", "Sutton", "Kingston upon Thames") #all contain just 0 or 1 stations
boroughs_to_keep <- boroughs %>%
  filter(!name %in% boroughs_to_remove)%>%
  st_transform(st_crs(jobs_in_45_min))
jobs_in_45_min_NEW_AREA <- jobs_in_45_min %>%
  filter(lsoa21cd %in% london_codes$lsoa21cd)%>%
  st_filter(., boroughs_to_keep, .predicate = st_intersects)

summary(jobs_in_45_min_NEW_AREA$jobs_standard)
summary(jobs_in_45_min_NEW_AREA$jobs_accessibleCP)
summary(jobs_in_45_min_NEW_AREA$jobs_accessible_SLOW)

#Weighted averages
calculations <- pop_centroids %>%
  dplyr::select(id, total_disabled, total_pop)%>%
  mutate(total_non_disabled = total_pop-total_disabled)%>%
  inner_join(jobs_in_45_min_NEW_AREA, by=c("id" = "lsoa21cd"))
calculations <- calculations %>%
  mutate(jobs_non_disabled_multiplied = jobs_standard * total_non_disabled,
         jobs_CP_multiplied = jobs_accessibleCP * total_disabled,
         jobs_SLOW_multiplied = jobs_accessible_SLOW * total_disabled)
avg_jobs_non_disabled <- sum(calculations$jobs_non_disabled_multiplied)/total_non_disabled
avg_jobs_disabled_CP <- sum(calculations$jobs_CP_multiplied)/total_disabled
avg_jobs_disabled_SLOW <- sum(calculations$jobs_SLOW_multiplied)/total_disabled
print(avg_jobs_non_disabled) #66509.12
print(avg_jobs_disabled_CP) #53821.65
print(avg_jobs_disabled_SLOW) #1569.763

print(avg_jobs_non_disabled - avg_jobs_disabled_CP) #CP: 12687.46
print(avg_jobs_non_disabled - avg_jobs_disabled_SLOW) #Slow: 64939.35

round(100 * avg_jobs_disabled_CP/avg_jobs_non_disabled, 2) #CP: 80.92%
round(100 * avg_jobs_disabled_SLOW/avg_jobs_non_disabled, 2) #Slow: 2.36%

#So even still, it's not as pronounced as expected
#I also tried for only LSOAs within 2km of tube stops and proportions were v similar (80.6%, 2.36%)

rm(total_disabled, total_non_disabled, calculations, avg_jobs_non_disabled, avg_jobs_disabled_CP, avg_jobs_disabled_SLOW, jobs_in_45_min_NEW_AREA)

# ----- Display Results -----

#Violin plot of job distributions
pivoted <- jobs_in_45_min %>%
  st_drop_geometry() %>%
  dplyr::select(jobs_standard, jobs_accessibleCP, jobs_accessible_SLOW) %>%
  rename(
    "Jobs Within 45 Minutes" = jobs_standard,
    "Jobs Within 45 Minutes, \nStep-Free" = jobs_accessibleCP,
    "Jobs Within 45 Minutes,\n Step-Free and \nSlower Walking Speed" = jobs_accessible_SLOW
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "type",
               values_to = "value")
pivoted$type <- factor(pivoted$type, levels = c(
  "Jobs Within 45 Minutes",
  "Jobs Within 45 Minutes, \nStep-Free",
  "Jobs Within 45 Minutes,\n Step-Free and \nSlower Walking Speed"
))

ggplot(pivoted, aes(x = type, y = value, fill = type)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Distribution of Accessible Jobs Within 45 Minutes",
       x = "Travel Type",
       y = "Jobs",
       caption = "Please note that the y axis actually extends further for the first two categories.") +
  ylim(0, 100000) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.caption = element_text(family = "Segoe UI Light", size = 8, hjust=0))
#Again, walking as responsible for most of the job accessibility
#Distributions are fairly constant

#Plot distributions of ratios
pivoted <- jobs_in_45_min %>%
  st_drop_geometry() %>%
  dplyr::select(ratioCP, ratioSLOW) %>%
  rename(
    "Speed Unchanged" = ratioCP,
    "Slower Walking Speed" = ratioSLOW
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "type",
               values_to = "value")
ggplot(pivoted, aes(x = type, y = value, fill = type)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Distribution of Job Accessiblity Ratios",
       x = NULL,
       y = "Ratio of Accessible Jobs versus no Mobility Restrictions") +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    axis.title.x = element_text(margin = margin(t = 10)))+
  coord_flip()
rm(pivoted)

#Map absolute differences
breaks <- c(0, 5000, 10000, 20000, 60000, 100000, 200000, 300000, 800000)
tmap_save(
  tm_shape(jobs_in_45_min) +
    tm_polygons(
      col = "ABSdiff_CP",
      style="fixed",
      breaks=breaks,
      palette="rd_pu",
      alpha=0.9,
      title = "Difference",
      textNA = "",
      border.alpha=0) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_title("Absolute Difference in Accessible Jobs within 45 Minutes") +
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
  filename = "maps/job_absolute_diffCP.png",
  dpi=300)

breaks <- c(0, 10000, 30000, 60000, 100000, 200000, 400000, 600000, 1000000)
tmap_save(
  tm_shape(jobs_in_45_min) +
    tm_polygons(
      col = "ABSdiff_SLOW",
      style="fixed",
      breaks=breaks,
      palette="rd_pu",
      alpha=0.9,
      title = "Difference",
      textNA = "",
      border.alpha=0) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_title("Absolute Difference in Accessible Jobs within 45 Minutes, \nSlower Walking Speed") +
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
  filename = "maps/job_absolute_diffSLOW.png",
  dpi=300)

#Map ratios
#CP
breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
tmap_save(
  tm_shape(jobs_in_45_min) +
    tm_polygons(
      col = "ratioCP",
      style="fixed",
      breaks=breaks,
      palette="-rd_pu",
      alpha=0.9,
      title = "Ratio",
      textNA = "",
      border.alpha=0
    ) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_title("Ratio of Accessible Jobs: Step-Free Stations Versus No Constraints") +
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
  filename = "maps/job_ratioCP.png",
  dpi=300)
#Slow
breaks <- c(0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1)
tmap_save(
  tm_shape(jobs_in_45_min) +
    tm_polygons(
      col = "ratioSLOW",
      style="fixed",
      breaks=breaks,
      palette="-rd_pu",
      alpha=0.9,
      title = "Ratio",
      textNA = "",
      border.alpha=0
    ) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_title("Ratio of Accessible Jobs: Step-Free Stations Versus No Constraints, \nSlower Walking Speed") +
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
  filename = "maps/job_ratioSLOW.png",
  dpi=300)
#Larger LSOAs as a problem - sizes are just so unrepresentative

#Join to in-need index
jobs_in_45_min <- jobs_in_45_min %>%
  left_join(., (pop_centroids %>% dplyr::select(id, step_free_benefit_indexW)), by = c("lsoa21cd" = "id"))
cor.test(jobs_in_45_min$ratioCP, jobs_in_45_min$step_free_benefit_indexW)
cor.test(jobs_in_45_min$ratioSLOW, jobs_in_45_min$step_free_benefit_indexW)
#Weak positive linear association, i.e. greater disparity and less in-need population (statistically significant)

#Bivariate choropleths: accessibility ratio versus proportion

#Need to add some slight noise to the data so we can add quantiles
bivariate_data <- jobs_in_45_min %>%
  dplyr::select(lsoa21cd, ratioCP, ratioSLOW, step_free_benefit_indexW)%>%
  mutate(ratioCP_jitter = jitter(ratioCP, amount = 1e-6),
         invCP=1/ratioCP_jitter,
         invSLOW=1/ratioSLOW,
         invSLOW = ifelse(is.infinite(invSLOW), 1100, invSLOW)) #invert, so higher values = greater disparity 

bi_data <- bi_class(bivariate_data, x = invCP, y = step_free_benefit_indexW, style = "quantile", dim = 4)
pal <- bi_pal("GrPink2", dim = 4, preview = FALSE)
bi_classes <- names(pal)
tmap_save(
  tm_shape(bi_data) +
    tm_polygons("bi_class",
                palette = pal,
                border.alpha = 0,
                legend.show = FALSE) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_compass(type = "8star",
               size = 3,
               position = c(0.9, 0.22)) +
    tm_scalebar(
      position = c(0.82, 0.08),
      text.size = 0.7,
      breaks = c(0, 5, 10)) +
    tm_title("Step-Free Accessibility Disparity versus Presence of In-Need Population")+
    tm_layout(
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.2,
      bg.color = "grey70"),
  filename = "maps/bivariate_choropleth_job_pop.png",
  dpi = 300)

bi_data <- bi_class(bivariate_data, x = invSLOW, y = step_free_benefit_indexW, style = "quantile", dim = 4)
pal <- bi_pal("GrPink2", dim = 4, preview = FALSE)
bi_classes <- names(pal)
tmap_save(
  tm_shape(bi_data) +
    tm_polygons("bi_class",
                palette = pal,
                border.alpha = 0,
                legend.show = FALSE) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_compass(type = "8star",
               size = 3,
               position = c(0.9, 0.22)) +
    tm_scalebar(
      position = c(0.82, 0.08),
      text.size = 0.7,
      breaks = c(0, 5, 10)) +
    tm_title("Step-Free Accessibility Disparity versus Presence of In-Need Population, \nSlower Walking Speed")+
    tm_layout(
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.2,
      bg.color = "grey70"),
  filename = "maps/bivariate_choropleth_job_popSLOW.png",
  dpi = 300)
#We can use the same legends as for the travel time analysis
#Again, slower walking speeds in this context as not particularly helpful - bias towards larger LSOAs

rm(bi_classes, boroughs_to_remove, pal, bi_data, bivariate_data, boroughs_to_keep)
