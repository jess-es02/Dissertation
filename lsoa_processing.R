#Processing socio-demographic data at LSOA-level

#In this file, we:
  # - Delineate the study area: all LSOAs in Greater London, and any within 2km of a tube/Overground stop outside of this area
  # - Prepare origins: population-weighted centroids
    # - We append information on total population, disabled population, and age bands
  # - Prepare destinations: workforce-weighted centroids (derived from OA-level statistics)
    # - We append working population as a proxy for local opportunities
  # - Create an index (using pct under 5, over 65, and disabled) to indicate presence of groups potentially benefitting from step-free upgrades

library(tidyverse)
library(janitor)
options(java.parameters = "-Xmx2G")
library(r5r)
library(gtfstools)
library(sf)
library(cols4all)
library(tmap)
library(tmaptools)
library(extrafont)

gtfs <- gtfstools::read_gtfs("final_r5r/gtfs.zip")
summary(gtfs)

# ----- Prepare AOI ------

#LSOA boundaries
lsoas <- st_read("data/LSOA_2021_EW_BSC_V4.shp")%>%
  clean_names()%>%
  select("lsoa21cd", "lsoa21nm", "geometry")

#Load in a csv with only London LSOA codes
#obtained from a random download from London Datastore (https://data.london.gov.uk/census/2021-ward-and-lsoa-estimates/)
london_codes <- read_csv("data/london_lsoas.csv")%>%
  clean_names()

#Filter for only London LSOAs
london_lsoas <- lsoas %>%#
  filter(lsoa21cd %in% london_codes$lsoa21cd)

#Add on any LSOAs within 2km of tube/Overground stops outside of London
#Decided to exclude the Lizzie line as all stations are accessible anyway, and it extends very far outside London

#Find tube stops
stops_on_tube_trips <- gtfs$routes %>%
  filter(route_type == 1 | (route_type == 2 & agency_id == 'LO'))%>%
  left_join(., gtfs$trips, by ="route_id")%>%
  distinct() %>% #all tube trips
  select(trip_id)%>%
  left_join(., gtfs$stop_times, by="trip_id")%>%
  select(stop_id)%>%
  distinct() %>%
  left_join(., gtfs$stops, by="stop_id")%>%
  st_as_sf(., coords = c("stop_lon", "stop_lat"), crs=4326)%>%
  st_transform(., 27700)

#Let's do a 2km buffer, to reflect people who can feasibly walk to these stops
stop_buffers <- st_buffer(stops_on_tube_trips, dist = 2000)
#Find LSOAs intersecting with the stop buffers
stop_buffer_lsoas <- st_filter(lsoas, stop_buffers)

#Combine all potential LSOAs: those in London, and those within 2km of a London transport stop
study_lsoas <- rbind(london_lsoas, stop_buffer_lsoas)%>%
  distinct(lsoa21cd, .keep_all = TRUE)
#st_write(study_lsoas, "data_export_vis/study_lsoas_new.gpkg", layer = "study_lsoas_O")
#st_write(stops_on_tube_trips, "data_export_vis/tube_stops_all.gpkg")

#Get bounding box for bbike OSM.pbf extract - considering both study LSOAs and all stops in the network
all_stops <- gtfs$stops %>%
  st_as_sf(., coords = c("stop_lon", "stop_lat"), crs=4326)
bbox_stops <- st_bbox(all_stops)
bbox_lsoas <- st_bbox(study_lsoas%>%st_transform(., 4326))
bbox_combined <- c(
  xmin = min(bbox_stops["xmin"], bbox_lsoas["xmin"]),
  ymin = min(bbox_stops["ymin"], bbox_lsoas["ymin"]),
  xmax = max(bbox_stops["xmax"], bbox_lsoas["xmax"]),
  ymax = max(bbox_stops["ymax"], bbox_lsoas["ymax"]))
print(bbox_combined)

# ----- Origins: pop-weighted centroids -------

#LSOA pop-weighted centroids: r5r-compatible format
pop_centroids <- read_csv("data/lsoa_pop_weighted_centroids.csv")%>%
  clean_names()%>%
  #st_as_sf(., coords = c("x", "y"), crs = 4326)%>%
  #st_transform(., crs=27700)%>%
  select("lsoa21cd", "x", "y")%>%
  rename("id" = lsoa21cd,
         "lon" = x,
         "lat" = y) %>%
  filter(id %in% study_lsoas$lsoa21cd)

#One centroid does not join to the r5r network - manually moving this

#a) Hillingdon 001E - in a gated community
#Moving it very slightly so it aligns with the next (non-private) road over
pop_centroids <- pop_centroids %>%
  mutate(lon = if_else(id == 'E01002482', -0.410789, lon),
         lat = if_else(id == 'E01002482', 51.61021, lat))

# ------ Destinations: workplace-weighted centroids -------
#The ONS does not release these, so we will estimate using OA-level data

#First, load in OA shapefile
oas <- st_read("large_data/OA_2021_EW_BFC_V8.shp")%>%
  clean_names() %>%
  select(oa21cd, lsoa21cd, lat, long) #this is a representative centroid which always falls inside the OA; different to a centroid

#Filter for only OAs in study area
oas <- oas %>%
  filter(lsoa21cd %in% study_lsoas$lsoa21cd)

#Load in OA-level working population
working_pop_oa <- read_csv("data/workforce_pop_oa.csv")%>%
  clean_names()%>%
  rename("oa21cd" = output_areas_code, 
         "working_pop" = count)

#Append working population to sf object
oas <- oas %>%
  left_join(., working_pop_oa, by = "oa21cd")

#Find weighted average: r5r-compatible formatting
#We use the provided "representative centroids", but could change these to geometric centroids
workforce_centroids <- oas %>%
  st_drop_geometry()%>%
  group_by(lsoa21cd)%>%
  summarise(
    lat = sum(lat * working_pop)/sum(working_pop),
    lon = sum(long * working_pop)/sum(working_pop))%>%
  rename("id" = lsoa21cd)#%>%
#st_as_sf(., coords = c("lon", "lat"), crs=4326)%>%
#st_transform(., 27700)

#Four centroids do not join to the r5r network - manually moving these

#a) Hillingdon 001E - on a gated road
#Moving to the closest non-gated road
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01002482', -0.41118, lon),
         lat = if_else(id == 'E01002482', 51.61036, lat))

#b) Kingston upon Thames 020C - inside Chessington World of Adventures!
#Move coordinates to the park entrance
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01002948', -0.314287, lon),
         lat = if_else(id == 'E01002948', 51.35000, lat))

#c) Hillingdon 031A - inside Heathrow
#Tricky to find an optimal place to move the centroid, because r5r does not support indoor routing
#Let's move to a spot very close to the PT stops and see if that works
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01002444', -0.45215, lon),
         lat = if_else(id == 'E01002444', 51.47121, lat))
#This is the only spot I found which works
#But is likely to overestimate accessibility to jobs as it is so close to the station entrance - in reality, people would have to walk more

#d) Three Rivers 011B - inside an RAF base
#Move coordinates to the entrance
workforce_centroids <- workforce_centroids %>%
  mutate(lon = if_else(id == 'E01023840', -0.408365, lon),
         lat = if_else(id == 'E01023840', 51.61951, lat))

# ------ Origin Attributes --------

#Age: proportion under 5 and 65+ (TS007B - Age by broad age bands)
age <- read_csv("data/nomis_age.csv")%>%
  clean_names()%>%
  rename(
    "id" = mnemonic,
    "total_pop" = total,
    "total_under_5" = aged_4_years_and_under)%>%
  mutate(total_65_plus = aged_65_to_74_years + aged_75_to_84_years + aged_85_years_and_over,
         pct_65_plus = round(100*total_65_plus/total_pop, 3),
         pct_under_5 = round(100*total_under_5/total_pop, 3))%>%
  select(id, total_pop, total_under_5, pct_under_5, total_65_plus, pct_65_plus)

#Disability: disabled under equality act (TS038 - Disability)
disability <- read_csv("data/nomis_disability.csv")%>%
  clean_names()%>%
  rename(
    "id" = mnemonic,
    "total_pop" = total_all_usual_residents,
    "total_disabled" = disabled_under_the_equality_act)%>%
  mutate(pct_disabled = round(100*total_disabled/total_pop, 3))%>%
  select(id, total_disabled, pct_disabled)

#Join to pop centroids
pop_centroids <- pop_centroids %>%
  left_join(., age, by="id")%>%
  left_join(., disability, by="id")

# ------ Destination Attributes: workforce pop -------

working_pop_lsoa <- read_csv("data/workforce_pop_lsoa.csv")%>%
  clean_names()%>%
  rename("id" = lower_layer_super_output_areas_code, 
         "working_pop" = count)%>%
  select(-lower_layer_super_output_areas_label)

#Join to workforce centroids
workforce_centroids <- workforce_centroids %>%
  left_join(., working_pop_lsoa, by="id")

rm(lsoas, oas, working_pop_lsoa, working_pop_oa, age, disability, london_lsoas, stop_buffers, stop_buffer_lsoas, stops_on_tube_trips, bbox_combined, bbox_lsoas, bbox_stops, all_stops)

# ---- Create pop-centroid step-free benefit index ------
pop_centroids <- pop_centroids %>%
  mutate(across(starts_with("pct"), ~ as.numeric(scale(.)), .names = "z_{.col}"))

#Unweighted: average of all three z scores
pop_centroids <- pop_centroids %>%
  mutate(step_free_benefit_indexUW = rowMeans(across(c(z_pct_disabled, z_pct_under_5, z_pct_65_plus))))

#Weighted: 0.6 for disabled, 0.2 for under 5 or 65+
pop_centroids <- pop_centroids %>%
  mutate(step_free_benefit_indexW = 0.6*z_pct_disabled + 0.2*z_pct_under_5 + 0.2*z_pct_65_plus)

#Map these
lsoa_attributes <- study_lsoas %>%
  left_join(., pop_centroids, by=c("lsoa21cd" = "id"))%>%
  select(lsoa21cd, step_free_benefit_indexUW, step_free_benefit_indexW, geometry)%>%
  rename("Unweighted Benefit Index" = step_free_benefit_indexUW,
         "Weighted Benefit Index" = step_free_benefit_indexW)

breaks <- c(-3, -2, -1, 0, 1, 2, 3, 4)
palette_colors <- c4a("bu_wh_rd", n = length(breaks) - 1)
break_labels <- c("-3 to -2", "-2 to -1", "-1 to 0", "0 to 1", "1 to 2", "2 to 3", "3 to 4")

tmap_save(
  tm_shape(lsoa_attributes) +
    tm_polygons(
      fill = c("Unweighted Benefit Index", "Weighted Benefit Index"),
      palette = "bu_wh_rd",
      breaks = breaks,
      fill.legend = tm_legend(title = ""),
      fill.free = FALSE,
      legend.show = FALSE,
      textNA = ""
    ) +
    tm_facets(ncol = 2) +
    #tm_add_legend(type = "fill", labels = break_labels, col = palette_colors) +
    tm_basemap("Esri.OceanBasemap") +
    tm_title("Distribution of Populations in Need of Step-Free Access") +
    tm_layout(
      legend.outside = TRUE,
      legend.outside.position = "left",
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.5,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9
    ),
  filename = "maps/population_indices.png",
  dpi = 300
)
#Might have to manually combine the legend in the document

rm(lsoa_attributes, break_labels, breaks, legend_labels)
