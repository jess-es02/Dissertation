library(opentripplanner)
library(tmap)
library(tmaptools)
library(maptiles)
library(extrafont)
library(rcartocolor)
library(spdep)
library(tidyverse)

# ------- Job EDA ---------

#First, let's examine the distribution of jobs as opportunities
#lsoa_processing should already have been run

#Join workforce population to LSOA sf, get jobs per km^2
study_lsoas_work <- study_lsoas %>%
  left_join(., workforce_centroids, by=c("lsoa21cd" = "id"))%>%
  select(-lat, -lon)%>%
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
                border.col = "bisque4",
                title = "Workers per km\u00B2",
                palette="Peach",
                textNA = "") +
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

# ----- Set up OTP routing -------

#Temporarily switch from Java 21 (for r5r) to 17 (for OTP)
Sys.setenv(JAVA_HOME = Sys.getenv("JAVA_HOME17"))
Sys.setenv(PATH = paste0(Sys.getenv("JAVA_HOME"), "/bin;", Sys.getenv("PATH")))

#Set up the OTP directory
otp_path <- "otp"
dir.create(otp_path, recursive = TRUE)
path_otp <- otp_dl_jar(otp_path, cache = TRUE, version = "2.2.0")

#Create directory structure
dir.create("otp/graphs/standard", recursive = TRUE)
dir.create("otp/graphs/accessible", recursive = TRUE)

#Next, manually paste the GTFS/accessible GTFS into each
#Alongside the OSM road network, filtered with osmium

#Build graphs

#Standard
# log1 <- otp_build_graph(otp = path_otp, dir = otp_path, router = "standard", quiet=FALSE, memory=9216)
# log2 <- otp_setup(otp = path_otp, dir = otp_path, router="standard")
# otpcon <- otp_connect()
# test_route <- otp_plan(otpcon, #Leicester Square to Camden Town
#                       fromPlace = c(-0.12811, 51.51145),
#                       toPlace = c(-0.142915, 51.53929),
#                       mode = c("WALK", "TRANSIT"))
# qtm(test_route%>%filter(route_option==2)) #all looks good
# otp_stop()

#Wheelchair accessible
# log3 <- otp_build_graph(otp = path_otp, dir = otp_path, router = "accessible", quiet=FALSE, memory=9216)
# log4 <- otp_setup(otp = path_otp, dir = otp_path, router="accessible")
# otpcon <- otp_connect()
# routingOptions <- otp_routing_options()
# routingOptions$wheelchair <- TRUE
# routingOptions <- otp_validate_routing_options(routingOptions)
# test_route <- otp_plan(otpcon, #Leicester Square to Camden Town - test non-step-free stations don't work
#                        fromPlace = c(-0.12811, 51.51145),
#                        toPlace = c(-0.142915, 51.53929),
#                        mode = c("WALK", "TRANSIT"),
#                        routeOptions = routingOptions)
# test_route <- otp_plan(otpcon, #High Barnet to Tottenham Court Road - test step-free stations still work
#                        fromPlace = c(-0.1943191, 51.65037),
#                        toPlace = c(-0.130031, 51.51641),
#                        mode = c("WALK", "TRANSIT"),
#                        routeOptions = routingOptions)
# test_route <- otp_plan(otpcon, #High Barnet to Victoria (change at Euston) - test partially-accessible stations still work
#                        fromPlace = c(-0.1943191, 51.65037),
#                        toPlace = c(-0.1439399, 51.49588),
#                        mode = c("WALK", "TRANSIT"),
#                        routeOptions = routingOptions)
# tmap_mode("view")
# qtm(test_route %>% filter(route_option == 3), col = "leg_mode")

#Configure options

#Standard: no wheelchair, walking speed 1.4m/s, no maximum walk
routingOptionsS <- otp_routing_options()
routingOptionsS$walkSpeed <- 1.4
routingOptionsS <- otp_validate_routing_options(routingOptionsS)

#Wheelchair: wheelchair, walking speed 0.43m/s (Sonenblum et al., 2012)
routingOptionsW <- otp_routing_options()
routingOptionsW$walkSpeed <- 0.43
#routingOptionsW$maxWalkDistance <- 1000
routingOptionsW$wheelchair <- TRUE
routingOptionsW <- otp_validate_routing_options(routingOptionsW)
#Max walk distance cannot be set here - return to this if time

#This is all for now, but we could alter speed for electric wheelchairs, for example
#Or keep speed the same as standard for a ceteris paribus comparison
#Could also alter walkReluctance, but I am considering what is theoretically possible rather than what is preferable
#Could add maximum transfers to be more realistic?

# ----- Cumulative opportunities measure -------

#Define origins and destinations
selected_ids <- head(pop_centroids$id, 5)
sample_origins <- pop_centroids[pop_centroids$id %in% selected_ids, ]
sample_destinations <- workforce_centroids[workforce_centroids$id %in% selected_ids, ]

toPlace = sample_destinations[rep(seq(1, nrow(sample_destinations)), times = nrow(sample_destinations)),]
toPlace <- st_as_sf(toPlace, coords = c("lon", "lat"), crs = 4326) %>%
  select(id, geometry)

fromPlace = sample_origins[rep(seq(1, nrow(sample_origins)), each  = nrow(sample_origins)),]
fromPlace <- st_as_sf(fromPlace, coords = c("lon", "lat"), crs = 4326) %>%
  select(id, geometry)

#Wheelchair

#Load wheelchair object
log4 <- otp_setup(otp = path_otp, dir = otp_path, router="accessible")
otpcon <- otp_connect()

test <- otp_plan(otpcon,
                 fromPlace = fromPlace,
                 toPlace = toPlace,
                 fromID = fromPlace$id,
                 toID = toPlace$id,
                 get_geometry = FALSE,
                 distance_balance = TRUE,
                 mode = c("WALK", "TRANSIT"),
                 routeOptions = routingOptionsW,
                 numItineraries = 1,
                 #ncores = max(round(parallel::detectCores() * 1.25) - 1, 1))
)

#Would need to set date, time, maxWalkDistance, numItineraries

#To do:
# - Trial query from origin to destination centroids
# - Could use r5r to map time to nearest accessible station vs nearest station in general
# - Could I consider number of transfers in cumulative opportunities, not just opportunities reached?
# - For cumulative opportunities, could I also compare with an interchange restriction? More realistic for PwMD, indicates convenience etc.
# - Bivariate Moran's i with vehicle ownership?
# - Isochrones, not just job access?

# - Configure settings
# - Run accessibility and isochrones for each

gtfs <- gtfstools::read_gtfs("final_r5r/gtfs_accessible.zip")
view(gtfs$stops)
view(gtfs$pathways)
