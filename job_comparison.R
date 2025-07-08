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

#Removed OTP code - some notes from this:
#This is all for now, but we could alter speed for electric wheelchairs, for example
#Or keep speed the same as standard for a ceteris paribus comparison
#Could also alter walkReluctance, but I am considering what is theoretically possible rather than what is preferable
#Could add maximum transfers to be more realistic?

# ----- Cumulative opportunities measure -------
