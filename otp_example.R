#This script demonstrates how to run routing using OTP with two London graph configurations:
#A "standard" and wheelchair-accessible graph, both created in schedule_processing.R

#It had been hoped to use this in the actual dissertation
#However, limitations with computational power and multi-route processing meant this was not feasible in the time given

#Nonetheless, I am uploading this here as a reference and proof of concept
#Particularly as single-route wheelchair-accessible routing works well

library(opentripplanner)
library(tidyverse)

# ---- Set up directory structure ----

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

# ----- Build and test "standard" graph --------
log1 <- otp_build_graph(otp = path_otp, dir = otp_path, router = "standard", quiet=FALSE, memory=9216)
log2 <- otp_setup(otp = path_otp, dir = otp_path, router="standard")
otpcon <- otp_connect()
test_route <- otp_plan(otpcon, #Leicester Square to Camden Town
                      fromPlace = c(-0.12811, 51.51145),
                      toPlace = c(-0.142915, 51.53929),
                      mode = c("WALK", "TRANSIT"))

# ------ Build and test wheelchair-accessible graph ------
log3 <- otp_build_graph(otp = path_otp, dir = otp_path, router = "accessible", quiet=FALSE, memory=9216)
log4 <- otp_setup(otp = path_otp, dir = otp_path, router="accessible")
otpcon <- otp_connect()

#Set up routing options - more could be added, e.g. walkSpeed
routingOptions <- otp_routing_options()
routingOptions$wheelchair <- TRUE
routingOptions <- otp_validate_routing_options(routingOptions)

test_route <- otp_plan(otpcon, #Leicester Square to Camden Town - test non-step-free stations don't work
                       fromPlace = c(-0.12811, 51.51145),
                       toPlace = c(-0.142915, 51.53929),
                       mode = c("WALK", "TRANSIT"),
                       routeOptions = routingOptions)
test_route <- otp_plan(otpcon, #High Barnet to Tottenham Court Road - test step-free stations still work
                       fromPlace = c(-0.1943191, 51.65037),
                       toPlace = c(-0.130031, 51.51641),
                       mode = c("WALK", "TRANSIT"),
                       routeOptions = routingOptions)
test_route <- otp_plan(otpcon, #High Barnet to Victoria (change at Euston) - test partially-accessible stations still work
                       fromPlace = c(-0.1943191, 51.65037),
                       toPlace = c(-0.1439399, 51.49588),
                       mode = c("WALK", "TRANSIT"),
                       routeOptions = routingOptions)

#See here for more possibilities:
#https://docs.ropensci.org/opentripplanner/articles/advanced_features.html
