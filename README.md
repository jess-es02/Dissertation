# Assessing Step-Free Accessibility Disparities on the London Underground

This repository hosts the complete code for my Masters Dissertation, entitled "An Equity-Focused Approach to Prioritise Step-Free Upgrades on the London Underground". Below is an explanation of each script, alongside extra information for reproducibility purposes.

## Scripts
Although each script is intended to be run after the other, 1 and 2 can be skipped if the user manually imports the resultant GTFS files from the OneDrive link provided below.

1. **rail_download.R**: downloads and processes Overground and Elizabeth Line data from the National Rail API.
2. **schedule_processing.R**: processes Traveline data for TfL services and joins this with TfL accessibility data. Two GTFS files are produced: a general GTFS object for use in r5r (gtfs.zip), and an accessible version (gtfs_accessible.zip) for accurate wheelchair routing in OpenTripPlanner.
3. **lsoa_processing.R**: delineates the study area and processes socio-demographic data at LSOA level.
4. **maps_summary_stats.R**: produces maps of the study area and summary statistics about the present level of step-free access.
5. **time_analysis.R**: uses r5r to assess the disparity in travel time from each LSOA centroid to its nearest station compared to its nearest accessible station.
6. **cumulative_opportunities.R**: uses r5r to assess the disparity in job accessibility when using the standard versus wheelchair-accessible travel network.
7. **catchment_analysis.R**: clusters LSOAs based on disparities in job accessibility and time to stations, alongside presence of in-need populations. This is used to identify stations which should be prioritised in an egalitarian-aligned scenario.
8. **network_analysis.R**: represents the step-free tube network as a graph and simulates adding non-step-free stations. Efficiency and centrality measures are calculated, thereby identifying stations to be prioritised in a utilitarian-aligned scenario.
9. **scenario_assessment.R**: compares the egalitarian and utilitarian scenarios with the stations TfL is considering for step-free upgrades.

There are also two standalone scripts:
* **gtfs_to_igraph.R**: this is a (slightly) modified version of a function written by [Rafael Pereira](https://github.com/rafapereirabr/gtfs_to_igraph) which converts GTFS files into an iGraph object, used in file 8.
* **otp_example.R**: demonstrates how to run accurate wheelchair-accessible routing in OpenTripPlanner, using the accessible GTFS file produced in script 2. It had been hoped to use this in the final dissertation, but OpenTripPlanner was unable to cope with multi-route processing. This script is nonetheless included as proof of concept.

## Data
Although most data has been uploaded to the `data` folder, some files were too large to be uploaded. First, users will need to download an OA shapefile from the [ONS Open Geography Portal](https://geoportal.statistics.gov.uk/datasets/31dac98df61a4312991646842b147e2f_0/explore?location=52.548768%2C-2.489483%2C6.49), saving it to a folder entitled `large_data`.

Other key files can be downloaded from [this OneDrive link](https://liveuclac-my.sharepoint.com/:f:/g/personal/ucfnjeb_ucl_ac_uk/EmEPh94EOcJPiiv7vajfxsABdRxn-hJzFM0GCr0hK7Ahng?e=ij1K4f). These are:

1. **national_rail_atoc.zip**: the GTFS file for all National Rail services, downloaded on 29/05/2025. This should be saved to the folder `large_data`. Otherwise, this can be downloaded at the start of script 1. 
2. **london_traveline.zip**: the GTFS file for all non-rail London public transport services, downloaded from the [Traveline](https://www.travelinedata.org.uk/traveline-open-data/traveline-national-dataset/) website on 09/05/2025. This should be saved into `large_data`. While this can be downloaded again from Traveline, I recommend using the version I used, as a lot of the issues I had to correct (e.g. problematic IDs) are likely version-specific.
3. **gtfs.zip**: the final GTFS file for the whole of London, produced in script 2. This should be saved to a folder entitled `final_r5r`.
4. **gtfs_accessible.zip**: an OpenTripPlanner-compatible wheelchair-accessible GTFS file, produced in script 2. Although this was not used in the final dissertation, please see the otp_example.R script for a demonstration of its use.
5. **planet_-0.972,51.228_0.3339,51.7629.osm.pbf**: the OSM street network, downloaded from bbike on 06/06/2025. It should be saved to the `final_r5r` folder.

Please note that the National Rail and Traveline data are free to share and adapt under the [Open Government License](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/), while the OSM street network is free to share under the [Open Database License](https://www.openstreetmap.org/copyright). 

## Extra Reproducibility Information
* The R Java environment should be installed following the [r5r package instructions](https://ipeagit.github.io/r5r/articles/r5r.html).
* However, please note that r5r and OpenTripPlanner use different versions of Java (21 and 17, respectively). To circumvent this problem, I added paths to both Java versions in my **.Renviron** file as follows. The otp_example.R file shows how to switch between these.
```
JAVA_HOME=[path to Java 21]
JAVA_HOME17=[path to Java 17]
```
* To download data from the National Rail or TfL Unified API, it is necessary to first register for these services online, and then add this registration information to **.Renviron**. These should be saved:
```
tfl_api_key=[api key]
national_rail_username=[username]
national_rail_password=[password]
```
* If using OpenTripPlanner for wheelchair-accessible routing, the OSM.pbf file only works when certain tags are removed. To do this, first download [Osmium](https://osmcode.org/osmium-tool/), then filter the file with this command:
```
osmium tags-filter planet_-0.972,51.228_0.3339,51.7629.osm.pbf w/highway wa/public_transport=platform wa/railway=platform w/park_ride=yes r/type=restriction r/type=route -o filtered.osm.pbf -f pbf,add_metadata=false
```
