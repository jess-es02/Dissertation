# Dissertation

**To add for reproducibility:**
- Setting up R Java environment, as in r5r instructions
- Setting Java Home in .Renviron (JAVA_HOME for r5r, JAVA_HOME17 for OTP)
- What to do with Traveline data? It's technically under the Government ODL so could probably be uploaded here (check) - but might be too large to do so anyway
  - If so, should probably upload Traveline (e.g. DropBox) because of all the specific things I have had to sort, e.g. problematic IDs - these are probably version-specific
- Traveline data was downloaded 09/05/2025, National Rail 29/05/2025
- Data excluded from gitignore - user would need to add:
  - [OA shapefile](https://geoportal.statistics.gov.uk/datasets/31dac98df61a4312991646842b147e2f_0/explore?location=52.693294%2C-2.489483%2C6.49) - to add to large_data folder
  - Traveline London zip file (london_traveline.zip), alongside converted GTFS zip file (gtfs_london.zip), saved in large_data folder
  - User would need their own TfL Unified API key, saved in .Renviron
  - OSM road network, downloaded from bbike. Coords -0.972, 51.228 x 0.3339, 51.7629
    - I originally tried using osmextract (more reproducible) but there were problems with some of the tags, stopping setup_r5 from completing successfully
    - Note that I downloaded this data on 06/06/25
    - Should I upload this data myself? e.g. DropBox
    - For OTP routing, it needs to be filtered using osmium
      - User can download this themself (but I will probably have to upload it)
      - Using the recommended command from the OSM website: `osmium tags-filter planet_-0.972,51.228_0.3339,51.7629.osm.pbf w/highway wa/public_transport=platform wa/railway=platform w/park_ride=yes r/type=restriction r/type=route -o filtered.osm.pbf -f pbf,add_metadata=false`
- To get the Overground/Lizzie line data, user will need to register for the National Rail API and point to these in .Renviron