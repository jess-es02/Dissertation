# Dissertation

**To add for reproducibility:**
- Setting up R Java environment, as in r5r instructions
- Setting Java Home in .Renviron
- What to do with Traveline data? It's technically under the Government ODL so could probably be uploaded here (check) - but might be too large to do so anyway
- Traveline data was downloaded 09/05/2025
- Data excluded from gitignore - user would need to add:
  - [OA shapefile](https://geoportal.statistics.gov.uk/datasets/31dac98df61a4312991646842b147e2f_0/explore?location=52.693294%2C-2.489483%2C6.49) - to add to data folder
  - Traveline London zip file (london_traveline.zip), alongside converted GTFS zip file (gtfs_london.zip), saved in large_data folder
  - Also needs to run the osmextract line to download the London road network to the large_data folder
  - User would need their own TfL Unified API key
