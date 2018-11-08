# seabird_tracking_atlas

# *****************************
## Codes for the California Current Telemetry Atlas
## Original Codes written by Bill Henry 
## Adapted into functions & scripts by Rachael Orben 
# *****************************

Scripts are meant as templates for polygon-species-group combinations.

STA_1_TrackPrepFilter.R
This script preps the tracking data and uses a speed/angle filter to cull erronious locations

STA_2_BrownianBridges.R
This script 
  1. labels the tracking data as in or out of the buffered polygon 
  2. segments the tracking data based on a time threshold
  3. combines segments from each indivdual (within a group - year, season, month)
  4. combines individuals into groups (alltime, year, season, month)
  5. plots the density and tracks for each group 

MakeClippers.R
This script takes shapefiles, reprojects them, makes polygons, and associated grid for kernel density estimates. 
  (add habitat - onland vs. at sea here)

EEZtoPoly.R 
  Doesn't exist yet. Will be used to take EEZs and turn them into polygons within R. 
  
STA_Functions.R
  Where all the actual work happens. 

##
/supporttables  
  a couple of .csv or excel files with metadata for track filtering - these need some cleaning. 
