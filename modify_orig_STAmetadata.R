#Modify Metadata for use in STA coding
  
#add: tag_type, 

#keep: band_number, animal_id, tag_id, species, *deploy_year*, deploy_site, lat_deploc, long_deploc, incl_deploy_loc, 
  #incl_recovery_loc, lat_colony, lon_colony, lat_end, lon_end, datetime_deploy_UTC, datetime_recover_UTC, 
  #collab1_point_contact_name, collab1_point_contact_email, collab1_organization, collab2_point_contact_name, 
  #collab2_point_contact_email, collab2_organization, file_name *age*, *sex*, notes,datetime_start_track_UTC, datatime_end_track_UTC,

#ask/modify - decide to keep or remove:  observer

#rename: ptt_tagid to STA_id

#modify, then remove: loc_data

#remove: UnfilteredDataInCompiledFile, FilteredDataInCompiledFile, grouping_var_1, dir1, dir2, 
  #tag_sensors, Mass_kg, Length_cm, Girth_cm, transmission duration, feather sample, blood sample, 
  #chick foraging_datetime_start_UTC, Duration, nb_locs, Species_num, Status_numb, Sex_num, Duty_cycle, Best_track, 
  #Repeat, Complete_track, Loc_num, Track_ID

library(dplyr)
# Set main dir: Sys.info()[7] is the username for the computer.  fill in the "" with your user name 
if(Sys.info()[7]=="rachaelorben") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/"}
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

if(Sys.info()[7]=="cherylhorton") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CCESTA/"}
if(Sys.info()[7]=="cherylhorton") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

setwd(dir <- "C:/Users/cahorton/Documents/TelemetryAtlas/supporttables")

#This is the original metadata file with extraneous columns included
PTT_metadata_mod <- read.csv(paste0(dir,'/supporttables/PTT_metadata.csv'))
#PTT_metadata_mod <- as_tibble(PTT_metadata_mod)
head(PTT_metadata_mod)
str(PTT_metadata_mod)

# #keep rows coded "1" in colomn "loc_data" - get rid of all rows coded "0" in loc_data
# RAO Sept 5, 2019: I think we should keep the loc_data==0 since it acts as an index of potential datasets. 
#     (also I have some of those tracks nows)
# PTT_metadata_mod_1 <- subset(PTT_metadata_mod, loc_data == 1)
# head(PTT_metadata_mod_1)
# str(PTT_metadata_mod_1)
# names(PTT_metadata_mod_1)

#create a dataframe of only the columns we want in the metadata for coding
meta <- PTT_metadata_mod%>%dplyr::select(ptt_deploy_id,band_no, animal_id, tag_id, 
                                         species, age, sex,year,tag_sensors,
                                         loc_data,file_name,deploy_site, site_abbrev,
                                         lat_deploc, lon_deploc, incl_deploy_loc, incl_recovery_loc, 
                                         lat_colony, lon_colony, 
                                         lat_end, lon_end, 
                                         datetime_deploy_UTC, datetime_recover_UTC, 
                                         datetime_start_track_UTC, datatime_end_track_UTC,
                                         collab1_point_contact_name, collab1_point_contact_email, collab1_organization, 
                                         collab2_point_contact_name, collab2_point_contact_email, collab1_organization.1,
                                         Notes)
head(meta)
names(meta)

#rename mislabeled columns in this new dataframe
meta <- rename(meta, deploy_year = year)
meta <- rename(meta, collab2_organization = collab1_organization.1)
meta<- renane(meta, STA_id <- ptt_deploy_id)
names(meta)

#Add a column for location type and fill in with PTT for all existing data in this dataframe
meta$location_type="PTT"

#Adds CentralPlaceForager for dividing trips if needed, Y given to animals with a colony location. 
meta$CentralPlaceForage_YN<-"N"
meta$CentralPlaceForage_YN[is.na(meta$lat_colony)==FALSE]<-"Y"
head(meta)

# #Add a column that concatinates various columns to create a unique ID
# meta$STA_unqID <- paste(meta$band_no, meta$animal_id, 
#                         meta$tag_id, meta$species, meta$deploy_year, sep = "_")
# names(meta)
# head(meta)

dt<-Sys.Date()
#save STA_rcode_metadata as a .rda, adds date to file name so most recent verions is obvious
save(meta, file = paste0("STA_metadata_",dt,".rda"))

#export new STA_rcode_metadata as a .csv
write.csv(meta, file = paste0("STA_metadata_",dt,".csv"))
