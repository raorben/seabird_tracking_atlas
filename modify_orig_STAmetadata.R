#Modify Metadata for use in STA coding
  
#add: tag_type, unique ID (concatinate columns band_number, animal_id, tag_id - maybe also species and/or deploy_year)

#keep: band_number, animal_id, tag_id, species, *deploy_year*, deploy_site, lat_deploc, long_deploc, incl_deploy_loc, 
  #incl_recovery_loc, lat_colony, lon_colony, lat_end, lon_end, datetime_deploy_UTC, datetime_recover_UTC, 
  #collab1_point_contact_name, collab1_point_contact_email, collab1_organization, collab2_point_contact_name, 
  #collab2_point_contact_email, collab2_organization, file_name *age*, *sex*, notes

#ask/modify - decide to keep or remove: ptt_deploy_id, datetime_start_track_UTC, datatime_end_track_UTC, observer, ptt_tagid

#modify, then remove: loc_data

#remove: UnfilteredDataInCompiledFile, FilteredDataInCompiledFile, site_abbrev, grouping_var_1, dir1, dir2, 
  #tag_sensors, Mass_kg, Length_cm, Girth_cm, transmission duration, feather sample, blood sample, 
  #chick foraging_datetime_start_UTC, Duration, nb_locs, Species_num, Status_numb, Sex_num, Duty_cycle, Best_track, 
  #Repeat, Complete_track, Loc_num, Track_ID

library(dplyr)

setwd(dir <- "C:/Users/cahorton/Documents/TelemetryAtlas/supporttables")

#This is the original metadata file with extraneous columns included
PTT_metadata_mod <- read.csv(file = 'PTT_metadata_mod.csv')
#PTT_metadata_mod <- as_tibble(PTT_metadata_mod)
head(PTT_metadata_mod)
str(PTT_metadata_mod)

#keep rows coded "1" in colomn "loc_data" - get rid of all rows coded "0" in loc_data
PTT_metadata_mod_1 <- subset(PTT_metadata_mod, loc_data == 1)
head(PTT_metadata_mod_1)
str(PTT_metadata_mod_1)
names(PTT_metadata_mod_1)

#create a dataframe of only the columns we want in the metadata for coding
STA_rcode_metadata <- select(PTT_metadata_mod_1, "band_no", "animal_id", "tag_id", "species", "year",
                      "deploy_site", "lat_deploc", "lon_deploc", "incl_deploy_loc", "incl_recovery_loc", 
                      "lat_colony", "lon_colony", "lat_end", "lon_end", "datetime_deploy_UTC", "datetime_recover_UTC", 
                      "collab1_point_contact_name", "collab1_point_contact_email", "collab1_organization", 
                      "collab2_point_contact_name", "collab2_point_contact_email", "collab1_organization.1", "file_name",
                      "age", "sex", "Notes")
head(STA_rcode_metadata)
names(STA_rcode_metadata)

#rename mislabeled columns in this new dataframe
STA_rcode_metadata <- rename(STA_rcode_metadata, deploy_year = year)
STA_rcode_metadata <- rename(STA_rcode_metadata, collab2_organization = collab1_organization.1)
names(STA_rcode_metadata)

#Add a column for tag type and fill in with PTT for all existing data in this dataframe
STA_rcode_metadata$tag_type="PTT"
head(STA_rcode_metadata)

#Add a column that concatinates various columns to create a unique ID
STA_rcode_metadata$STA_unqID <- paste(STA_rcode_metadata$band_no, STA_rcode_metadata$animal_id, 
                                         STA_rcode_metadata$tag_id, STA_rcode_metadata$species, STA_rcode_metadata$deploy_year, sep = "_")
names(STA_rcode_metadata)
head(STA_rcode_metadata)

#save STA_rcode_metadata as a .rda
save(STA_rcode_metadata, file = "STA_rcode_metadata.rda")

#export new STA_rcode_metadata as a .csv
write.csv(STA_rcode_metadata, file = "STA_rcode_metadata.csv")
