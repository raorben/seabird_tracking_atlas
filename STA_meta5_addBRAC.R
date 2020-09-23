library(readxl)
library(dplyr)
library(lubridate)

# clear all
rm(list=ls())


# Set main dir: Sys.info()[7] is the username for the computer.  fill in the "" with your user name 
if(Sys.info()[7]=="rachaelorben") {dir<-"/Users/rachaelorben/Research/SeabirdTrackingAtlas/"}
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

if(Sys.info()[7]=="cherylhorton") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CCESTA/"}
if(Sys.info()[7]=="cherylhorton") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

meta<-readRDS(file = paste0(gitdir,"supporttables/STA_metadata_2019-11-23_785birds.rda"))

brac<-read.csv("/Users/rachaelorben/Box/DASHCAMS/data/Field Data/2019_DASHCAMS_Deployment_Field_Data.csv",
               na.strings=c("NA","NaN", " ","","na"),# places a NA in all cells with all these cases
               stringsAsFactors=FALSE)#gets rid of factors  

head(brac)

brac<-brac%>%filter(is.na(Tag_ID)==FALSE)%>%
  filter(Tag_Company!="CATS")

#puts in unique deployment ID for Seabird Telemetry Atlas
brac$STA_id<-(max(meta$STA_id, na.rm=TRUE)+1):(max(meta$STA_id, na.rm=TRUE)+nrow(brac))

brac <- rename(brac, tag_id = Tag_ID)
brac <- rename(brac, band_no = USGS_Band)
brac <- rename(brac, species = Species)
brac <- rename(brac, Tag_brand = Tag_Company)
brac$lat_deploc <- 46.2491667
brac$lon_deploc <- 124.5
brac$loc_data<-1
brac$CPF_YN<-"N"
brac$location_type<-"GPS"
brac$tag_sensors<-"Y"
brac$band_no<-gsub(brac$band_no,pattern = "-",replacement = "")

brac$datetime_deploy_UTC<-mdy_hm(paste0(brac$Capture_Date, " ",brac$Capture_Time), tz="UTC")
brac$deploy_year<-year(brac$datetime_deploy_UTC)
brac$datetime_start_track_UTC<-brac$datetime_deploy_UTC
brac$datetime_end_track_UTC<-mdy_hm(brac$Deployment_End)
brac$collab1_point_contact_name<-"Rachael Orben"
brac$collab1_point_contact_email<-"raorben@gmail.com"

brac$animal_id<-paste0(brac$tag_id,"_",brac$band_no)
brac$file_name<-paste0(brac$animal_id,"_",brac$deploy_year)
brac$deploy_site<-"Columbia River"
brac$site_abbrev<-"MCR"
brac$incl_deploy_loc<-0
brac$CPF_YN<-"N"
names(meta)
names(brac)
brac<-brac%>%dplyr::select(STA_id,band_no, animal_id, tag_id, species, CPF_YN, deploy_year,
                           location_type,tag_sensors, loc_data, file_name,deploy_site,site_abbrev,
                           lat_deploc,lon_deploc,incl_deploy_loc,datetime_deploy_UTC,datetime_start_track_UTC,
                           datetime_end_track_UTC,
                           collab1_point_contact_name,
                           collab1_point_contact_email, Notes,Tag_brand)


str(brac$datetime_deploy_UTC)


meta<-bind_rows(meta,brac)


dt<-Sys.Date()

#save STA_rcode_metadata as a .rda, adds date to file name so most recent verions is obvious
#adds this to Github to accessable for all. This might get a bit cumbersome, but I am not sure of other options 
saveRDS(meta, file = paste0(gitdir,"supporttables/STA_metadata_",dt,"_",nrow(meta),"birds.rda"))

#export new STA_rcode_metadata as a .csv
#keeps this file local for now
write.csv(meta, file = paste0(dir,"supporttables/STA_metadata_",dt,"_",nrow(meta),"birds.csv"),row.names = FALSE)
