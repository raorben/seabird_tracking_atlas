library(readxl)
library(dplyr)
library(lubridate)

# clear all
rm(list=ls())


# Set main dir: Sys.info()[7] is the username for the computer.  fill in the "" with your user name 
if(Sys.info()[7]=="rachaelorben") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/"}
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

if(Sys.info()[7]=="cherylhorton") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CCESTA/"}
if(Sys.info()[7]=="cherylhorton") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

meta<-readRDS(file = paste0(gitdir,"supporttables/STA_metadata_2019-09-05_781birds.rda"))

nofu<-read.csv("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/NOFU/NOFU_Hatch_meta.csv",
              na.strings=c("NA","NaN", " ",""),# places a NA in all cells with all these cases
              stringsAsFactors=FALSE)#gets rid of factors  
#puts in unique deployment ID for Seabird Telemetry Atlas
nofu$STA_id<-(max(meta$STA_id, na.rm=TRUE)+1):(max(meta$STA_id, na.rm=TRUE)+nrow(nofu))

nofu$tag_id<-as.character(nofu$tag_id)
nofu$loc_data<-1
str(nofu$datetime_deploy_UTC)
nofu$datetime_deploy_UTC<-ymd_hm(nofu$datetime_deploy_UTC, tz="UTC")
str(meta$datetime_deploy_UTC)
meta<-bind_rows(meta,nofu)


dt<-Sys.Date()

#save STA_rcode_metadata as a .rda, adds date to file name so most recent verions is obvious
#adds this to Github to accessable for all. This might get a bit cumbersome, but I am not sure of other options 
saveRDS(meta, file = paste0(gitdir,"supporttables/STA_metadata_",dt,"_",nrow(meta),"birds.rda"))

#export new STA_rcode_metadata as a .csv
#keeps this file local for now
write.csv(meta, file = paste0(dir,"supporttables/STA_metadata_",dt,"_",nrow(meta),"birds.csv"),row.names = FALSE)
