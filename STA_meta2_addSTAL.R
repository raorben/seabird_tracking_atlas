library(dplyr)

# clear all
rm(list=ls())

# Set main dir: Sys.info()[7] is the username for the computer.  fill in the "" with your user name 
if(Sys.info()[7]=="rachaelorben") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/"}
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

if(Sys.info()[7]=="cherylhorton") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CCESTA/"}
if(Sys.info()[7]=="cherylhorton") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

meta<-readRDS(file = paste0(gitdir,"/supporttables/STA_metadata_2019-09-05.rda"))
str(meta)
stal <- read.csv(paste0(dir,'/supporttables/PTT_metadata_STAL.csv'),
                             na.strings=c("NA","NaN", " ",""),# places a NA in all cells with all these cases
                             stringsAsFactors=FALSE)#gets rid of factors  

stal$animal_id<-as.character(stal$animal_id)
stal$file_name<-as.character(stal$file_name)

stal$STA_id<-(max(meta$STA_id, na.rm=TRUE)+1):(max(meta$STA_id, na.rm=TRUE)+nrow(stal))
stal$CPF_YN<-"N"
stal$location_type<-"GPS"

stal$deploy_year<-year(mdy_hm(stal$datetime_deploy_UTC))
stal$tag_sensors<-"none"

stal$datetime_deploy_UTC<-mdy_hm(stal$datetime_deploy_UTC)

meta<-bind_rows(meta,stal%>%dplyr::select(-year,-observer))
names(meta)

dt<-Sys.Date()

#save STA_rcode_metadata as a .rda, adds date to file name so most recent verions is obvious
#adds this to Github to accessable for all. This might get a bit cumbersome, but I am not sure of other options 
saveRDS(meta, file = paste0(gitdir,"supporttables/STA_metadata_",dt,"_",nrow(meta),"birds.rda"))

#export new STA_rcode_metadata as a .csv
#keeps this file local for now
write.csv(meta, file = paste0(dir,"supporttables/STA_metadata_",dt,"_",nrow(meta),"birds.csv"),row.names = FALSE)


