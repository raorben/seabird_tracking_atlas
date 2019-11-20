library(readxl)
library(dplyr)
library(lubridate)
library(stringr)

# clear all
rm(list=ls())


# Set main dir: Sys.info()[7] is the username for the computer.  fill in the "" with your user name 
if(Sys.info()[7]=="rachaelorben") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/"}
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

if(Sys.info()[7]=="cherylhorton") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CCESTA/"}
if(Sys.info()[7]=="cherylhorton") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

meta<-readRDS(file = paste0(gitdir,"supporttables/STA_metadata_2019-11-18_751birds.rda"))

palo.df<-read.csv("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/PALO/Pacific Loons Alaska Colville River Delta.csv",
               na.strings=c("NA","NaN", " ",""),# places a NA in all cells with all these cases
               stringsAsFactors=FALSE)#gets rid of factors  

palo<-read.csv("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/PALO/Pacific Loons Alaska Colville River Delta-reference-data.csv",
               na.strings=c("NA","NaN", " ",""),# places a NA in all cells with all these cases
               stringsAsFactors=FALSE)#gets rid of factors  

palo.df.st<-palo.df%>%group_by(tag.local.identifier)%>%summarise(tmin=min(timestamp))
  
names(palo)
names(meta)

#puts in unique deployment ID for Seabird Telemetry Atlas
palo$STA_id<-(max(meta$STA_id, na.rm=TRUE)+1):(max(meta$STA_id, na.rm=TRUE)+nrow(palo))

palo <- rename(palo, tag_id = tag.id)
palo <- rename(palo, animal_id = animal.id)
palo$species<-"PALO"
palo <- rename(palo, band_no = animal.ring.id)
palo <- rename(palo, age = animal.life.stage)
palo <- rename(palo, Duty_cycle = duty.cycle)
palo <- rename(palo, deploy_site = study.site)
palo <- rename(palo, lat_deploc = deploy.on.latitude)
palo <- rename(palo, lon_deploc = deploy.on.longitude)
palo <- rename(palo, datetime_end_track_UTC=deploy.off.date)
palo <- rename(palo, Tag_brand=tag.manufacturer.name)

palo$datetime_deploy_UTC<-ymd_hms(palo$deploy.on.date)
palo$location_type<-"Argos"
palo$tag_sensors<-"y"
palo$loc_data<-1
palo$incl_deploy_loc<-1

palo$site_abbrev[palo$deploy_site=="Yukon-Kuskokwim Delta"]<-"YKDelta"
palo$site_abbrev[palo$deploy_site=="ConocoPhillips Alpine Facility"]<-"CPAF"

palo$CPF_YN<-"N"
palo$collab1_point_contact_name<-"Joel Schmutz"
palo$collab1_point_contact_email<-"jschmutz@usgs.gov"
palo$collab1_organization<-"USGS"
palo$collab2_point_contact_name<-"Autumn-Lynn Harrison"
palo$collab2_point_contact_email<-"HarrisonAL@si.edu"
palo$collab2_organization<-"Smithsonian Conservation Biology Institute"

palo<-palo%>%dplyr::select(-tag.readout.method,-animal.mass,-animal.reproductive.condition,
                           -attachment.type,-deploy.on.person,
                           -tag.model,-tag.production.date,-animal.taxon,-deploy.on.date)

palo$tag_id<-as.character(palo$tag_id)
palo$animal_id<-as.character(palo$animal_id)

#adds in a start time from the first location
for (i in 1:nrow(palo)){
  ID<-palo$tag_id[i]
  ST<-palo.df.st%>%filter(tag.local.identifier==ID)
  if(is.na(palo$datetime_deploy_UTC[i])==TRUE)(
    palo$datetime_deploy_UTC[i]<-ST$tmin
  )
}
palo$deploy_year<-year(palo$datetime_deploy_UTC)
palo$file_name<-paste0(palo$species,"_",palo$deploy_year,"_",palo$site_abbrev,"_",palo$animal_id)

meta<-bind_rows(meta,palo)



# RTLO --------------------------------------------------------------------

rtlo.df<-read.csv("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/RTLO/RedThroatedLoon_telemetry_ak_2000_2011/redThroatedLoon_telemetry_ak_uherKoch_2000_2011.csv",
                  na.strings=c("NA","NaN", " ",""),# places a NA in all cells with all these cases
                  stringsAsFactors=FALSE)#gets rid of factors  
names(rtlo.df)

rtlo.df$datetime<-ymd_hms(paste0(rtlo.df$date," ",rtlo.df$hour,":",rtlo.df$minute,":",rtlo.df$second))
rtlo<-rtlo.df%>%group_by(animal,ptt,herd)%>%
  summarise(datetime_deploy_UTC=min(datetime))

#puts in unique deployment ID for Seabird Telemetry Atlas
rtlo$STA_id<-(max(meta$STA_id, na.rm=TRUE)+1):(max(meta$STA_id, na.rm=TRUE)+nrow(rtlo))

rtlo$deploy_year<-year(rtlo$datetime_deploy_UTC)
rtlo$datetime_start_track_UTC<-rtlo$datetime_deploy_UTC
rtlo$species<-"RTLO"
rtlo$location_type<-"Argos"
rtlo$tag_sensors<-"none"
rtlo$loc_data<-1
rtlo$incl_deploy_loc<-1
rtlo$CPF_YN<-"N"
rtlo$collab1_point_contact_name<-"Joel Schmutz"
rtlo$collab1_point_contact_email<-"jschmutz@usgs.gov"
rtlo$collab1_organization<-"USGS"
rtlo$collab2_point_contact_name<-"Brian Uher-Koch"
rtlo$collab2_point_contact_email<-"buher-koch@usgs.gov"
rtlo$collab2_organization<-"USGS"

rtlo <- rename(rtlo, site_abbrev = herd)
rtlo <- rename(rtlo, animal_id = animal)
rtlo <- rename(rtlo, tag_id = ptt)

rtlo$deploy_site<-NA
rtlo$deploy_site[rtlo$site_abbrev=="ACP"]<-"Arctic Coastal Plain"
rtlo$deploy_site[rtlo$site_abbrev=="SP"]<-"Seward Peninsula"
rtlo$deploy_site[rtlo$site_abbrev=="YKD"]<-"Yukon-Kuskokwim Delta"
rtlo$deploy_site[rtlo$site_abbrev=="CRD"]<-"Copper River Delta"

rtlo$file_name<-paste0(rtlo$species,"_",rtlo$deploy_year,"_",rtlo$site_abbrev,"_",rtlo$tag_id)
rtlo$tag_id<-as.character(rtlo$tag_id)

names(rtlo)

unique(meta$tag_sensors)

meta<-meta%>%filter(species!="RTLO")
meta<-bind_rows(meta,rtlo)


dt<-Sys.Date()

#save STA_rcode_metadata as a .rda, adds date to file name so most recent verions is obvious
#adds this to Github to accessable for all. This might get a bit cumbersome, but I am not sure of other options 
saveRDS(meta, file = paste0(gitdir,"supporttables/STA_metadata_",dt,"_",nrow(meta),"birds.rda"))

#export new STA_rcode_metadata as a .csv
#keeps this file local for now
write.csv(meta, file = paste0(dir,"supporttables/STA_metadata_",dt,"_",nrow(meta),"birds.csv"),row.names = FALSE)
