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

meta<-readRDS(file = paste0(gitdir,"supporttables/STA_metadata_2019-09-05_652birds.rda"))

#TABLES needed to run SDAFreitas_CCESTA filter function
sol<-read.csv(paste0(dir,"supporttables/SOL_CaptureData_v2.csv"),
                  na.strings=c("NA","NaN", " ",""),# places a NA in all cells with all these cases
                  stringsAsFactors=FALSE)#gets rid of factors  

#brings in wegu data paired with recapture or tag failure dates - needs updating still
wegu_end<-read_excel("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/WEGU/data/DeployMatrix_WEGU_EndTImes_Ornitella.xlsx",
                 na = "NA")
wegu_end<-wegu_end%>%filter(Tag_brand!="igotu" & Tag_brand!="MrLee")

sol$Tag_brand[sol$Tag_brand=="none"]<-NA

#reduces sol data to just PTT & GPS deployment birds
solmeta<-sol%>%filter(Species=="COMU" | Species=="WEGU" | Species=="HYGU")
solmeta<-solmeta%>%filter(is.na(Tag_brand)==FALSE | is.na(Tag_model)==FALSE)%>%#removes birds without loggers
        filter(VHF==FALSE)%>%      #removes VHF birds
        filter(Deploy_Loc!="CRP") #removes COMU deployed by JA at mouth of Columbia (already in STA)

#fixes times
solmeta$DateTime_local.f<-mdy_hm(solmeta$DateTime_local, tz="America/Los_Angeles")
solmeta$datetime_deploy_UTC<-with_tz(solmeta$DateTime_local.f, tz="UTC")

#fixes WEGU without capture time in local
solmeta$datetime_GMT<-mdy_hm(paste0(solmeta$Deploy.Date_gmt," ",solmeta$Deploy.Hour_gmt,":",solmeta$Deploy.Min_gmt))
solmeta$datetime_deploy_UTC[11:21]<-solmeta$datetime_GMT[11:21]

#renames sol meta to meta columns
names(solmeta)
names(meta)
solmeta <- rename(solmeta, species = Species)
solmeta <- rename(solmeta, age = Age_Class)
solmeta <- rename(solmeta, animal_id = Animal_ID)
solmeta <- rename(solmeta, lat_deploc = Latitude)
solmeta <- rename(solmeta, lon_deploc = Longitude)
solmeta <- rename(solmeta, sex = Sex)
solmeta <- rename(solmeta, tag_id = Tag_ID)
solmeta <- rename(solmeta, argos_id = Argos_ID)
solmeta <- rename(solmeta, deploy_site = Deploy_Loc)

#adds location type
solmeta$location_type<-NA
solmeta$location_type[solmeta$species=="COMU"]<-"Argos"
solmeta$location_type[solmeta$species=="COMU" & solmeta$Tag_brand=="University of Amsterdam"]<-"GPS"
solmeta$location_type[solmeta$species=="WEGU"]<-"GPS"
solmeta$location_type[solmeta$species=="WEGU" & solmeta$Tag_brand=="WildlifeComputers"]<-"Argos"
  
solmeta$deploy_year<-year(solmeta$datetime_deploy_UTC)
solmeta$band_no<-solmeta$animal_id
solmeta$collab1_point_contact_name<-"Rachael Orben"
solmeta$collab1_point_contact_email<-"Rachael.orben@oregonstate.edu"
solmeta$collab1_organization<-"Oregon State University"

#adds central place forager Y or N
solmeta$CPF_YN<-NA
solmeta$CPF_YN[solmeta$species=="COMU"]<-"N"
solmeta$CPF_YN[solmeta$species=="WEGU"]<-"Y"

solmeta$argos_id[solmeta$location_type=="GPS"]<-NA

solmeta$lat_colony<-NA
solmeta$lon_colony<-NA

solmeta$deploy_site[solmeta$deploy_site=="HUNTER"]<-"Hunters"
solmeta$deploy_site[solmeta$deploy_site=="CLEF"]<-"Cleft"
solmeta$deploy_site[solmeta$deploy_site=="South Beach, Marina Lot"]<-"South Jetty"
solmeta$site_abbrev<-solmeta$deploy_site

solmeta$lat_colony[solmeta$deploy_site=="Cleft"]<-44.292674
  solmeta$lon_colony[solmeta$deploy_site=="Cleft"]<-(-124.112719)
solmeta$lat_colony[solmeta$deploy_site=="Hunters"]<-42.314232
  solmeta$lon_colony[solmeta$deploy_site=="Hunters"]<-(-124.425725)
solmeta$lat_colony[solmeta$deploy_site=="South Jetty"]<-44.419042
  solmeta$lon_colony[solmeta$deploy_site=="South Jetty"]<-(-124.082793)
unique(solmeta$deploy_site)

solmeta$Tag_brand[solmeta$Tag_brand=="IGOTU"]<-"igotu"
solmeta$Tag_brand[solmeta$Tag_brand=="Ornitella"]<-"Ornitela"
solmeta$Tag_brand[solmeta$Tag_brand=="Mr.Lee"]<-"MrLee"
solmeta$Tag_brand[solmeta$Tag_brand=="Mr. Lee"]<-"MrLee"

wegu<-solmeta%>%filter(species=="WEGU")

ids<-unique(wegu$band_no)

newinfo<-data.frame()
for (i in 1:length(ids)){
  info<-wegu%>%filter(band_no==ids[i])
  print(nrow(info))
  if(nrow(info)==1){
    info$datetime_recover_UTC<-NA
    if(info$Tag_brand=="CATS"){newinfo<-rbind(newinfo,info)}
    if(info$Tag_brand=="Ornitela"){a<-wegu_end%>%
                                      filter(Tag_ID==info$tag_id)%>%
                                      filter(ID==info$animal_id)%>%
                                      dplyr::select(Deployment_End_byhand_GMT)
    info$datetime_recover_UTC<-a$Deployment_End_byhand_GMT
      newinfo<-rbind(newinfo,info)}
    if(info$Tag_brand=="WildlifeComputers"){newinfo<-rbind(newinfo,info)}
  }
  
  if(nrow(info)==2){
    info$datetime_recover_UTC<-info$datetime_deploy_UTC[2]
    newinfo<-rbind(newinfo,info[1,])
  }
  if(nrow(info)==3){
    info<-info%>%filter(Tag_brand!="CATS")
    print(info)
    info$datetime_recover_UTC<-info$datetime_deploy_UTC[2]
    newinfo<-rbind(newinfo,info[1,])
  }
}
newinfo$year<-year(newinfo$datetime_deploy_UTC)
newinfo%>%group_by(year,deploy_site)%>%summarise(n=n())
newinfo$year<-year(newinfo$datetime_deploy_UTC)
newinfo$loc_data<-1

newinfo$file_name<-paste0(newinfo$band_no,"_",newinfo$Tag_brand,"_",newinfo$year)
solmeta$datetime_end_track_UTC<-solmeta$datetime_recover_UTC

#identifies which WEGU were not recapture, no loc data, not location data with UVA tags
solmeta$loc_data<-1
solmeta$loc_data[solmeta$Tag_brand=="University of Amsterdam"]<-0
solmeta$loc_data[solmeta$argos_id=="160661"]<-0

solmeta$tag_sensors[solmeta$Tag_brand=="TELONICS"]<-"Argos wetcounter"

#adds in file names for the 2015 COMU
comu<-solmeta%>%filter(species!="WEGU")

comu$file_name[comu$deploy_year==2015 | comu$deploy_site=="Yaquina Head"]<-comu$argos_id[comu$deploy_year==2015 | comu$deploy_site=="Yaquina Head"]
comu$file_name<-as.character(comu$file_name)

#COMUS
ids<-unique(comu$argos_id[comu$deploy_year==2016 | comu$deploy_year==2017])
ids<-ids[is.na(ids)==FALSE]
ids<-ids[ids>0]
for (i in ids){
  idx<-which(comu$argos_id==i)
  comu$file_name[idx]<-paste0(comu$argos_id[idx],"_",comu$deploy_year[idx])
}
comu$loc_data[comu$file_name=="160356_2017"]<-0

solmeta<-bind_rows(comu,newinfo%>%dplyr::select(-year))
solmeta$file_name



#remove unwanted columns
solmeta<-solmeta%>%dplyr::select(-OID,-Deploy.Date_gmt,-Deploy.Hour_gmt,-Deploy.Min_gmt,
                                 -DateTime_local,-GMT_offset_hrs,-Metal_Band_Leg,
                                 -Attachment_method,-Other,-Tag_mass,-Tag_model,
                                 -Color_Band,-Band_Code,-Lat_Deg,-Lat_Min,-Lon_Deg,
                                 -Lon_Min,-Blood_Gender,-Blood_Contaminants,-Blood_ISO,
                                 -Feathers,-Feather_Location,-Bacteria_Swabs,-Viral_Swabs,
                                 -Translocated,-How_Sexed,-Bandit_Age,-How_Aged,-Age_Years,
                                 -How_Captured,-Nest.,-Nest_Contents,-Comments_Capt,
                                 -PTT,-GPS,-VHF,-GSM,-DateTime_local.f,-datetime_GMT,
                                 -GPS_schedule)

#puts in unique deployment ID for Seabird Telemetry Atlas
solmeta$STA_id<-(max(meta$STA_id, na.rm=TRUE)+1):(max(meta$STA_id, na.rm=TRUE)+nrow(solmeta))

unique(solmeta$Tag_brand)

#put dataframes together
meta$tag_id<-as.character(meta$tag_id)
meta<-bind_rows(meta,solmeta)
names(meta)

dt<-Sys.Date()

#save STA_rcode_metadata as a .rda, adds date to file name so most recent verions is obvious
#adds this to Github to accessable for all. This might get a bit cumbersome, but I am not sure of other options 
saveRDS(meta, file = paste0(gitdir,"supporttables/STA_metadata_",dt,"_",nrow(meta),"birds.rda"))

#export new STA_rcode_metadata as a .csv
#keeps this file local for now
write.csv(meta, file = paste0(dir,"supporttables/STA_metadata_",dt,"_",nrow(meta),"birds.csv"),row.names = FALSE)
