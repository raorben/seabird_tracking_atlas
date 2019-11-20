library(dplyr)


if(Sys.info()[7]=="rachaelorben") {dir<-"/Users/rachaelorben/Research/SeabirdTrackingAtlas/"
gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"
datadir<-"/Users/rachaelorben/Research/SeabirdTrackingAtlas/"}


meta <- readRDS(file = paste0(gitdir,"/supporttables/STA_metadata_2019-11-18_804birds.rda"))


# STAL --------------------------------------------------------------------
stal<-read.csv("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Albs_STAL_OConnor_pubs/Post_fledging_tracking/data/STALfledglings_all_MTIandSPDfiltered_excluding_deadbirds_and_postfledging_drift_lessthan20kmhr_0.5km_interp_24hr_gaplimit_1hr_timesamp_forR.csv")
names(stal)

stal%>%group_by(pttid)%>%summarise(n=n())
unique(stal$name)

stal%>%filter(name==3309)


# BRAC --------------------------------------------------------------------
data<-readRDS("/Users/rachaelorben/Box/DASHCAMS/Analysis/data_processed/Locations_speedtrimed_2019-10-22.rda")
brac.info<-meta%>%filter(species=="BRAC")
names(data)

data$device_id[data$device_id==14869001733569]<-"1733569"
data$device_id[data$device_id==14869001744392]<-"1744392"

ids=unique(data$device_id)
for (i in 1:nrow(brac.info)){
  d<-data%>%dplyr::filter(device_id==ids[i])
  info<-brac.info%>%dplyr::filter(tag_id==ids[i])
  info$file_name
  
  d <- rename(d, utc = UTC_datetime)
  d <- rename(d, lat1= Latitude)
  d <- rename(d, lon1 = Longitude)
  
  write.csv(d,paste0(datadir,"species/BRAC/1_DataIn/",info$file_name,".csv"))
}

# WEGU --------------------------------------------------------------------
data<-readRDS("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/WEGU/data/AllBirdLocations15161718_WEGU_trimfilt.rda")
data$TagType[data$TagType=="Ornitella"]<-"Ornitela"
wegu.info<-meta%>%filter(species=="WEGU")
wegu.info$file_name

unique(wegu.info$file_name)

names(data)
ids=unique(wegu.info$file_name)
for (i in 1:nrow(wegu.info)){
  info<-wegu.info%>%dplyr::filter(file_name==ids[i])
  
  d<-data%>%dplyr::filter(TagType==info$Tag_brand)%>%
    filter(Band==info$band_no)%>%
    filter(DeployYr==year(info$datetime_deploy_UTC))
  
  d <- rename(d, utc = DateTime)
  d <- rename(d, lat1= lat)
  d <- rename(d, lon1 = lon)
  d <- rename(d, device_id = TagID)
  d$UniID_gap<-NA
  
  write.csv(d,paste0(datadir,"species/WEGU/1_DataIn/",info$file_name,".csv"))
}


