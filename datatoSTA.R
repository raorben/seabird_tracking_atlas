#HodgePodge Script that pulls data from all formats
#splits files into animals
#updates metadata with file names

library(sp)
library(trip)
library(crawl)
library(lubridate)
library(dplyr)

# clear all
rm(list=ls())

if(Sys.info()[7]=="rachaelorben") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/"} ##RAO
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}



# Diag File Ingest --------------------------------------------------------
userdir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/data/raw_tracking/PTT/rawdiag_dispose/2017/Diag"
diagdat17 <- list.files(path = userdir,
                      pattern=".dia",full.names = TRUE)
userdir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/data/raw_tracking/PTT/rawdiag_dispose/2016/Diag"
diagdat16 <- list.files(path = userdir,
                        pattern=".dia",full.names = TRUE)

##reads in all diag files in a folder- super easy!!
data17<-readDiag(diagdat17)
data16<-readDiag(diagdat16) 
data<-rbind(data16,data17)

IDs<-unique(data$id) #lists argos IDS

Birds<-data.frame() #empty vector
for(i in 1:length(IDs)){  
  tr1<-subset(data,id==IDs[i]) #droplevels should help with leading 0s in Argos IDS
  tr1 <- tr1[!duplicated(tr1), ] #remove duplicates
  tr1 <- tr1[order(tr1$gmt), ]# order the rows by time
  Birds<-rbind(Birds,tr1)
} 

###create csv files ###
for (i in 1:length(IDs)){
  bird<-droplevels(subset(Birds,id==IDs[i]))
  year<-year(ymd_hms(bird$gmt))[1]
  bird <- rename(bird, utc = gmt)
  bird <- rename(bird, lc = lq)
  bird$sensors<-NA
  Id<-IDs[i]
  write.csv(bird,paste0(dir,"/Species/COMU/1_DataIn/",Id,"_",year,".csv"),row.names=F)
}

meta<-readRDS(file = paste0(gitdir,"supporttables/STA_metadata_2019-09-05_781birds.rda"))
for (i in 1:length(IDs)){
  Id<-as.character(IDs[i])
  idx<-which(meta$argos_id==Id)
  bird<-droplevels(subset(Birds,id==IDs[i]))
  year<-year(ymd_hms(bird$gmt))[1]
  meta$file_name[idx]<-paste0(Id,"_",year)
}
meta%>%filter(species=="COMU")%>%filter(deploy_year==2016)%>%dplyr::select(file_name)
saveRDS(meta, file = paste0(gitdir,"supporttables/STA_metadata_2019-09-05_781birds.rda"))
write.csv(meta, file = paste0(dir,"supporttables/STA_metadata_2019-09-05_781birds.csv"),row.names = FALSE)


# NOFU data .dbf ----------------------------------------------------------
library(foreign)
wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}
chagulak_2002_oct+.dbf
userdir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/NOFU/NOFU telemetry data/"
files <- list.files(path = userdir,recursive = TRUE,full.names = TRUE,
                        pattern="locations.dbf")

Birds<-data.frame()
for (i in 1:length(files)){
track<-read.dbf(files[i], as.is = FALSE)
names(a)
Birds<-bind_rows(Birds,track)
}

Birds<-read.dbf("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/NOFU/NOFU telemetry data/More telemetry data/tottot.dbf", as.is = FALSE)
w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Russia","Mexico","Canada","North Korea","South Korea"),]
names(Birds)
ggplot()+
  #geom_path(data=Birds,aes(y=lat_a,x=wrap360(lon_p), group=animal,color=animal))+
  geom_point(data=Birds%>%filter(lc94!="LZ"),aes(y=lat_a,x=wrap360(lon_p), group=animal,color=animal), size=0.1)+
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),
               fill="black",color="grey60",size=0.1)+
  coord_fixed(ratio=1.7,xlim = c(140,wrap360(-121)),ylim=c(20,65))+facet_wrap(~animal)

Birds<-read.dbf("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/NOFU/NOFU telemetry data/More telemetry data/tottot.dbf", as.is = FALSE)
dir1<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/NOFU/1_DataIn/"

ids<-unique(Birds$animal)
met<-data.frame()
for (i in 1:length(ids)){
  track<-Birds%>%dplyr::filter(animal==ids[i])
  track$ddmm<-as.character(track$ddmm)
  track$mmdd <-as.character(track$mmdd)
  track$deployed <-mdy(track$deployed)
  track$utc<-paste0(track$gmt_date," ",track$gmt_hour,":",track$gmt_min,":",track$gmt_sec)
  track<-rename(track,lc=lc94)
  track$lc<-sub("L","", track$lc)
  track<-rename(track,lat1=lat_p)
  track<-rename(track,lon1=lon_p)
  track<-rename(track,lat2=lat_a)
  track<-rename(track,lon2=lon_a)
  track$uid<-1:nrow(track)
  m<-data.frame(file_name=as.character(ids[i]),
                animal_id=as.character(ids[i]),
                tag_id=track$ptt[1],
                deploy_year=year(track$deployed)[1],
                collab1_point_contact_name="Scott Hatch",
                location_type="Argos",
                deploy_site="Alaska",
                lat_colony=track$deplylat[1],
                lon_colony=track$deplylon[1],
                lat_deploc=track$deplylat[1],
                lon_deploc=track$deplylon[1],
                incl_deploy_loc=1,
                argos_id=track$ptt[1],
                species=track$spp[1],
                age=track$age[1],
                datetime_deploy_UTC=paste0(track$deployed," 00:00")[1])
  met<-rbind(met,m)
  write_csv(track%>%dplyr::select(-ddmm,-mmdd,-deployed),paste0(dir1,ids[i],".csv"))
}

write_csv(met,
          paste0("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/NOFU/NOFU_Hatch_meta.csv"))


# #loon data --------------------------------------------------------------
palo<-read.csv ("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/palo/Pacific Loons Alaska Colville River Delta.csv")
meta<-read.csv("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/supporttables/STA_metadata_2019-09-25_868birds.csv")
p<-meta%>%filter(species=="palo")

names(palo)

palo<-rename(palo,uid=event.id)
palo<-rename(palo,utc=timestamp)
palo<-rename(palo,lc=argos.lc)
palo<-rename(palo,lat1=argos.lat1)
palo<-rename(palo,lon1=argos.lon1)
palo<-rename(palo,lat2=argos.lat2)
palo<-rename(palo,lon2=argos.lon2)

for (i in 1:nrow(palo)){
  if (is.na(palo$lat1[i])==TRUE){
      palo$lat1[i]<-palo$location.lat[i]; palo$lon1[i]<-palo$location.lon[i]} 
}

IDs<-unique(palo$tag.local.identifier)
for (k in 1:length(IDs)){
  birdy<-palo%>%filter(tag.local.identifier==IDs[k])
  info<-p%>%filter(tag_id==IDs[k])
  
  file_name<-paste0("palo_",info$deploy_year,"_",info$site_abbrev,"_",info$animal_id)

  write.csv(birdy, paste0("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/PALO/1_DataIn/",file_name,".csv"))
  }

ggplot()+
  geom_path(data=palo,
              aes(x=wrap360(location.long), y=location.lat, color=as.factor(tag.local.identifier)))


# RTLO --------------------------------------------------------------------
rtlo<-read.csv("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/RTLO/RedThroatedLoon_telemetry_ak_2000_2011/redThroatedLoon_telemetry_ak_uherKoch_2000_2011.csv",
                  na.strings=c("NA","NaN", " ",""),# places a NA in all cells with all these cases
                  stringsAsFactors=FALSE)#gets rid of factors  
names(rtlo)

rtlo$utc<-ymd_hms(paste0(rtlo$date," ",rtlo$hour,":",rtlo$minute,":",rtlo$second))

rtlo<-rename(rtlo,lc=location_class)
rtlo<-rename(rtlo,lat1=lat_p)
rtlo<-rename(rtlo,lon1=lon_p)
rtlo<-rename(rtlo,lat2=lat_a)
rtlo<-rename(rtlo,lon2=lon_a)

info<-rtlo%>%group_by(animal,ptt,herd)%>%
  summarise(datetime_deploy_UTC=min(utc))

unique(rtlo$lc)
rtlo$lc<-str_remove(rtlo$lc,"L")

IDs<-unique(rtlo$animal)
for (k in 1:length(IDs)){
  birdy<-rtlo%>%filter(animal==IDs[k])
  a<-info%>%filter(animal==IDs[k])
  deploy_year<-year(a$datetime_deploy_UTC)
  birdy$uid<-1:nrow(birdy)
  
  file_name<-paste0("RTLO_",deploy_year,"_",a$herd,"_",a$ptt)
  write.csv(birdy, paste0("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/species/RTLO/1_DataIn/",file_name,".csv"))
  
}

# STAL --------------------------------------------------------------------


matlab2POS = function(x,tz = "UTC") {
  days = x - 719529 #719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 #86400 seconds in a day
  return(as.POSIXct(secs,origin = "1970-1-1",tz = tz))#returns POSIXct object
}

