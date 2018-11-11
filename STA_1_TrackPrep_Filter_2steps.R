library(gridExtra)
library(dplyr)
library(reshape2)
library(lubridate)
library(argosfilter)

# DATA PREPRUN SCRIPT: COMU
# data are from Argos PTT deployments 2012-2017
# some were downloaded via SeaTurtle (2012-2015)
# others were downloaded in the 'regular' format (2016 & 2017)

# clear all
rm(list=ls())

species="BFAL"
sp="BFAL"

# Set main dir: Sys.info()[7] is the username for the computer.  fill in the "" with your user name 
if(Sys.info()[7]=="rachaelorben") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/"}
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

if(Sys.info()[7]=="cherylhorton") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CCESTA/"}
if(Sys.info()[7]=="cherylhorton") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}



source(paste0(gitdir,"STA_Functions.R"))


#TABLES needed to run SDAFreitas_CCESTA filter function
meta<-read.table(paste0("/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/STA_rcode_metadata.csv"),header=T, sep=",", strip.white=T, na.strings = "",stringsAsFactors = FALSE)
meta$datetime_deploy_UTC<-as.POSIXct(strptime(meta$datetime_deploy_UTC,format="%m/%d/%Y %H:%M", tz="UTC"))
meta$datetime_recover_UTC<-as.POSIXct(strptime(meta$datetime_recover_UTC,format="%m/%d/%Y %H:%M", tz="UTC"))
meta$capture_type<-"at sea"
meta<-meta%>%dplyr::filter(species==sp)
meta$tag_type<-"ptt"

parameters <- read.csv (paste0(gitdir,"supporttables/parameters.csv"), header=T, sep=",", strip.white=T,stringsAsFactors = FALSE)
lcerrors <- read.csv(paste0(gitdir,"supporttables/lcerrors.csv"), header=T, sep=",", strip.white=T,stringsAsFactors = FALSE)

meta%>%dplyr::filter(species==sp)%>%
  group_by(deploy_year,deploy_site,collab1_point_contact_name)%>%
  dplyr::summarize(n_birds=n_distinct(tag_id),minDate=min(datetime_deploy_UTC))

  Files.birds<-list.files(paste0(dir,"species/",sp,"/1_DataIn/"),pattern = ".csv",full.names = TRUE,recursive = TRUE)
  Files.birds.short<-list.files(paste0(dir,"species/",sp,"/1_DataIn/"),pattern = ".csv",full.names = FALSE,recursive = T)

  tracks<-NULL
  for (i in 1: length(Files.birds)){
    birdy<-read.csv(Files.birds[[i]])
    a<-strsplit(x = Files.birds.short[[i]],split = "_")[[1]][4]
    ID<-gsub(".csv", "", a)
    
    birdy$utc<-as.POSIXct(strptime(birdy$utc,format="%m/%d/%Y %H:%M", tz="UTC"))

    binfo<-meta %>% dplyr::filter(tag_id==ID)
    
    if (is.na(binfo$datetime_recover_UTC)==TRUE)
      {birds<-birdy %>% filter(utc>binfo$datetime_deploy_UTC)}
    if (is.na(binfo$datetime_recover_UTC)==FALSE)
      {birds<-birdy %>% filter(utc>binfo$datetime_deploy_UTC) %>%
                               filter(utc<binfo$datetime_recover_UTC)}
    
    if (binfo$tag_type=="ptt" & binfo$capture_type=="at sea"){#add deployment location for PTT tags
      birdy<-rbind(birdy[1,],birdy)
      birdy$utc[1]<-binfo$datetime_deploy_UTC
      birdy$lat1[1]<-binfo$lat_deploc
      birdy$lon1[1]<-binfo$lon_deploc
    }
    
    head(birdy) 
    colnames(birdy)
    birdys<-birdy%>%dplyr::select(uid,uuid,prognum,tag_id,utc,lc,iq,lat1,lon1,lat2,lon2)
    birdys<-birdys%>%filter(lon1!=0)
    birdys$lat1<-as.numeric(birdys$lat1)
    birdys$lon1<-as.numeric(birdys$lon1)
    birdys$uniID<-paste0(birdys$tag_id,"_",year(birdys$utc[1]))
    birdys$tag_type<-binfo$tag_type
    tracks<-bind_rows(tracks,birdys)
  }
    
  length(unique(tracks$uniID))
  length(Files.birds)
  length(Files.birds.short)
  


  iDs<-unique(tracks$uniID)
  all_tracks<-NULL
  Track.Plots<-vector("list",length(unique(tracks$uniID)))
  INFO<-NULL
  
  for (k in 1:length(unique(tracks$uniID))) {
    
    track<-tracks%>%dplyr::filter(uniID==iDs[k])
    TrackLengthOrig=nrow(track)
    paramwant<-subset(parameters,(spp==species))
    vmax <- unlist(matrix(paramwant%>%
                          dplyr::filter(tag==track$tag_type[1])%>%
                          dplyr::select(vmaxSA_Tab2_W5ms_x_3SD)))
    ang <- unlist(matrix(paramwant%>%dplyr::filter(tag==track$tag_type[1])%>%dplyr::select(ang1,ang2)))
    distlim <- unlist(matrix(paramwant%>%dplyr::filter(tag==track$tag_type[1])%>%dplyr::select(distlim1,distlim2)))
    
    if (track$tag_type[1]=="GPS"){track$lc<-"G"}

    # replace lc with numbers
    track$lc <- as.character(track$lc) # change to characters
    
    # new order of accuracy
    # 3,2,1,0,-1,-2,-9
    track$lc[track$lc == "A"] <- -1
    track$lc[track$lc == "B"] <- -2
    track$lc[track$lc == "Z"] <- -9
    track$lc[track$lc == "G"] <- 99
    track$lc <- as.numeric(track$lc) # change to numbers
    
    
    #### delete duplicate points retaining based on 1. >lc & 2. >nb_mes, or 3. first duplicate, trash others
    #track <-  track %>% distinct(uniID,utc,.keep_all = T) %>%
    #  arrange(uniID,utc)
    track$utc<-trip::adjust.duplicateTimes(track$utc, track$uniID)
    
   
    ##needs IDs to be a character due to leading 0s
    spmk1<- sdafilter (track$lat1, track$lon1, track$utc, track$lc, vmax, ang, distlim)
      
      track$lat3<-track$lat1
      track$lon3<-track$lon1
      
      indi1 <- which(spmk1 == "removed") 
      for (i in 1:length(indi1))
      {indic<-indi1[i]
      track$lon3[indic]<-track$lon2[indic]
      track$lat3[indic]<-track$lat2[indic]}
      
      spmk2<- sdafilter (track$lat3, track$lon3, track$utc, track$lc, vmax, ang, distlim)
      indi2 <- which(spmk1 == "removed") 
      c<-intersect(indi1,indi2)
      a<-setdiff(indi1,indi2)
      
      for (i in 1:length(a))
      {indic<-a[i]
      track$lon1[indic]<-track$lon2[indic]
      track$lat1[indic]<-track$lat2[indic]}
      
      spmk1<- sdafilter (track$lat1, track$lon1, track$utc, track$lc, vmax, ang, distlim)
      track$cfilter<-spmk1
      birdnew<-subset(track,track$cfilter!="removed") ##remove too fast points
      Tracklength_clipped<-nrow(birdnew)
      
      #### plots retained SDA points on plot in blue	
      Track.Plots[[k]] <- ggplot()+
        geom_path(data=track, aes(x=wrap360(lon1),y=lat1),color="lightgrey")+
        geom_path(data=birdnew[which(birdnew$cfilter=="not"),],aes(x=wrap360(lon1),y=lat1),color="blue")+
        geom_point(data=birdnew[1,],aes(x=wrap360(lon1),y=lat1),color="green", cex=1.3)+
        geom_point(data=birdnew[nrow(birdnew),],aes(x=wrap360(lon1),y=lat1),color="red")+
        geom_text(data=birdnew,aes(x=max(wrap360(lon1))-5,y=max(lat1),label=track$uniID[1]))+
        xlab("Longitude")+
        ylab("Latitude")+
        theme_classic()
      
      info<-data.frame(animal.id=track$uniID,ptt_deploy_id=track$tag_id,
                       vmax,ang[1],ang[2],distlim[1],distlim[2],
                       lcerrref=NA,TrackLengthOrig,Tracklength_clipped,
                       TrackLength_ends_added=NA,latlon0=NA,dupremoved=NA,retained=NA)
      info$animal.id<-as.character(info$animal.id)
      info$lcerrref<-as.character(info$lcerrref)
      
      # bind all info
      INFO<-bind_rows(INFO,info) #was rbind_fill()
      
      track$ptt_deploy_id=track$tag_id
      
      #### create vector for points kept
      birdnew$keeps <- as.numeric((birdnew$cfilter=="not") | (birdnew$cfilter=="end_location")) #* (1-as.numeric(track$filtered=="end_location_rem")))
        
      all_tracks<- rbind(all_tracks,birdnew)
  }
  tf_out<-list(all_tracks,Track.Plots,INFO)
  names(tf_out)<-c("tracks_filt","tf_plots","tf_info")
      

tracks_filt<-tf_out$tracks_filt
tf_plots<-tf_out$tf_plot #list of ggplots showing prefiltered and filtered locations  
tf_info<-tf_out$tf_info

# Makes Quality Control plots for Freitas Filter --------------------------
pdf(paste0(dir,"species/",species,"/QCplots_",species,"_trackfilter.pdf"), onefile = TRUE)
for(i in 1:length(tf_plots)){
  top.plot <-tf_plots[[i]]
  grid.arrange(top.plot)
}
dev.off()


# Summarizes the filtering done by the Freitas filter ---------------------
filt_sum<-tf_filt_sum(tracks_filt)
head(filt_sum)

# Error estimates for each tag - based on performance after filter --------
filt_error<-tf_filt_error(tracks_filt,filt_sum,lcerrors,"costa") #ignore errors
head(filt_error) 
tail(filt_error) #wacky things at bottom are mean, sd, and number of birds

save(tracks_filt,tf_info,filt_sum,filt_error, 
     file = paste0(dir,"species/",species,"/",species,"_trackfilter.RData"))


#Optional output formats not used in the next step (.RData file is used instead)
#write.csv(tracks_filt,file=paste0(dir,"species/",species,"/",species,"_trackfilter.csv"))


