library(gridExtra)
library(dplyr)
library(reshape2)
library(ggplot2)
library(argosfilter)
library(lubridate)

# DATA PREPRUN SCRIPT: COMU
# data are from Argos PTT deployments 2012-2017
# some were downloaded via SeaTurtle (2012-2015)
# others were downloaded in the 'regular' format (2016 & 2017)

# clear all
rm(list=ls())

sp="BFAL" #need to add old data + Shaffer to meta, change file names
sp="BRAC" #get & compile GPS data 2014-2019
sp="WEGU" #add to metadata 
sp="COMU" #run Sept19 all years, looks good 
sp="PFSH" #run Sept19 all years, looks good
sp="SOSH" #run Sept19 all years, looks good
sp="STAL" #run Nov19 all years, looks good, need to add Aleutian birds
sp="RTLO" #run Sept19 all years, looks good
sp="PALO" #run Sept19 all years, looks good
sp="NOFU" #run Sept19 all years, 2 birds in EEZ, one other whose track needs cleaning
sp="LAAL"

# Set main dir: Sys.info()[7] is the username for the computer.  fill in the "" with your user name 
#if(Sys.info()[7]=="rachaelorben") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/"}
#if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

# set directories
if(Sys.info()[7]=="rachaelorben") {dir<-"/Users/rachaelorben/Research/SeabirdTrackingAtlas/"} ##RAO
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

if(Sys.info()[7]=="cherylhorton") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CCESTA/"}
if(Sys.info()[7]=="cherylhorton") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}


files.sources = list.files(paste0(gitdir,"R"), full.names = TRUE)
sapply(X = files.sources, FUN=source)


#TABLES needed to run SDAFreitas_CCESTA filter function
meta<-read.table(paste0(dir,"supporttables/STA_metadata_2020-10-19_908birds.csv"),header=T, sep=",", 
                 strip.white=T, na.strings=c("NA","NaN", " ",""),stringsAsFactors = FALSE)
meta$datetime_deploy_UTC<-mdy_hm(meta$datetime_deploy_UTC)
meta$datetime_end_track_UTC<-mdy_hm(meta$datetime_end_track_UTC)
meta$datetime_recover_UTC<-mdy_hm(meta$datetime_recover_UTC)
meta$datetime_start_track_UTC<-mdy_hm(meta$datetime_start_track_UTC)
str(meta)  
saveRDS(meta,paste0(dir,"supporttables/STA_metadata_2020-10-19_908birds.rds"))

parameters <- read.csv (paste0(gitdir,"supporttables/parameters.csv"), header=T, sep=",", strip.white=T,stringsAsFactors = FALSE)
lcerrors <- read.csv(paste0(gitdir,"supporttables/lcerrors.csv"), header=T, sep=",", strip.white=T,stringsAsFactors = FALSE)

(t<-meta%>%dplyr::filter(species==sp)%>%dplyr::filter(loc_data==1)%>%
  group_by(deploy_year,deploy_site,collab1_point_contact_name,location_type)%>%
  dplyr::summarize(n_birds=n_distinct(STA_id),minDate=min(datetime_deploy_UTC),
                                                           minDate2=min(datetime_start_track_UTC)))
t$species<-sp
write.table(t, file=paste0(dir,"species/DataSetSum.csv"),sep = ",", append = TRUE,col.names = NA)

meta%>%dplyr::filter(species==sp)%>%dplyr::select(STA_id,file_name, loc_data)

#Tracks are single files file name matching one in the meta file. Saved in "dir.in".  
#Output is a list, obj 1 is the concatinated data, obj 2 is a list of plots, obj 3 is a table of the filtering info
tf_out<-track_prep_filter(species=sp,
                          year=NA,
                          dir=dir,
                          dir.in=paste0(dir,"species/",sp,"/1_DataIn"),
                          tagtype="gps", #ptt #gps
                          lcerrref="costa",
                          parameters=parameters,
                          meta=meta,
                          lcerrors=lcerrors)
tracks_filt<-tf_out$tracks_filt
tf_plots<-tf_out$tf_plot #list of ggplots showing prefiltered and filtered locations  
tf_info<-tf_out$tf_info
tf_missing<-tf_out$missing #ids of missing data - should be empty

# Makes Quality Control plots for Freitas Filter --------------------------
pdf(paste0(dir,"species/",sp,"/QCplots_",sp,"_trackfilter.pdf"), onefile = TRUE)
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
tail(filt_error) 

save(tracks_filt,tf_info,filt_sum,filt_error, 
     file = paste0(dir,"species/",sp,"/",sp,"_trackfilter.RData"))


#Optional output formats not used in the next step (.RData file is used instead)
#write.csv(tracks_filt,file=paste0(dir,"species/",species,"/",species,"_trackfilter.csv"))


