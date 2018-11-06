library(gridExtra)
library(dplyr)
library(reshape)

# DATA PREPRUN SCRIPT: COMU
# data are from Argos PTT deployments 2012-2017
# some were downloaded via SeaTurtle (2012-2015)
# others were downloaded in the 'regular' format (2016 & 2017)

# clear all
rm(list=ls())

species="COMU"

# Set main dir: Sys.info()[7] is the username for the computer.  fill in the "" with your user name 
if(Sys.info()[7]=="rachaelorben") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CCESTA/"}
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

if(Sys.info()[7]=="cherylhorton") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CCESTA/"}
if(Sys.info()[7]=="cherylhorton") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}



source(paste0(gitdir,"STA_Functions.R"))


#TABLES needed to run SDAFreitas_CCESTA filter function
meta<-read.table(paste0(dir,"supporttables/PTT_metadata.csv"),header=T, sep=",", strip.white=T, na.strings = "",stringsAsFactors = FALSE)

parameters <- read.csv (paste0(gitdir,"supporttables/parameters.csv"), header=T, sep=",", strip.white=T,stringsAsFactors = FALSE)
lcerrors <- read.csv(paste0(gitdir,"supporttables/lcerrors.csv"), header=T, sep=",", strip.white=T,stringsAsFactors = FALSE)

meta%>%dplyr::filter(species=="COMU")%>%
  group_by(year,deploy_site,collab1_point_contact_name)%>%
  dplyr::summarize(n_birds=n_distinct(tag_id),minDate=min(datetime_deploy_UTC))

#Tracks are single files file name matching one in the meta file. Saved in "dir.in".  
#Output is a list, obj 1 is the concatinated data, obj 2 is a list of plots, obj 3 is a table of the filtering info
tf_out<-trackfilter(species=species,
                          year=NA,
                          dir=dir,
                          dir.in=paste0(dir,"species/COMU/1_DataIn"),
                          tagtype="ptt", 
                          lcerrref="costa",
                          parameters=parameters,
                          meta=meta,
                          lcerrors=lcerrors)
tracks_filt<-tf_out[[1]]
tf_plots<-tf_out[[2]] #list of ggplots showing prefiltered and filtered locations  
tf_info<-tf_out[[3]]


# Makes Quality Control plots for Freitas Filter --------------------------
pdf(paste0(dir,"species/",species,"/",species,"_trackfilter_QC_plots.pdf"), onefile = TRUE)
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


