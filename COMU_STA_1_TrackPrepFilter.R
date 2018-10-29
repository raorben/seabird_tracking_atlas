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
source(paste0(gitdir,"STA_Functions.R"))


#TABLES needed to run SDAFreitas_CCESTA filter function
meta<-read.table(paste0(dir,"supporttables/PTT_metadata.csv"),header=T, sep=",", strip.white=T, na.strings = "",stringsAsFactors = FALSE)
parameters <- read.csv (paste0(dir,"supporttables/parameters.csv"), header=T, sep=",", strip.white=T,stringsAsFactors = FALSE)
lcerrors <- read.csv(paste0(dir,"supporttables/lcerrors.csv"), header=T, sep=",", strip.white=T,stringsAsFactors = FALSE)

meta%>%filter(species=="COMU")%>%
  group_by(year,deploy_site,collab1_point_contact_name)%>%
  summarize(n_birds=n_distinct(tag_id),minDate=min(datetime_deploy_UTC))

#Tracks are single files file name matching one in the meta file. Saved in "dir.in".  
#Output is a list, obj 1 is the concatinated data, obj 2 is a list of plots, obj 3 is a table of the filtering info
output<-SDAFreitas_CCESTA(species=species,
                          year=NA,
                          dir=dir,
                          dir.in=paste0(dir,"species/COMU/1_DataIn"),
                          tagtype="ptt", 
                          lcerrref="costa",
                          parameters=parameters,
                          meta=meta,
                          lcerrors=lcerrors)
tracks<-output[[1]]
PLOTS<-output[[2]]
info<-output[[3]]

# Makes Quality Control plots for Freitas Filter --------------------------
pdf(paste0(dir,"species/",species,"/",species,"_CCESTA_1_Freitas_QC_plots.pdf"), onefile = TRUE)
for(i in 1:length(PLOTS)){
  top.plot <-PLOTS[[i]]
  grid.arrange(top.plot)
}
dev.off()


# Summarizes the filtering done by the Freitas filter ---------------------
filter.results<-SummaryFreitas_filtered(tracks)
head(filter.results)


# Error estimates for each tag - based on performance after filter --------
indiv.error.results<-SummaryFreitas_errorRetained(tracks,lcerrors,"costa") #ignore errors
head(indiv.error.results) 
tail(indiv.error.results) #wacky things at bottom are mean, sd, and number of birds

OUTPUT<-list(tracks,PLOTS,info,filter.results,indiv.error.results)
saveRDS(OUTPUT,file=paste0(dir,"species/",species,"/",species,"_CCESTA_1_FreitasFilt.rda"))
OUTPUT<-readRDS(file=paste0(dir,"species/",species,"/",species,"_CCESTA_1_FreitasFilt.rda"))

tracks<-OUTPUT[[1]];PLOTS<-OUTPUT[[2]];info<-OUTPUT[[3]];
filter.results<-OUTPUT[[4]];indiv.error.results<-OUTPUT[[5]]


