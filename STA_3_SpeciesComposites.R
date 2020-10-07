#spatial stuff
library(adehabitatHR)
library(SDMTools)
library(raster)
library(sp)
library(marmap)

#data manipulation
library(stringr)
library(dplyr)
library(trip) #segmentleavetime
library(lubridate)

#plotting
library(ggplot2) #tracksclipped
library(gridExtra)#for pdfs
library(cowplot)#for multi-panel plots

rm(list=ls()) #empty environment
#"BRAC","WEGU", saving these for later!

species<-c("BFAL","COMU","PFSH","SOSH","STAL","RTLO","PALO","NOFU")
timegrp<-"all" #"year", "all", "season"
clipperName<-"Oregon_wUSEEZ" #"PNW_wUSEEZ"

# set directories
if(Sys.info()[7]=="rachaelorben") {dir<-"/Users/rachaelorben/Research/SeabirdTrackingAtlas/"} ##RAO
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

files.sources = list.files(paste0(gitdir,"R"), full.names = TRUE)
sapply(X = files.sources, FUN=source)

estUD_list <- vector(mode = "list", length = length(species))
names(estUD_list) <- species

for (i in 1:length(species)){
  sp<-species[i]
  estUD_list[[sp]]<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_bb_3_groups_",timegrp,".rda"))
  }


CON<-NULL
for (i in 1:length(species)){
  sp<-species[i]
  estUD<-estUD_list[[sp]]
  
  estUD$all$den_sum@vol = FALSE
  estUD.ud.vol <- getvolumeUD(estUD$all$den_sum, standardize=TRUE)
  estUD.con<-getverticeshr(estUD.ud.vol,percent = 50)
  
  dat<-fortify(estUD.con)
  dat$species<-sp
  CON<-rbind(CON,dat)
  }

#grouping variable for ggplot
CON$sp_piece<-paste0(CON$species,"_",CON$piece)

ggplot()+
  geom_path(data=CON,
            aes(x=long,y=lat,group=sp_piece,color=species))


