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

#loads functions
files.sources = list.files(paste0(gitdir,"R"), full.names = TRUE)
sapply(X = files.sources, FUN=source)

#background maps for plotting
states<-map_data('state') 
states_sub<-states[states$region%in%c("washington","oregon","california","alaska"),]
or_sub<-states[states$region%in%c("oregon"),]
w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("Canada"),]

#"BRAC","WEGU", saving these for later!
species<-c("BFAL","PFSH","SOSH","STAL","NOFU","COMU","RTLO","PALO")

grps<-data.frame(timegrp="all",clipperName=c("PNW_wUSEEZ","Oregon_wUSEEZ"))

# set directories
if(Sys.info()[7]=="rachaelorben") {dir<-"/Users/rachaelorben/Research/SeabirdTrackingAtlas/"} ##RAO
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

SP_Contours<-NULL
Polys_Over<-NULL
for (i in 1:nrow(grps)){
  clipperName<- grps$clipperName[i]
  timegrp<- grps$timegrp[i]

  clipper_list<-readRDS(file=paste0(dir,"polygons/",clipperName,".rda"))
  projWant<-clipper_list$projWant
  clip<-fortify(spTransform(clipper_list$clipper_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))
  buf<-fortify(spTransform(clipper_list$clipperbuff_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))


estUD_list <- vector(mode = "list", length = length(species))
names(estUD_list) <- species

for (i in 1:length(species)){
  sp<-species[i]
  estUD_list[[sp]]<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_bb_3_groups_",timegrp,".rda"))
  }

CON<-NULL
polys<-NULL
for (i in 1:length(species)){
  sp<-species[i]
  estUD<-estUD_list[[sp]]
  estUD$all$den_sum@proj4string<-CRS(projWant)
  estUD$all$den_sum@vol = FALSE
  estUD.ud.vol <- getvolumeUD(estUD$all$den_sum, standardize=TRUE)
  estUD.con<-getverticeshr(estUD.ud.vol,percent = 50)
  estUD.con@data$id<-sp

  estUD.con.wgs84<-spTransform(estUD.con,CRS=CRS("+proj=longlat +ellps=WGS84"))
  
  dat<-fortify(estUD.con.wgs84)
  dat$species<-sp
  CON<-rbind(CON,dat)
  if (length(polys)==0){polys<-estUD.con}
  if (length(polys)==0) next
  polys<-bind(polys,estUD.con)
  }

#grouping variables for ggplot
CON$sp_piece<-paste0(CON$species,"_",CON$piece)
CON$clip_grp<-paste0(clipperName,"_",timegrp)

r<-polyCount(polys,Res = 0.01)
f<-as.data.frame(r,xy = TRUE)
f$clip_grp<-paste0(clipperName,"_",timegrp)
head(f)

SP_Contours<-rbind(SP_Contours,CON)
Polys_Over<-rbind(Polys_Over,f)
}



# Oregon EEZ plots --------------------------------------------------------
clipperName<-"Oregon_wUSEEZ"
clipper_list<-readRDS(file=paste0(dir,"polygons/",clipperName,".rda"))
projWant<-clipper_list$projWant
clip<-fortify(spTransform(clipper_list$clipper_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))
buf<-fortify(spTransform(clipper_list$clipperbuff_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))

A<-ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=SP_Contours%>%filter(clip_grp=="Oregon_wUSEEZ_all"),
               aes(x=long,y=lat,group=sp_piece,fill=species,color=species),alpha=.2)+
  geom_path(data=clip,aes(x=long,y=lat), color="grey50")+
  geom_path(data=buf,aes(x=long,y=lat), color="grey75")+
  coord_fixed(ratio=1.7,xlim = c((-130),(-122)),ylim=c(41,47))+
  labs(fill = "Species", color="Species") +
  xlab("Longitude")+
  ylab("Latitude")+
  theme_classic()

B<-ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_tile(data = Polys_Over%>%
              filter(is.na(layer)==FALSE)%>%
              filter(clip_grp=="Oregon_wUSEEZ_all") , 
            aes(x = x, y = y, fill = as.factor(layer))) +
  scale_fill_viridis_d(direction = (-1)) +
  geom_path(data=clip,aes(x=long,y=lat), color="grey50")+
  geom_path(data=buf,aes(x=long,y=lat), color="grey75")+
  labs(fill = "Core Area\nOverlap\n(# species)")+
  coord_fixed(ratio=1.7,xlim = c((-130),(-122)),ylim=c(41,47))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_classic()

quartz(width=10,height=6)
OR<-grid.arrange(A,B,nrow=1)
ggsave(OR,filename = paste0(dir,"Allspecies","Oregon_wUSEEZ_all","_","all",".png"),width=10,height=6, dpi = 300)

# PNW  --------------------------------------------------------------------
clipperName<-"PNW_wUSEEZ"
clipper_list<-readRDS(file=paste0(dir,"polygons/",clipperName,".rda"))
projWant<-clipper_list$projWant
clip<-fortify(spTransform(clipper_list$clipper_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))
buf<-fortify(spTransform(clipper_list$clipperbuff_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))


C<-ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=SP_Contours%>%filter(clip_grp=="PNW_wUSEEZ_all"),
               aes(x=long,y=lat,group=sp_piece,fill=species,color=species),alpha=.2)+
  #scale_color_viridis_d(direction = (-1)) +
  #scale_fill_viridis_d(direction = (-1)) +
  geom_path(data=clip,aes(x=long,y=lat), color="grey50")+
  geom_path(data=buf,aes(x=long,y=lat), color="grey75")+
  labs(fill = "Species", color="Species") +
  coord_fixed(ratio=1.7,xlim = c((-130),(-122)),ylim=c(40,49))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_classic()


D<-ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_tile(data = Polys_Over%>%
              filter(is.na(layer)==FALSE)%>%
              filter(clip_grp=="PNW_wUSEEZ_all") , 
            aes(x = x, y = y, fill = as.factor(layer))) +
  scale_fill_viridis_d(direction = (-1)) +
  labs(fill = "Core Area\nOverlap\n(# species)")+
  geom_path(data=clip,aes(x=long,y=lat), color="grey50")+
  geom_path(data=buf,aes(x=long,y=lat), color="grey75")+
  coord_fixed(ratio=1.7,xlim = c((-130),(-122)),ylim=c(40,49))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_classic()

quartz(width=10,height=6)
PNW<-grid.arrange(C,D,nrow=1)
ggsave(PNW,filename = paste0(dir,"Allspecies","PNW_wUSEEZ","_","all",".png"),width=10,height=6, dpi = 300)
