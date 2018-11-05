library(adehabitatHR)
library(SDMTools)
library(raster)
library(stringr)


library(dplyr)
library(sp)
library(ggplot2) #tracksclipped
library(trip) #segmentleavetime
library(gridExtra)#for pdfs
rm(list=ls())


species<-"COMU"


# set directories
if(Sys.info()[7]=="rachaelorben") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CCESTA/"} ##RAO
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}
source(paste0(gitdir,"STA_Functions.R"))

# Read in Clipper ---------------------------------------------------------
clipperName<-"PACSEA_convhull_All_coastclip"

# read in list of all potential clipper files
# polys are stored as WGS84 and then projected in MakeClippers.R
# this reads in the polygon info and the processed Clipper. 

polyinfo<-read.csv (paste(dir,"supporttables/clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
print(polyinfo%>%filter(name==clipperName)) # shows clipper info. 
clipper<-readRDS(file=paste0(dir,"polygons/",clipperName,".rda"))
plot(clipper[[2]], axes=T,  border="gray") #clipper
plot(clipper[[3]], add=T) #buffer

# read in bird metadata ---------------------------------------------------
meta<-read.table(paste0(dir,"supporttables/PTT_metadata.csv"),header=T, sep=",", strip.white=T, na.strings = "")
meta<-meta[meta$species==species,]

# reads in Frietas filtered tracking data for species
# get speed value used in argosfilter::sdafilter
load(file=paste0(dir,"species/",species,"/",species,"_trackfilter.RData"))
speed<-tf_info$vmax[1]

# #########################################################################
# Clips tracks to the polygon   -------------------------------------------
## adds a 1 or 0 for in or out of polygon, or buffer
tracksclipped<-PolygonClip(all_tracks=tracks_filt,
                                  CLIPPERS=clipper,
                                  dir.out=dir,
                                  prjtracks="+proj=longlat +ellps=WGS84 +datum=WGS84")

tracks_filt_clip_spatialdf<-tracksclipped[[1]];Clipper.Plots<-tracksclipped[[2]];tracks_filt_clip<-tracksclipped[[3]]

# Adds a time component to identify and label segments -------------------------------
tracks_filt_clip_seg<-segmentleavetime(hrs=72, tracks=tracks_filt_clip, clipperName)

Idx<-which(grepl(paste(clipperName,'_id2',sep=''), names(tracks_filt_clip_seg)))
unique(tracks_filt_clip_seg[,Idx]) #how many segments?


# Makes Quality Control plots for PolygonClip --------------------------
pdf(paste0(dir,"species/",species,"/",species,"_QCplots_",clipperName,".pdf"), onefile = TRUE)
for(i in 1:length(Clipper.Plots)){
  top.plot <-Clipper.Plots[[i]]
  grid.arrange(top.plot)
}
dev.off()



tracks_filt_clip_seg$time.grp.var<-tracks_filt_clip_seg$year

# Calculate Segment BrownianBridges ------------------------------------
cellsize<-3000
resolution="3km" # cell size in km, used in file names

SegmentBB<-segmentBB(ptt=tracks_filt_clip_seg, #tracking data
                            clipperName, #e.g. "PACSEA_buff33_coastclip", must match what you have used.
                            CLIPPERS=clipper, #output from: PolygonPrep_CCESTA with desired polygon
                            speed,
                            id.out = c("99999"), # to manually exclude birds or segments "99999" excludes none
                            contour=99.999, # the maximum contour to return, use 99.999 for 100 ud contours
                            sig2=cellsize,#
                            cellsize=cellsize,# related to error tags, in m
                            minNo=2,#minimum number of points to run for the bb - important with small clip areas where tracks are cut up when animal enters and leaves a box
                            id.2="seg",##all=bird.id (run entire ptt) or "seg"=clip.name_id2 (run segments of track that are in box then sum them based on number of days tracked)
                            tagtype="ptt",
                            meta=meta) #metadata from PTT_metadata_all.csv for the species

saveRDS(SegmentBB,file=paste0(dir,"species/",species,"/",species,"_",clipperName,"_segmentBB.rda"))
SegmentBB<-readRDS(file=paste0(dir,"species/",species,"/",species,"_",clipperName,"_segmentBB.rda"))
bb<-SegmentBB[[1]]; bbvol<-SegmentBB[[2]]; tracksums.out<-SegmentBB[[3]];contour=SegmentBB[[4]]
tag <- names (bb)

### Makes Quality Control plots for IndividualBB --------------------------
  pdf(paste0(dir,"species/",species,"/",species,"_QCplots_",clipperName,"_segmentBB.pdf"), onefile = TRUE)
  for (i in 1:length(tag)) {
    image(bb[[i]], useRasterImage=TRUE,col=c("light grey", topo.colors(40)))
  }
  dev.off()

### Compiles Segments BB by Individuals and Groups ----------------------------------------
bb<-SegmentBB[[1]]; bbvol<-SegmentBB[[2]]; tracksums<-SegmentBB[[3]]

  #set grouping variable (year, season, month)
  #needs to be the same used for segmentation if individuals span groups
  
  tracksums$grp<-lubridate::year(tracksums$date.begin)

  bbindis<-bb_individuals(bb_probabilitydensity=bb, #Output from IndividualBB
                        tracksums)  #ignore directory errors if the directories already exist
bbindis
#bbindis is a list of estUDm for each grouping variable. Then each estUDm contains a list of estUDs 
#for each individual weighted by the tracking time inside the polygon










# make summary table for all years
summary.grp.ids.mat<-do.call(rbind, summary.grp.ids)
grp.id.wants<-as.data.frame(table(summary.grp.ids.mat[,1]))

# initiate loop to sum all years to create raster for 
# 1. all years weighted by (number of individuals tracked for that year/total number individuals tracks for all years)
# 2. number of birds per cell for all years

# grp.id <-1
for (grp.id in 1:length(grp.ids)) {
  # allyrs.dur.notracks.wght=ud.years[[yr]] * (# individuals tracked for year i/sum # individuals tracked for all years)
  if (exists("allgrps.dur.notracks.wght")) {
    allgrps.dur.notracks.wght<-allgrps.dur.notracks.wght + (ud.grp.ids[[grp.id]]*(grp.id.wants[grp.id,2]/sum(grp.id.wants[,2])))
    allgrps.indiv<-allgrps.indiv + noindiv.grp.ids[[grp.id]]  
  } else {
    allgrps.dur.notracks.wght<-(ud.grp.ids[[grp.id]]*(grp.id.wants[grp.id,2]/sum(grp.id.wants[,2])))
    allgrps.indiv<-noindiv.grp.ids[[grp.id]] 
  } 
}

# examine distribution of contours
hist(log(allgrps.dur.notracks.wght),100)
hist((allgrps.dur.notracks.wght),1000)
hist(log(allgrps.indiv),100)
hist((allgrps.indiv),1000)

# export summary table
write.table(as.data.frame(summary.grp.ids.mat), paste(dir,species,"/3_BB_out/",species,"_",clipperName,"_",resolution,"_",min(grp.ids),"_",max(grp.ids),"_tracksummary.csv", sep=""), sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE,na="")

# export files for all tracks weighted by no ind tracked/year
# export as .acs (ASCII = although Arc does not recognize header info), used to import into arc
write.asc(allgrps.indiv, paste(dir,species,"/5_Compiled/",clipperName,"/",resolution,"/",species,"_",resolution,"_all_ni", sep=""),gz=FALSE)
write.asc(allgrps.dur.notracks.wght, paste(dir,species,"/5_Compiled/",clipperName,"/",resolution,"/",species,"_",resolution,"_all_bb", sep=""),gz=FALSE)
sum(allgrps.dur.notracks.wght,na.rm=TRUE)

image(allgrps.dur.notracks.wght)

# Plot Rasters  ------------------------------------------------------------
species="COMU"
CCESTArasterplot(clipperName,dir,
                 raster.in.dir=paste0("species/",species,"/3_Compiled/",clipperName,"/3km/"),
                 rastername=paste0(species,"_3km_all_bb.asc"))

CCESTArasterplot(clipperName,dir,
                 raster.in.dir=paste0("species/",species,"/3_Compiled",clipperName,"/3km/"),
                 rastername=paste0(species,"_2015_3km_bb.asc"))

CCESTArasterplot(clipperName,dir,
                 raster.in.dir=paste0("species/",species,"/3_Compiled/",clipperName,"/3km/"),
                 rastername=paste0(species,"_2010_3km_bb.asc"))

CCESTArasterplot(clipperName,dir,
                 raster.in.dir=paste0("species/",species,"/3_Compiled/",clipperName,"/3km/"),
                 rastername=paste0(species,"_2011_3km_bb.asc"))

CCESTArasterplot(clipperName,dir,
                 raster.in.dir=paste0("species/",species,"/3_Compiled/",clipperName,"/3km/"),
                 rastername=paste0(species,"_2012_3km_bb.asc"))

CCESTArasterplot(clipperName,dir,
                 raster.in.dir=paste0("species/",species,"/3_Compiled/",clipperName,"/3km/"),
                 rastername=paste0(species,"_2013_3km_bb.asc"))



raster.in.dir=paste0(species,"/3_Compiled/",clipperName,"/3km/")
rastername=paste0(species,"_3km_all_bb.asc")
library(ggplot2)

w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Mexico","Canada","Alaska"),]
states<-map_data('state') 
states_sub<-states[states$region%in%c("washington","oregon","california","alaska"),]

#convert the raster to points for plotting
allgrps.indiv.raster<-raster(paste0(dir,raster.in.dir,rastername))
cellStats(allgrps.indiv.raster,'sum')

proj4string(allgrps.indiv.raster)<-CRS("+proj=aea +lat_1=30 +lat_2=50 +lat_0=40 +lon_0=-125 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
allgrps.indiv.raster.wgs84<-projectRaster(allgrps.indiv.raster,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

map.p <- rasterToPoints(allgrps.indiv.raster.wgs84)

CLIPPERS<-readRDS(file=paste0(dir,"polygons/CCESTA_",clipperName,".rda")) #list(clipper,clipper_proj,clipperBuff_proj,projWant,clipperName)
clipper<-CLIPPERS[[1]]

#Now make the map
#Make the points a dataframe for ggplot
df <- data.frame(map.p)
head(df)
colnames(df)<-c("Longitude","Latitude","Density")
df[df$Density==0,]<-NA

df$Density.n<-sqrt(df$Density)
df$Density.d<-NA
df$Density.d[df$Density.n>=0.75 & df$Density.n<=0.95]<-95
df$Density.d[df$Density.n<=0.75 & df$Density.n>=0.50]<-75  
df$Density.d[df$Density.n<=0.50 & df$Density.n>=0.25]<-50  
df$Density.d[df$Density.n<=0.25]<-25  

ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Density.n)) +
  scale_fill_gradientn(colors=c("#3096C6", "#9EC29F","#F2EE75", "#EE5B2F","#E7292A"),name="Density.n") +
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
  geom_polygon(data=clipper,aes(long,lat,group=group),fill="NA",color="gray",size=.2)+
  theme_bw() +
  coord_equal() +
  coord_fixed(ratio=1.7,xlim = c(-126.5,-121),ylim=c(37,48))+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(data=df, aes(y=Latitude, x=Longitude,fill=as.factor(Density.d))) +
  scale_fill_manual(values=c("#3096C6", "#9EC29F","#F2EE75", "#EE5B2F"),name="Density.n") +
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
  geom_polygon(data=clipper,aes(long,lat,group=group),fill="NA",color="gray",size=.2)+
  theme_bw() +
  coord_equal() +
  coord_fixed(ratio=1.7,xlim = c((-130),(-160)),ylim=c(52.5,62))+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

filename1 <- sapply(strsplit(rastername, split='.', fixed=TRUE), function(x) (x[1]))
ggsave(paste0(dir,filename1,".png"))
}



### Export Individual BB ASCII files for ArcMap -----------------------------------------
#fix error if directory doesn't exist
#changed with depretiation of adehabitat - > what outputs do we want?
#ExportASCII_SegmentBB(SegmentBB, species, clipperName,cellsize=cellsize3km, dir)


# ### export .shp with tracking data for all bird.ids (makes a folder)
#tracks.filts.sp<-tracksclipped[[1]]
#clipperName<-tracksclipped[[4]]
#  writeOGR(obj=tracks.filts.sp,dsn=paste(dir,"species/",species,"/",species,'_all_pts_Freitas_in',clipperName, sep = ""),
#           layer=paste(species,'all_pts_Freitas_in',clipperName, sep = ""),
#           overwrite_layer='T', driver="ESRI Shapefile")


