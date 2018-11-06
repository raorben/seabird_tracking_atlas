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
(speed<-unique(tf_info$vmax))

# #########################################################################
# Clips tracks to the polygon   -------------------------------------------
## adds a 1 or 0 for in or out of polygon, or buffer
tracksclipped<-clip_topoly(all_tracks=tracks_filt,
                                  CLIPPERS=clipper,
                                  dir.out=dir,
                                  prjtracks="+proj=longlat +ellps=WGS84 +datum=WGS84")

tracks_filt_clip_spatialdf<-tracksclipped[[1]];Clipper.Plots<-tracksclipped[[2]];tracks_filt_clip<-tracksclipped[[3]]

# Adds a time component to identify and label segments -------------------------------
tracks_filt_clip_seg<-calc_leavetimesegs(hrs=72, tracks=tracks_filt_clip, clipperName)
head(tracks_filt_clip_seg)
(Idx<-which(grepl(paste(clipperName,'_id2',sep=''), names(tracks_filt_clip_seg))))
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

segments<-bb_segmentation(ptt=tracks_filt_clip_seg, #tracking data
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

saveRDS(segments,file=paste0(dir,"species/",species,"/",species,"_",clipperName,"_segmentBB.rda"))
segments<-readRDS(file=paste0(dir,"species/",species,"/",species,"_",clipperName,"_segmentBB.rda"))

bb<-segments[[1]]; bbvol<-segments[[2]]; tracksums.out<-segments[[3]];contour=segments[[4]]
tag <- names(bb)

### Makes Quality Control plots for IndividualBB --------------------------
pdf(paste0(dir,"species/",species,"/",species,"_QCplots_",clipperName,"_segmentBB.pdf"), onefile = TRUE)
  for (i in 1:length(tag)) {
    image(bb[[i]], useRasterImage=TRUE,col=c("light grey", topo.colors(40)))
  }
dev.off()

### Compiles Segments BB by Individuals and Groups ----------------------------------------

  #set grouping variable (year, season, month)
  #needs to be the same used for segmentation if individuals span groups
  
tracksums.out$grp<-lubridate::year(tracksums.out$date.begin)

bbindis<-bb_individuals(bb_probabilitydensity=bb, #Output from IndividualBB
                        tracksums.out)  #ignore directory errors if the directories already exist
bbindis
#bbindis is a list of estUDm for each grouping variable. Then each estUDm contains a list of estUDs 
#for each individual weighted by the tracking time inside the polygon

#sum individual densities by group weighted by the number of days for each individual / total days:
names(bbindis) #check groups
bbgroups<-bb_sumbygroup(bbindis,tracksums.out)



# Plot Rasters  ------------------------------------------------------------
library(ggplot2)

w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("USA","Mexico","Canada","Alaska"),]
states<-map_data('state') 
states_sub<-states[states$region%in%c("washington","oregon","california","alaska"),]

CLIPPERS<-readRDS(file=paste0(dir,"polygons/CCESTA_",clipperName,".rda")) #list(clipper,clipper_proj,clipperBuff_proj,projWant,clipperName)
clipper<-CLIPPERS[[1]]
(prj<-CLIPPERS[[4]])
clipper_proj<-spTransform(clipper, CRS(prj))


#convert the raster to points for plotting
a<-bbgroups[[2]]
allgrps.indiv.raster<-raster(a$den_sum)
## crop and mask
proj4string(allgrps.indiv.raster)<-CRS("+proj=aea +lat_1=30 +lat_2=50 +lat_0=40 +lon_0=-125 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
proj4string(allgrps.indiv.raster)
proj4string(clipper_proj)

cellStats(allgrps.indiv.raster,'sum')
r2 <- crop(allgrps.indiv.raster, extent(clipper_proj))
allgrps.indiv.raster.clip <- raster::mask(r2, clipper_proj)

## Check that it worked
plot(allgrps.indiv.raster.clip)
plot(clipper_proj, add=TRUE, lwd=2)

allgrps.indiv.raster.clip.wgs84<-projectRaster(allgrps.indiv.raster.clip,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

map.p <- rasterToPoints(allgrps.indiv.raster.clip.wgs84)

#Now make the map
#Make the points a dataframe for ggplot
df <- data.frame(map.p)
head(df)
colnames(df)<-c("Longitude","Latitude","Density")
df[df$Density==0,]<-NA

df$Density.n<-sqrt(df$Density)#*10000
hist(df$Density.n)
#df$Density.d<-NA
#df$Density.d[df$Density.n>=0.75 & df$Density.n<=0.95]<-95
#df$Density.d[df$Density.n<=0.75 & df$Density.n>=0.50]<-75  
#df$Density.d[df$Density.n<=0.50 & df$Density.n>=0.25]<-50  
#df$Density.d[df$Density.n<=0.25]<-25  

A<-ggplot(data=fortify(df)%>%dplyr::filter(Density.n>0.000000001), aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Density.n)) +
  scale_fill_gradientn(colors=c("#3096C6", "#9EC29F","#F2EE75", "#EE5B2F","#E7292A"),name="Density.n") +
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
  geom_polygon(data=clipper,aes(long,lat,group=group),fill="NA",color="black",size=.5)+
  theme_bw() +
  coord_equal() +
  coord_fixed(ratio=1.7,xlim = c(-126.5,-121),ylim=c(37,48))+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

B<-ggplot(data=fortify(df), aes(y=Latitude, x=Longitude)) +
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
  geom_polygon(data=clipper,aes(long,lat,group=group),fill="NA",color="black",size=.5)+
  geom_path(data=tracks_filt%>%
              filter(year==2013)%>%
              filter(keeps==1),
            aes(x=lon1,y=lat1,group=tag_id,color=as.factor(tag_id)),size=0.2)+
  theme_bw() +
  coord_equal() +
  coord_fixed(ratio=1.7,xlim = c(-126.5,-121),ylim=c(37,48))+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

gridExtra::grid.arrange(A,B,ncol=2)
