#spatial stuff
library(adehabitatHR)
library(SDMTools)
library(raster)
library(sp)

#data manipulation
library(stringr)
library(dplyr)
library(trip) #segmentleavetime
library(lubridate)

#plotting
library(ggplot2) #tracksclipped
library(gridExtra)#for pdfs

rm(list=ls()) #empty environment


species<-"COMU"


# set directories
if(Sys.info()[7]=="rachaelorben") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/SeabirdTrackingAtlas/"} ##RAO
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

source(paste0(gitdir,"STA_Functions.R"))

# Read in Clipper ---------------------------------------------------------
clipperName<-"PACSEA_convhull_All_coastclip"

# read in list of all potential clipper files
# polys are stored as WGS84 and then projected in MakeClippers.R
# this reads in the polygon info, the processed Clipper and plots it.

polyinfo<-read.csv (paste(gitdir,"supporttables/clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
print(polyinfo%>%filter(name==clipperName)) # shows clipper info. 
clipper_list<-readRDS(file=paste0(dir,"polygons/",clipperName,".rda"))
plot(clipper_list$clipper_proj, axes=T,  border="gray") #clipper
plot(clipper_list$clipperbuff_proj, add=T) #buffer

# read in bird metadata ---------------------------------------------------
meta<-read.table(paste0(dir,"supporttables/PTT_metadata.csv"),header=T, sep=",", strip.white=T, na.strings = "")
meta<-meta[meta$species==species,]

# reads in Frietas filtered tracking data for species
# get speed value used in argosfilter::sdafilter
load(file=paste0(dir,"species/",species,"/",species,"_trackfilter.RData"))
(speed<-unique(tf_info$vmax))
remove(tf_info); remove(filt_sum); remove(filt_error)

## #########################################################################
# Unique ID for dataset and grouping
unique(paste0(tracks_filt$ptt_deploy_id,"_",year(tracks_filt$utc)))
tracks_filt$uniID<-paste0(tracks_filt$ptt_deploy_id,"_",year(tracks_filt$utc))
length(unique(tracks_filt$uniID))

# #########################################################################
# Clips tracks to the polygon   -------------------------------------------
## adds a 1 or 0 for in or out of polygon, or buffer
tracks_inpoly<-in_poly(all_tracks=tracks_filt,
                                  CLIPPERS=clipper_list,
                                  dir.out=dir,
                                  prjtracks="+proj=longlat +ellps=WGS84 +datum=WGS84")
clipper.plots<-tracks_inpoly$Clipper.Plots

# Makes Quality Control plots for PolygonClip --------------------------
pdf(paste0(dir,"species/",species,"/QCplots_",species,"_",clipperName,".pdf"), onefile = TRUE)
for(i in 1:length(clipper.plots)){
  top.plot <-clipper.plots[[i]]
  grid.arrange(top.plot)
}
dev.off()


# Adds a time component to identify and label segments -------------------------------
# only returns segments inside the poly or buffer
tracks_inpoly_df<-tracks_inpoly$tracks.out

tracks_seg_df<-calc_leavetimesegs(hrs=72, tracks=tracks_inpoly_df, clipperName)
unique(tracks_seg_df$seg_id) #how many segments?
length(unique(tracks_seg_df$seg_id))

# Calculate Segment BrownianBridges ------------------------------------
cellsize<-3000
resolution="3km" # cell size in km, used in file names

segments<-bb_segmentation(tracks=tracks_seg_df, #tracking data
                            clipperName, #e.g. "PACSEA_buff33_coastclip", must match what you have used.
                            CLIPPERS=clipper_list, #output from: PolygonPrep_CCESTA with desired polygon
                            speed,
                            id.out = c("99999"), # to manually exclude birds or segments "99999" excludes none
                            sig2=cellsize,#the second smoothing parameter was 3000 m (the approximate mean error for PTT locations)
                            cellsize=cellsize,# related to error tags, in m
                            minNo=2,#minimum number of points to run for the bb - important with small clip areas where tracks are cut up when animal enters and leaves a box
                            proj4tracks="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 

saveRDS(segments,file=paste0(dir,"species/",species,"/",species,"_",clipperName,"_bb_1_segments.rda"))
segments<-readRDS(file=paste0(dir,"species/",species,"/",species,"_",clipperName,"_bb_1_segments.rda"))

bb<-segments$bb
bbvol<-segments$bbvol
tracksums.out<-segments$tracksums.out
contour<-segments$contour
tag <- names(bb)

### Makes Quality Control plots for IndividualBB --------------------------
pdf(paste0(dir,"species/",species,"/QCplots_",species,"_",clipperName,"_bb_1_segments.pdf"), onefile = TRUE)
  for (i in 1:length(tag)) {
    image(bb[[i]], useRasterImage=TRUE,col=c("light grey", topo.colors(40)))
  }
dev.off()

### Compiles Segments BB by Individuals and Groups ----------------------------------------

  #set grouping variable (year, season, month)
  #needs to be the same used for segmentation if individuals span groups
  
tracksums.out$grp<-lubridate::year(tracksums.out$date.begin)

bbindis<-bb_individuals(bb_probabilitydensity=bb, #Output from IndividualBB
                        tracksums.out,
                        cellsize=3000)  #the UD is multiplied by the cellsize^2 to make the individual ud = 1

saveRDS(bbindis,file=paste0(dir,"species/",species,"/",species,"_",clipperName,"_bb_2_individuals.rda"))
bbindis<-readRDS(file=paste0(dir,"species/",species,"/",species,"_",clipperName,"_bb_2_individuals.rda"))

#ignore directory errors if the directories already exist
bbindis
#bbindis is a list of estUDm for each grouping variable. Then each estUDm contains a list of estUDs 
#for each individual weighted by the tracking time inside the polygon

#sum individual densities by group weighted by the number of days for each individual / total days:
names(bbindis) #check groups

bbgroups<-bb_sumbygroup(bbindis,tracksums.out)

saveRDS(bbgroups,file=paste0(dir,"species/",species,"/",species,"_",clipperName,"_bb_3_groups.rda"))
bbgroups<-readRDS(file=paste0(dir,"species/",species,"/",species,"_",clipperName,"_bb_3_groups.rda"))



# Plot Rasters  ------------------------------------------------------------
sta_quickplot(bbgroups,
              clipper_list=clipper_list,
              dir,
              species)

#keeping this in the script for easy editing 
sta_quickplot<-function(bbgroups,
                        clipper_list=clipper_list,
                        dir,
                        species){

  
  require(ggplot2)
  states<-map_data('state') 
  states_sub<-states[states$region%in%c("washington","oregon","california","alaska"),]

  #get the clipper ready
  clipper_wgs84<-clipper_list$clipper
  (prjWant<-clipper_list$projWant)
  clipper_proj<-clipper_list$clipper_proj
  
  grp.ids<-names(bbgroups)

  for (h in 1:length(names(bbgroups))){
      # make the estUD into a raster
      a<-bbgroups[[h]]
      allgrps.indiv.raster<-raster(a$den_sum)
      
      # add projection to raster
      proj4string(allgrps.indiv.raster)<-CRS(prjWant)
      proj4string(allgrps.indiv.raster)

      #cellStats(allgrps.indiv.raster,'sum')
      
      # crop raster to clipper
      r2 <- crop(allgrps.indiv.raster, extent(clipper_proj))
      allgrps.indiv.raster.clip <- raster::mask(r2, clipper_proj)

      ## Check that it worked
      #plot(allgrps.indiv.raster.clip)
      #plot(clipper_proj, add=TRUE, lwd=2)

      # take clipped raster and reproject in WGS84 for viz
      allgrps.indiv.raster.clip.wgs84<-projectRaster(allgrps.indiv.raster.clip,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
      # make into points
      bb.df.wgs84 <- data.frame(rasterToPoints(allgrps.indiv.raster.clip.wgs84))
      colnames(bb.df.wgs84)<-c("Longitude","Latitude","Density")
      
      # take sqrt of Density to better highlight high use areas
      bb.df.wgs84$Density[bb.df.wgs84$Density==0]<-NA
      bb.df.wgs84$Density.n<-sqrt(bb.df.wgs84$Density)


A<-ggplot() +
  geom_raster(data=fortify(bb.df.wgs84)%>%dplyr::filter(Density.n>0.0001), aes(y=Latitude, x=Longitude,fill=Density.n)) +
  scale_fill_gradientn(colors=c("#3096C6", "#9EC29F","#F2EE75", "#EE5B2F","#E7292A"),name="sqrt(Density.n)") +
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
  geom_polygon(data=clipper_wgs84,aes(long,lat,group=group),fill="NA",color="black",size=.5)+
  theme_bw() +
  coord_equal() +
  coord_fixed(ratio=1.7,xlim = c(-126.5,-121),ylim=c(37,48))+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

B<-ggplot() +
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
  geom_path(data=tracks_filt%>%
              filter(year==grp.ids[[h]])%>%
              filter(keeps==1),
            aes(x=lon1,y=lat1,group=tag_id,color=as.factor(tag_id)),size=0.2)+
  geom_polygon(data=clipper_wgs84,aes(long,lat,group=group),fill="NA",color="black",size=.5)+
  theme_bw() +
  coord_equal() +
  coord_fixed(ratio=1.7,xlim = c(-126.5,-121),ylim=c(37,48))+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plotAB<-gridExtra::grid.arrange(A,B,ncol=2)
ggsave(plotAB,filename = paste0(dir,"species/",species,"/",grp.ids[h],"_",species,"_",clipper_list$clipperName,".png"),width=10,dpi = 300)

  }
}
