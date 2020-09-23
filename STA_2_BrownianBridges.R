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


sp="BFAL" #TODO: need to add old data + Shaffer to meta, change file names
sp="COMU" #run Sept19 all years, looks good
sp="PFSH" #run Sept19 all years, looks good
sp="SOSH" #run Sept19 all years, looks good
sp="STAL" #need to remake datafiles
sp="BRAC" #TODO: get & compile GPS data 2014-2019
sp="WEGU" #TODO: add to metadata
sp="RTLO" #run Sept2020
sp="PALO" #run Sept2020, looks good
sp="NOFU" #run Sept19 all years, looks good

##grouping used to make brownian bridges
timegrp<-"season" #"year", "all"

# set directories
if(Sys.info()[7]=="rachaelorben") {dir<-"/Users/rachaelorben/Research/SeabirdTrackingAtlas/"} ##RAO
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

source(paste0(gitdir,"STA_Functions.R"))

# A time grouping variable needs to be decided on and modifed for this script to work. 
# Right now only one nesting of this variable is allowed so if you want a summary of all tracks
# give a time variable of "all"

# Read in Clipper ---------------------------------------------------------
clipperName<-"PNW_wUSEEZ"
clipperName<-"Oregon_wUSEEZ"

# read in list of all potential clipper files
# polys are stored as WGS84 and then projected in MakeClippers.R
# this reads in the polygon info, the processed Clipper and plots it.

polyinfo<-read.csv (paste(gitdir,"supporttables/clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
print(polyinfo%>%filter(name==clipperName)) # shows clipper info. 
clipper_list<-readRDS(file=paste0(dir,"polygons/",clipperName,".rda"))
plot(clipper_list$clipper_proj, axes=T,  border="gray") #clipper
plot(clipper_list$clipperbuff_proj, add=T) #buffer

# read in bird metadata ---------------------------------------------------
meta<-read.table(paste0(dir,"supporttables/STA_metadata_2019-11-16_815birds.csv"),header=T, sep=",", strip.white=T, na.strings = "")
meta<-meta[meta$species==sp,]
  meta%>%filter(loc_data==1)%>%group_by(species,deploy_year, deploy_site)%>%summarise(n=n())

# reads in Frietas filtered tracking data for species
# get speed value used in argosfilter::sdafilter
load(file=paste0(dir,"species/",sp,"/",sp,"_trackfilter.RData"))
(speed<-unique(tf_info$vmax))
remove(tf_info); remove(filt_sum); remove(filt_error)

## #########################################################################
#set UniID with time group variable
tracks_filt_grp<-timegrp_apply(tracks_filt,timegrp)

frags<-tracks_filt_grp%>%group_by(uniID)%>%summarise(n=n())%>%filter(n<3)
tracks_filt_grp<-tracks_filt_grp%>%filter(!uniID %in% frags$uniID)

# #########################################################################
# Clips tracks to the polygon   -------------------------------------------
## adds a 1 or 0 for in or out of polygon, or buffer
tracks_inpoly<-in_poly(all_tracks=tracks_filt_grp,
                                  CLIPPERS=clipper_list,
                                  dir.out=dir,
                                  prjtracks="+proj=longlat +ellps=WGS84 +datum=WGS84")
clipper.plots<-tracks_inpoly$Clipper.Plots
tracks_inpoly.df<-tracks_inpoly$tracks.out #all locations w/ in-out poly

# Makes Quality Control plots for PolygonClip --------------------------
pdf(paste0(dir,"species/",sp,"/QCplots_",sp,"_",clipperName,"_",timegrp,".pdf"), onefile = TRUE)
for(i in 1:length(clipper.plots)){
  top.plot <-clipper.plots[[i]]
  grid.arrange(top.plot)
}
dev.off()


# Adds a time component to identify and label segments -------------------------------
# only returns segments inside the poly or buffer
tracks_inpoly_df<-tracks_inpoly$tracks.out

tracks_seg_df<-calc_leavetimesegs(hrs=72, 
                                  tracks=tracks_inpoly_df, 
                                  clipperName)
unique(tracks_seg_df$seg_id) #how many segments?
length(unique(tracks_seg_df$seg_id))

summary(tracks_seg_df)

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

saveRDS(segments,file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_1_segments.rda"))
segments<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_1_segments.rda"))

bb<-segments$bb
bbvol<-segments$bbvol
tracksums.out<-segments$tracksums.out
contour<-segments$contour
tag <- names(bb)

image(bb)
plot(getverticeshr(bb, 70), add=TRUE, lwd=2)

### Makes Quality Control plots for IndividualBB --------------------------
pdf(paste0(dir,"species/",sp,"/QCplots_",sp,"_",clipperName,"_",timegrp,"_bb_1_segments.pdf"), onefile = TRUE)
  for (i in 1:length(tag)) {
    image(bb[[i]], useRasterImage=TRUE,col=c("light grey", topo.colors(40)))
  }
dev.off()

### Compiles Segments BB by Individuals and Groups ----------------------------------------
#set time grouping variable (year, season, month)
#needs to be the same used for UniID segmentation if individuals span groups

#year:  
#tracksums.out$timegrp<-lubridate::year(tracksums.out$date.begin)
#all:
#tracksums.out$timegrp<-"all"
#season
tracksums.out$timegrp<-NA
tracksums.out$timegrp<-sapply(strsplit(tracksums.out$uniID, split='_', fixed=TRUE), function(x) (x[3]))


bbindis<-bb_individuals(bb_probabilitydensity=bb, #Output from IndividualBB
                        tracksums=tracksums.out,
                        cellsize=3000)  #the UD is multiplied by the cellsize^2 to make the individual ud = 1

saveRDS(bbindis,file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_bb_2_individuals_",timegrp,"_.rda"))
bbindis<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_bb_2_individuals_",timegrp,"_.rda"))

#ignore directory errors if the directories already exist
bbindis
#bbindis is a list of estUDm for each grouping variable. 
#Then each estUDm contains a list of estUDs 
#for each individual weighted by the tracking time inside the polygon

#sum individual densities by group weighted by the number of days for 
#each individual / total days:
names(bbindis) #check groups

bbgroups<-bb_sumbygroup(bbindis,
                        tracksums.out)

class(bbindis)<-"estUD"
match.arg(bbgroups)
getverticeshr(bbgroups, standardize=TRUE,percent = 95)
summary(bbgroups)

saveRDS(bbgroups,file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"bb_3_groups.rda"))
bbgroups<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"bb_3_groups.rda"))

# getvolumeUD(bbindis)
# getverticeshr(bbvol, percent=25, standardize=TRUE)
# a<-getverticeshr(bbindis[[1]], percent=25,  standardize = TRUE)
# plot(a)

# Plot Rasters  ------------------------------------------------------------


#year
#tracks_inpoly.df$timegrp<-lubridate::year(tracks_inpoly.df$utc)
#all
#tracks_inpoly.df$timegrp<-"all"
#season
tracks_inpoly.df$timegrp<-NA
tracks_inpoly.df$timegrp<-sapply(strsplit(tracks_inpoly.df$uniID, split='_', fixed=TRUE), function(x) (x[3]))

#summary of data inside polygon
idsinpoly<-unique(tracksums.out$uniID)

#need to match ids with metadata table
(ids<-vapply(strsplit(idsinpoly, "_", fixed = TRUE), "[", "", 1))
meta$tag_id

meta%>%dplyr::filter(species==sp)%>%filter(STA_id %in% ids)%>%
  group_by(deploy_year,deploy_site,collab1_point_contact_name)%>%
  dplyr::summarize(n_birds=n_distinct(tag_id))

userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'
bathy2<-readRDS(paste0(userdir,"/Analysis/compileddata/Bathymetryforggplot.rda"))

unwrap360<-function(lon360){
  lon<-ifelse(lon360 > 180, -360 + lon360, lon360)
  return(lon)}
bathy2$lon<-unwrap360(bathy2$V1)

#keeping this in the script for easy editing 
sta_quickplot<-function(bbgroups,
                        clipper_list=clipper_list,
                        dir,
                        species,
                        tracks_inpoly.df){

  
  require(ggplot2)
  require(cowplot)
  require(gridExtra)
  library(RColorBrewer)
  states<-map_data('state') 
  states_sub<-states[states$region%in%c("washington","oregon","california","alaska"),]
  w2hr<-map_data('world')
  w2hr_sub<-w2hr[w2hr$region%in%c("Canada"),]
  
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
  scale_fill_gradientn(colors=c("#3096C6", "#9EC29F","#F2EE75", "#EE5B2F","#E7292A"),name="Density") +
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
  geom_polygon(data=clipper_wgs84,aes(long,lat,group=group),fill="NA",color="black",size=.5)+
  theme_bw() +
  xlab("Longitude")+
  ylab("Latitude")+
  coord_fixed(ratio=1.7,xlim = c(-130,-121),ylim=c(39,48.3))+
  #guides(fill = guide_colorbar(barwidth = 2, barheight = 10,direction = "vertical"))+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position=c(.84,.16),
        #panel.background=element_rect(fill="transparent",colour=NA),
        legend.background = element_rect(fill = "transparent",colour = "transparent"),
        #legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text=element_text(color="white", size=8),
        legend.title = element_text(color="white"))

colourCount = nrow(unique(tracks_filt%>%dplyr::select(STA_id)))+1
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

B<-ggplot() +
  geom_tile(data=bathy2,aes(x=wrap360(lon),y=V2,fill=Depth))+
  scale_fill_gradientn(colours = c("grey95", "grey25"),name="Depth") +
  geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="black",color="grey60",size=0.1)+
  geom_polygon(data=states_sub,aes(wrap360(long),lat,group=group),fill="black",color="grey60",size=0.1)+
  geom_path(data=tracks_inpoly.df%>%filter(timegrp==names(bbgroups)[h]),
            aes(x=wrap360(lon1),y=lat1,group=STA_id,color=as.factor(STA_id)),size=0.2)+
  scale_color_manual(values = getPalette(colourCount))+
  geom_polygon(data=clipper_wgs84,aes(wrap360(long),lat,group=group),fill="NA",color="black",size=.5)+
  annotate("text",x=wrap360(-129.8),y=48,label=sp,color="white",size=10,hjust = 0)+
  annotate("text",x=wrap360(-123.7),y=42.8,label="Bird sample size",color="white",size=5,hjust = 0)+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_fixed(ratio=1.7,xlim = c(wrap360(-130),wrap360(-121)), ylim=c(39,48.3))+
  scale_x_continuous(breaks=c(230,232.5,235,237.5),
                     labels=c("-130.0", "-127.5", "-125.0","-122.5"))+
  theme_bw() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

tracks_inpoly.df$month<-month(tracks_inpoly.df$utc)
tracks_inpoly.df$date<-date(tracks_inpoly.df$utc)

birdsMO<-unique(tracks_inpoly.df%>%dplyr::select("uniID","timegrp","in_poly","date","month"))
birdsMO.dys<-tracks_inpoly.df%>%group_by(uniID, timegrp,in_poly,month)%>%
  summarise(n=n_distinct(date))
birdsMO.dys1<-birdsMO.dys%>%
  group_by(in_poly,timegrp,month)%>%
  summarise(mean=mean(n),sd=sd(n))

sample.size.inset<-ggplot()+
  geom_bar(data=unique(birdsMO%>%filter(timegrp==names(bbgroups)[h])%>%
                         dplyr::select("uniID","month","in_poly")), 
           aes(x=month, group=in_poly,fill=as.factor(in_poly)))+
  scale_fill_manual(values = c("grey80","lightblue"))+
  geom_point(data=birdsMO.dys1%>%filter(timegrp==names(bbgroups)[h]), 
             aes(y=mean, x=month), color="black", size=.4)+
  geom_errorbar(data=birdsMO.dys1%>%filter(timegrp==names(bbgroups)[h]),
                aes(x=month, ymin=mean-sd, ymax=mean+sd), 
                width=.3,position=position_dodge(.9))+
  scale_x_continuous(limits =c(1,12),breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  theme_classic()+
  theme(axis.title=element_blank(),
        axis.text = element_text(color="white", size=6),
        axis.ticks = element_line(color="white"),
        axis.line = element_line(color="white"),
        plot.background = element_blank(),#odd box around plots
        panel.background = element_rect(fill = "grey30"),
        #plot.background = element_rect(color="transparent",fill="grey50"),
        legend.position = "none",
        #panel.background = element_rect(fill = "transparent"),
        strip.background = element_blank())+
  facet_wrap(~in_poly,ncol = 1, scales="free_y")

B.with.inset <-
  ggdraw() +
  draw_plot(B) +
  draw_plot(sample.size.inset, x = 0.66, y = .15, width = .3, height = .3)

plotAB<-gridExtra::grid.arrange(B.with.inset ,A,ncol=2)
ggsave(plotAB,filename = paste0(dir,"species/",sp,"/",grp.ids[h],"_",sp,"_",clipper_list$clipperName,".png"),width=12,height=10, dpi = 300)
saveRDS(plotAB,paste0(dir,"species/",sp,"/",grp.ids[h],"_",sp,"_",clipper_list$clipperName,".rds"))

  }
}


sta_quickplot(bbgroups,
              clipper_list=clipper_list,
              dir,
              species=sp,
              tracks_inpoly.df)
