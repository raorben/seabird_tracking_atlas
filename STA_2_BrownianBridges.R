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


sp="BFAL" #
sp="COMU" #run Sept19 all years, looks good
sp="PFSH" #run Sept19 all years, looks good
sp="SOSH" #run Sept19 all years, looks good
sp="STAL" #need to remake datafiles
sp="BRAC" #TODO: get & compile GPS data 2014
sp="WEGU" #TODO: add to metadata
sp="RTLO" #run Sept2020, looks good
sp="PALO" #run Sept2020, looks good
sp="NOFU" #run Sept19 all years, looks good

##grouping used to make brownian bridges
timegrp<-"all" #"year", "all", "season"

# set directories
if(Sys.info()[7]=="rachaelorben") {dir<-"/Users/rachaelorben/Research/SeabirdTrackingAtlas/"} ##RAO
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

files.sources = list.files(paste0(gitdir,"R"), full.names = TRUE)
sapply(X = files.sources, FUN=source)

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
meta<-read.table(paste0(dir,"supporttables/STA_metadata_2020-10-19_908birds.csv"),header=T, sep=",", strip.white=T, na.strings = "")
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

saveRDS(tracks_inpoly.df,file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_0_tracks_inpoly.df.rda"))
tracks_inpoly.df<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_0_tracks_inpoly.df.rda"))


# Makes Quality Control plots for PolygonClip --------------------------
pdf(paste0(dir,"species/",sp,"/QCplots_",sp,"_",clipperName,"_",timegrp,".pdf"), onefile = TRUE)
for(i in 1:length(clipper.plots)){
  top.plot <-clipper.plots[[i]]
  grid.arrange(top.plot)
}
dev.off()


# Adds a time component to identify and label segments -------------------------------
# only returns segments inside the poly or buffer
tracks_inpoly.df<-tracks_inpoly$tracks.out

tracks_seg_df<-calc_leavetimesegs(hrs=72, 
                                  tracks=tracks_inpoly.df, 
                                  clipperName)
unique(tracks_seg_df$seg_id) #how many segments?
length(unique(tracks_seg_df$seg_id))
b<-tracks_seg_df%>%group_by(seg_id)%>%summarise(n=n())
summary(tracks_seg_df)

# Calculate Segment BrownianBridges ------------------------------------
cellsize<-3000
resolution="3km" # cell size in km, used in file names
mino=3 # IMPORTANT to be small for PTT datasets, use 10+ for GPS data
segments<-bb_segmentation(tracks=tracks_seg_df, #tracking data
                            clipperName, #e.g. "PACSEA_buff33_coastclip", must match what you have used.
                            CLIPPERS=clipper_list, #output from: PolygonPrep_CCESTA with desired polygon
                            speed,
                            id.out = c("99999"), # to manually exclude birds or segments "99999" excludes none
                            sig2=cellsize,#the second smoothing parameter was 3000 m (the approximate mean error for PTT locations)
                            cellsize=cellsize,# related to error tags, in m
                            minNo=minNo,#minimum number of points to run for the bb - important with small clip areas where tracks are cut up when animal enters and leaves a box
                            proj4tracks="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 

saveRDS(segments,file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_1_segments.rda"))
segments<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_1_segments.rda"))

bb<-segments$bb
bbvol<-segments$bbvol
tracksums.out<-segments$tracksums.out
contour<-segments$contour
tag <- names(bb)

names(segments)
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
tracksums.out$timegrp<-"all"
#season
#tracksums.out$timegrp<-NA
#tracksums.out$timegrp<-sapply(strsplit(tracksums.out$uniID, split='_', fixed=TRUE), function(x) (x[3]))


bbindis<-bb_individuals(bb_probabilitydensity=bb, #Output from IndividualBB
                        tracksums=tracksums.out,
                        cellsize=3000)  #the UD is multiplied by the cellsize^2 to make the individual ud = 1

saveRDS(bbindis,file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_2_individuals.rda"))
bbindis<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_2_individuals.rda"))

#ignore directory errors if the directories already exist
bbindis
#bbindis is a list of estUDm for each grouping variable. 
#Then each estUDm contains a list of estUDs 
#for each individual weighted by the tracking time inside the polygon

#sum individual densities by group weighted by the number of days for 
#each individual / total days:
names(bbindis) #check groups
summary(bbindis)

bbgroups<-bb_sumbygroup(bbindis,
                        tracksums.out)

saveRDS(bbgroups,file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_3_groups.rda"))
bbgroups<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_3_groups.rda"))


# Plot Rasters  ------------------------------------------------------------
#year
#tracks_inpoly.df$timegrp<-lubridate::year(tracks_inpoly.df$utc)
#all
tracks_inpoly.df$timegrp<-"all"
#season
#tracks_inpoly.df$timegrp<-NA
#tracks_inpoly.df$timegrp<-sapply(strsplit(tracks_inpoly.df$uniID, split='_', fixed=TRUE), function(x) (x[3]))

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
bathy2$lon<-unwrap360(bathy2$V1)

#makes vizulization plots
sta_quickplot(bbgroups,
              clipper_list=clipper_list,
              dir,
              species=sp,
              tracks_inpoly.df,
              bathy2)

#saves as .asc files this one is squareroottransflormed
sta_saveraster_sqrt(bbgroups,
               clipper_list=clipper_list,
               dir,
               species=sp)

#saves as .asc files
sta_saveraster(bbgroups,
                    clipper_list=clipper_list,
                    dir,
                    species=sp)
