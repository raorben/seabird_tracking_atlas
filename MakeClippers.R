library(adehabitatHR)
library(SDMTools)
library(raster)
library(ggplot2)
library(gridExtra)

rm(list=ls())

# set directories
if(Sys.info()[7]=="rachaelorben") {dir<-"/Users/rachaelorben/Research/SeabirdTrackingAtlas/"} ##RAO
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

# read in STA wrapper functions
source(paste0(gitdir,"STA_Functions.R"))

# read in list of potential clipper files, polys are stored as WGS84 and then projected by PolygonPrep_CCESTA
clipPolyList<-read.csv (paste(gitdir,"supporttables/clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
rowNum=30
print(clipPolyList[rowNum,]) # show a list of the clipper files
clipperName<-as.character(clipPolyList$name[rowNum])

# Pulls in requested polygon  --------------------------------------------
#and associated projection data, projects, calculates buff, return polygons
CLIPPERS<-polygrid_prep(rno=rowNum,
                      clipPolyList=clipPolyList, 
                      dir=dir,
                      plot="on",
                      bufferkm=33.6,
                      cellsize=3000)
#sf version
CLIPPERS<-polygrid_forsf_prep(clipperName=clipperName,
                              clipperfileName=paste0(clipperName,"_sf.rda"),
                              rno=rowNum,
                              clipPolyList=clipPolyList, 
                              dir=dir,
                              plot="on",
                              bufferkm=33.6,
                              cellsize=3000)

clipperName<-CLIPPERS$clipperName
rast<-CLIPPERS$rast
saveRDS(object=CLIPPERS,file=paste0(dir,"polygons/",clipperName,".rda"))
CLIPPERS<-readRDS(file=paste0(dir,"polygons/",clipperName,".rda"))

