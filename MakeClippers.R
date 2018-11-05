library(adehabitatHR)
library(SDMTools)
library(raster)
library(ggplot2)
library(gridExtra)

rm(list=ls())

# set directories
if(Sys.info()[7]=="rachaelorben") {dir<-"/Volumes/GoogleDrive/My Drive/Seabird_Oceanography_Lab/Oregon_coast_tracking/Analysis/CCESTA/"} ##RAO
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

# read in STA wrapper functions
source(paste0(gitdir,"STA_Functions.R"))

# read in list of potential clipper files, polys are stored as WGS84 and then projected by PolygonPrep_CCESTA
clipPolyList<-read.csv (paste(dir,"supporttables/clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
print(clipPolyList[26,]) # show a list of the clipper files
clipperName<-as.character(clipPolyList$name[26])

# Pulls in requested polygon  --------------------------------------------
#and associated projection data, projects, calculates buff, return polygons
CLIPPERS<-PolygonPrep(rno=26,
                      clipPolyList=clipPolyList, 
                      dir=dir,
                      plot="on",
                      bufferkm=33.6,
                      cellsize=3000)

clipperName<-CLIPPERS[[5]]
saveRDS(object=CLIPPERS,file=paste0(dir,"polygons/",clipperName,".rda"))
CLIPPERS<-readRDS(file=paste0(dir,"polygons/",clipperName,".rda"))

