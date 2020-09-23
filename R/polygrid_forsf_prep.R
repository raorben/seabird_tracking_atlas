polygrid_forsf_prep<-function(clipperName=CN,
                              clipperfileName=paste0(CN,"_sf.rda"),
                              rno=27,
                              clipPolyList=clipPolyList, 
                              dir=dir,
                              plot="on",
                              bufferkm=33.6,
                              cellsize=3000){
  
  require(sp)
  require(maptools)
  require(raster)
  require(rgdal)
  require(rgeos)
  require(stringr)
  require(sf)
  
  # read in polygon
  clipper <- readRDS(paste0(dir,"polygons/",clipperfileName)) # clipper comes in as unprojected WGS84
  clipper <- as(clipper, 'Spatial')
  # read in projection best for selected polygon (contained in table clipPolyList)
  projWant<-paste("+",as.character(clipPolyList$Proj4.specs[rno]),sep="")
  
  
  # transform spatial polygons to projection appropriate for region in question (California Current in this case)
  clipper_proj<-spTransform(clipper, CRS(projWant))
  
  #### ui enter buffer distance
  buffDist<-bufferkm*1000 # convert km to m
  
  # create buffer
  clipperBuff_proj<-gBuffer(clipper_proj, byid=F, id=NULL, width=buffDist, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0)
  
  ## plot shape polygon for clipping
  if (plot=="on") {
    p1<- plot(clipper_proj, axes=T,  border="gray")
    p1<-p1 + plot(clipperBuff_proj, add=T)
  }
  
  # create a buffer for the grid for the kernal densities
  buff_kd_Dist<-200*1000 # convert km to m
  clipperBuff_projKD<-gBuffer(clipper_proj, byid=F, id=NULL, width=buff_kd_Dist, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0)
  
  #### get extent of clipper buffer and make grid for KD analysis
  ext<-extent(clipperBuff_projKD[1,1])
  grid.lon <- c(ext@xmin, ext@xmin, ext@xmax, ext@xmax)
  grid.lat <- c(ext@ymax, ext@ymin, ext@ymin, ext@ymax)
  grid.loc <- SpatialPoints(cbind(grid.lon, grid.lat))
  rast <- ascgen(grid.loc, cellsize=cellsize)
  
  plot(rast)  
  plot(clipper_proj, col="white",add=T)
  plot(clipperBuff_proj, add=T, border="gray")
  plot(ext, col="green",add=T)
  
  CLIPPERS<-list(clipper,clipper_proj,clipperBuff_proj,projWant,clipperName,rast)
  names(CLIPPERS)<-c("clipper","clipper_proj","clipperbuff_proj","projWant","clipperName","rast")
  return(CLIPPERS)
}
