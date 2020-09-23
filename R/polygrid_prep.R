polygrid_prep<-function(rno,
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
  
  
  #### select clipperfile from clipPolyList
  clipperN<-as.character(clipPolyList$clipFileName[rno])
  clipperName<-as.character(clipPolyList$name[rno])
  
  #### dir.in.poly in polygon .shp file for clipping
  dir.in.poly <- paste0(dir,"polygons/",clipperN)
  
  # read in polygon
  clipper <- readOGR(dir.in.poly,clipperN) # clipper comes in as unprojected WGS84
  # read in projection best for selected polygon (contained in table clipPolyList)
  projWant<-paste("+",as.character(clipPolyList$Proj4.specs[rno]),sep="")
  
  # if polygon multipart, select polygon
  #if (clipPolyList$mult_polygons[rno]==1) {
  #  clipper<-subset(clipper,(clipper$LME_NAME)==as.character(clipPolyList$poly_want[rno]))
  #}
  
  #if (clipPolyList$mult_polygons[rno]==1) {
  #  clipper<-clipper[clipper@data$Territory1 == "Alaska", ]
  #}
  
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
  
  #### get extent of clipper buffer and make grid for KD analysis
  ext<-extent(clipperBuff_proj[1,1])
  grid.lon <- c(ext@xmin, ext@xmin, ext@xmax, ext@xmax)
  grid.lat <- c(ext@ymax, ext@ymin, ext@ymin, ext@ymax)
  grid.loc <- SpatialPoints(cbind(grid.lon, grid.lat))
  rast <- ascgen(grid.loc, cellsize=cellsize)
  
  #plot(rast)  
  #plot(clipper_proj, col="white",add=T)
  #plot(clipperBuff_proj, add=T, border="gray")
  #plot(ext, col="green",add=T)
  
  CLIPPERS<-list(clipper,clipper_proj,clipperBuff_proj,projWant,clipperName,rast)
  names(CLIPPERS)<-c("clipper","clipper_proj","clipperbuff_proj","projWant","clipperName","rast")
  return(CLIPPERS)
}