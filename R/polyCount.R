## polyCount  ######################################################################################################

## Phil Taylor & Mark Miller, 2012

## polyCount calculates the number of overlapping Polygons. The calculation is
## done by overlapping the polygons within a grid and counting the number
## falling within each gridcell. The Res parameter sets the size of the grid
## (in decimal degrees) to be used. Smaller grids will allow for more detailed
## counts but will slow computing time. The function returns a raster object with
## values showing the proportion of Polys (i.e. nPolys/total nPolys) overlapping
## each cell. The raster extent is set at the bounding limits of the Polys or, when
## data crosses the dateline, set to the northern and southern most limits of the
## Polys but longitudinally crossing the circumference of the world.

## Polys must be a SpatialPolygonsDataFrame of the polygons to be counted.
## Res must be a numeric object indicating the resolution in decimal degrees.

## Steffen Oppel revision on 14 Dec 2016 - removed + (Res * 100) from NCol in L. 664
## Steffen Oppel revision on 16 Dec 2016: fixed the point overlay problem by using a proper grid instead
## Steffen Oppel revision on 27 Dec 206: ensured that output was projected in WGS84

## version 1.2    05-04-2012

polyCount <- function(Polys, Res = 0.1)
{
  
  require(raster)
  require(maps)
  
  if(!class(Polys) %in% c("SpatialPolygonsDataFrame", "SpatialPolygons")) stop("Polys must be a SpatialPolygonsDataFrame")
  if(is.na(projection(Polys))) stop("Polys must be projected")
  
  
  Poly.Spdf <- spTransform(Polys, CRS=CRS("+proj=longlat +ellps=WGS84"))
  DgProj <- Polys@proj4string
  
  DateLine <- Poly.Spdf@bbox[1,1] < -178 & Poly.Spdf@bbox[1,2] > 178
  if(DateLine == TRUE) {print("Data crosses DateLine")}
  
  UDbbox <- bbox(Poly.Spdf)
  if(DateLine == TRUE)  {UDbbox[1,] <- c(-180,180)}
  BL <- floor(UDbbox[,1])                 # + (Res/2) - removed on 16 Dec 2016 because it results in some polygons outside the grid
  TR <- ceiling(UDbbox[,2])
  NRow <- ceiling(sqrt((BL[1] - TR[1])^2)/Res)
  NCol <- ceiling(sqrt((BL[2] - TR[2])^2)/Res) #+ (Res * 100)				### THIS LINE CAUSES PROBLEMS BECAUSE IT GENERATES LATITUDES >90 which will cause spTransform to fail
  Grid <- GridTopology(BL, c(Res,Res), c(NRow, NCol))
  newgrid<-SpatialGrid(Grid, proj4string = CRS("+proj=longlat + datum=wgs84"))
  spol <- as(newgrid, "SpatialPolygons")								### this seems to create an orphaned hole
  SpGridProj <- spTransform(spol, CRS=DgProj)
  GridIntersects <- over(SpGridProj, Polys)
  SpGridProj<- SpatialPolygonsDataFrame(SpGridProj, 
                                        data = data.frame(ID=GridIntersects$id, row.names=sapply(SpGridProj@polygons,function(x) x@ID)))
  SpGridProj <- subset(SpGridProj, !is.na(SpGridProj@data$ID))
  #SpGrid <- SpatialPoints(Grid, proj4string = CRS("+proj=longlat + datum=wgs84"))
  #SpdfGrid <- SpatialPointsDataFrame(SpGrid, data.frame(Longitude=SpGrid@coords[,1], Latitude=SpGrid@coords[,2]))
  #SpGridProj <- spTransform(SpdfGrid, CRS=DgProj)
  #GridIntersects <- over(SpGridProj, Polys)
  #SpGridProj@data$Intersects$ID <- GridIntersects$ID
  #SpGridProj <- subset(SpGridProj, !is.na(SpGridProj@data$Intersects$ID))   ### SpGridProj[!is.na(SpGridProj@data$Intersects$ID),] 			###
  plot(SpGridProj)
  
  Count <- 0
  for(i in 1:length(Polys))
  {
    TempB <- Polys[i,]
    Temp <- over(SpGridProj, TempB)[,1]     ### inserted based on Matthew Carroll's advice; MAY NEED TO SWAP arguments in 'over'?
    Temp[is.na(Temp)] <- 0
    Temp[Temp > 0] <- 1
    Temp <- as.numeric(Temp)
    Count <- Count + Temp
    #Prop <- Count/i                        ### removed to improve efficiency
  }
  Prop <- Count#/length(Polys)     ### removed from loop over polys as it only needs to be calculated once
  #GridIntersects$inside<-as.numeric(as.character(GridIntersects$ID))    ### this only works for numeric trip_id!!
  #GridIntersects$Prop <- 0
  #GridIntersects$Prop[!is.na(GridIntersects$inside)] <- Prop    #[,1] removed based on Matthew Carroll's advice, because fixed in L. 678
  SpGridProj@data$Prop <- Prop
  SpGridOUT <- spTransform(SpGridProj, CRS=CRS("+proj=longlat +ellps=WGS84"))   ### show output in WGS84
  SGExtent <- extent(SpGridOUT)
  RT <- raster(SGExtent, ncols=as.double(NCol), nrows=as.double(NRow))
  WgsRas <- (rasterize(x=SpGridOUT,y=RT, field = "Prop"))
  
  plot(WgsRas, asp=1)
  maps::map("world", add=T, fill=T, col="darkolivegreen3")      ## to avoid conflict with purrr
  projection(WgsRas) <- CRS("+proj=longlat + datum=wgs84")
  return(WgsRas)
}
