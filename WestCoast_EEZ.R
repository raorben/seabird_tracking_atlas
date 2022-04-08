library(sf)
library(dplyr)
require(rgdal)


nc <- st_read(dsn = "/Users/rachaelorben/Downloads/EEZ_Pacific_byState_Coast_UTM10/EEZ_Pacific_byState_Coast_UTM10.shp")%>%
  st_geometry()
USs<-st_simplify(nc, dTolerance = 1000) #~6nm smoother / ~11km
plot(USs)
CA<-USs[1]
US<-st_union(USs)
US.wgs84<-st_transform(US,crs = 4326)
plot(US.wgs84)
saveRDS(US.wgs84, paste0("/Users/rachaelorben/Research/SeabirdTrackingAtlas","/polygons/WestCoastEEZ_sf.rda"))


CA.wgs84<-st_transform(CA,crs = 4326)

plot(CA.wgs84)
saveRDS(CA.wgs84, paste0("/Users/rachaelorben/Research/SeabirdTrackingAtlas","/polygons/CA_EEZ_sf.rda"))

## NEXT RUN MakeClippers.R