library(sf)
library(dplyr)

#makes a polygon for the West Coast Region: Cape Mendicino north to limit of WC EEZ.

#http://www.marineregions.org/downloads.php World EEZ v10 (2018-02-21, 119 MB) 
nc <- st_read(dsn = "/Users/rachaelorben/Dropbox/Envrion_Vars/World_EEZ_v10_20180221/eez_v10.shp")
names(nc)
unique(nc$GeoName)
st_crs(nc)
nc_df<-data.frame(nc)
USs<-nc%>%dplyr::filter(GeoName=="United States Exclusive Economic Zone")
plot(US)

nc_360<-nc%>%st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
#USs<-st_simplify(US, dTolerance = .05) #~6nm smoother / ~11km

##make bounding box for tracking data
x_coord <- c(135, -120, -120, 135, 135)
y_coord <- c(20, 20, 65, 65,20)
polygon <-  cbind(x_coord, y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326, check_ring_dir = TRUE) %>%
  st_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

#removed most of the Stright of Juan de Fuca etc.
x_coord <- c(-123.9,-122, -122,-124.1, -123.9)
y_coord <- c(44.793749, 44.793749,50, 50,  44.793749)
polygon_out <-  cbind(x_coord, y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326, check_ring_dir = TRUE) %>%
  st_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))


x_coord <- c(-124.65, -123, -123, -124.6, -124.7)
y_coord <- c( 48.385729, 47.385729, 49, 49,48.385729)
polygon_tipofWA <-  cbind(x_coord, y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326, check_ring_dir = TRUE) %>%
  st_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

x_coord <- c(-124.133097,-123, -123, -124.135159, -124.135159)
y_coord <- c(42, 42,44.019124, 44.019124, 42)
polygon_FloranceOR <-  cbind(x_coord, y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326, check_ring_dir = TRUE) %>%
  st_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))


x_coord <- c(-130,-100, -100, -130, -130)
y_coord <- c(30,30,40.440132, 40.440132,30)
polygon_NCC <-  cbind(x_coord, y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326, check_ring_dir = TRUE) %>%
  st_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

selIDX<-st_intersects(USs, polygon, sparse = FALSE) #finds which eezs are inside the box
nc_360_NP<-USs%>%dplyr::filter(selIDX==TRUE) #selects the eezs in the box
nc_360_NP<-st_buffer(nc_360_NP, 0.0)#works!!!! needed to make the st_intersection code work - not the ideal fix
st_is_valid(nc_360_NP) #needs to be all TRUE

nc_360_NP_crop<-st_intersection(nc_360_NP, polygon) #cuts the eezs inside the box to the box

WC_eez<-st_difference(nc_360_NP_crop, polygon_out) #cuts the eezs inside the box to the box
WC_eez<-st_difference(WC_eez, polygon_tipofWA) #cuts the eezs inside the box to the box
WC_eez<-st_difference(WC_eez, polygon_FloranceOR) #cuts the eezs inside the box to the box

WC_eez<-st_difference(WC_eez, polygon_NCC) #cuts the eezs inside the box to the box
plot(st_geometry(WC_eez))


#this pulls out the biggest polygon - all the others are little and are probably islands
a<-WC_eez4$geometry[[1]][[2]][[1]] 
colnames(a)<-c("lon","lat")
a<-data.frame(a)
x_coord <- a$lon
y_coord <- a$lat


WC_eez <-  cbind(x_coord,y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326, check_ring_dir = TRUE) %>%
  st_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
plot(st_geometry(WC_eez))


x_coord <- c(-124.306290,-121, -121, -124.306290, -124.306290)
y_coord <- c(  40, 40,   40.650634, 40.650634, 40)
polygon_riverFortuna <-  cbind(x_coord, y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326, check_ring_dir = TRUE) %>%
  st_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

x_coord <- c(-124.073601,-121, -121, -124.073601, -124.073601)
y_coord <- c (41, 41, 41.551927, 41.551927, 41)
polygon_riverKalamath <-  cbind(x_coord, y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326, check_ring_dir = TRUE) %>%
  st_sf() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

WC_eez<-st_difference(WC_eez, polygon_riverFortuna) #cuts the eezs inside the box to the box
WC_eez<-st_difference(WC_eez, polygon_riverKalamath) #cuts the eezs inside the box to the box

plot(st_geometry(WC_eez))

#WC_eez6<-st_buffer(WC_eez5,dist = .1)
#plot(st_geometry(WC_eez6))
