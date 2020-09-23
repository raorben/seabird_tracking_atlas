bb_segmentation<-function(tracks, #tracking data with a unique id for each bird:time and a segment id
                          clipperName, #e.g. "PACSEA_buff33_coastclip", must match what you have used.
                          CLIPPERS, #output from: PolygonPrep_CCESTA with desired polygon
                          speed,
                          id.out = c("99999"), # to manually exclude birds or segments "99999" excludes none
                          sig2=3000,#, the second smoothing parameter was 3000 m (the approximate mean error for PTT locations)
                          cellsize=3000,#(user option)
                          minNo=2,#minimum number of point to run for the bb - important with small clip areas where tracks are cut up when animal enters and leaves a box
                          proj4tracks="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #Used to define projection of tracking data
){
  
  require(sp)
  require(maptools)
  require(raster)
  require(rgdal)
  require(proj4)
  require(ggplot2)
  require(rgeos)
  require(SDMTools)
  require(adehabitatHR)
  
  projWant<-CLIPPERS$projWant
  clipperNamecheck<-CLIPPERS$clipperName
  if (clipperName != clipperNamecheck){print("Clippers don't match, try again")}
  
  rast<-CLIPPERS$rast
  
  # get frequncies from the segments 
  segIDFreq<-as.data.frame(table(tracks$seg_id))
  
  # select segments > min #
  # removed segment named 0 <- this is points outside buffers
  segIDFreq<-segIDFreq[(segIDFreq$Var1)!=0 & segIDFreq$Freq>minNo,]
  segIDFreq<-segIDFreq[!segIDFreq$Var1 %in% id.out,] #manually removes ids if needed
  
  # subset orginal tracks to those Buffer_id1 w/ > minNo obs
  tracks <- tracks%>%dplyr::filter(seg_id %in% segIDFreq$Var1)
  
  #### convert ptt data to spatial, points are brought in WGS84
  tracks.sp <- SpatialPointsDataFrame(coords = tracks[c("lon1","lat1")], data = data.frame(utc = tracks$utc))
  
  # define projection, use the WGS84 projection that is typical
  tracks.sp@proj4string <- CRS(proj4tracks)
  
  # transform the track to projection selected for ploygon
  tracks.sp_proj<-spTransform(tracks.sp, CRS(projWant))
  projtracks<-CRS(projWant)
  
  # select relevant data for kernel density analysis
  tracks$date_time <- as.POSIXct(strptime (tracks.sp_proj@data$utc, "%Y-%m-%d %H:%M:%S"), "GMT")
  
  coords <- tracks.sp_proj@coords    # projected coords
  colnames(coords)<-c("x_proj","y_proj")
  tracks<-cbind(tracks,coords)
  
  # CREATE TRACKS USING YOUR TIME AND LOCATION DATA FOR KERNELBB ANALYSIS
  # catchall to remove duplicate datetimes within STA_id
  tracks1 <-  tracks %>% distinct(uniID,date_time,.keep_all = T) %>%
    arrange(uniID,date_time)
  
  track.ltj <- as.ltraj(xy = cbind(tracks1$x_proj,tracks1$y_proj), 
                        date = tracks1$date_time, 
                        id = tracks1$uniID, #bird level split by time
                        burst = tracks1$seg_id, #all the segments for each bird go in the same burst
                        typeII = TRUE)
  
  #summarize time tracked
  tracksums.out<-tracks1%>%
    group_by(uniID,seg_id,deploy_site,collab1_point_contact_name)%>%
    summarise(date.end=max(date_time),date.begin=min(date_time))%>%
    mutate(days=as.numeric(difftime(date.end,date.begin,units = c("days"))))
  
  # CALCULATE UDBB WITH YOUR SPECIFIED SPEED AND SMOOTHING TERMS 
  ## speed in m/s use same for each species and MATCH what you'be done for SDA, 
  #this possibly done b/c BB has prefilter based on speed, we've already done speed distance and angle
  bb <- kernelbb(track.ltj, sig1=speed, sig2=sig2, grid = rast, byburst=TRUE)  
  
  bbvol = getvolumeUD (bb)
  
  output<-list(bb,bbvol,tracksums.out,contour,projtracks,tracks1)
  names(output)<-c("bb","bbvol","tracksums.out","contour","projW","track")
  return(output)
}