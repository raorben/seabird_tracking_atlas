in_poly<-function(all_tracks=tracks,# tracks<-output[[1]] from function SDAFreitas_CCESTA
                  CLIPPERS=CLIPPERS,
                  dir.out=dir,
                  prjtracks="+proj=longlat +ellps=WGS84 +datum=WGS84"){ #default projection: the WGS84 projection that Argos Data is delivered in
  
  require(stringr)
  require(ggplot2)
  
  #Extraction of CLIPPER List from PolygonPrep_CCESTA
  clipper<-CLIPPERS$clipper
  clipper_proj<-CLIPPERS$clipper_proj
  clipperBuff_proj<-CLIPPERS$clipperbuff_proj
  projWant<-CLIPPERS$projWant
  clipperName<-CLIPPERS$clipperName
  rast<-CLIPPERS$rast
  
  all_tracks$utc<-as.POSIXct(format(strptime(as.character(all_tracks$utc), "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  
  ids<-unique(all_tracks$uniID)
  Clipper.Plots<-vector("list",length(ids))
  tracks.out<-data.frame()
  
  for (i in 1:length(ids)) {
    id <- ids[i]
    track <- all_tracks[all_tracks$uniID==id,]
    #year.id <- track$year[1]
    
    track.filts <- track%>%dplyr::filter(keeps==1)
    TrackLength<-nrow(track.filts)
    
    if(TrackLength==0)next
    #convert tracks into a spatial data frame & reproject
    track.filts.sp <- SpatialPointsDataFrame(coords = track.filts[c("lon1","lat1")], data = track.filts)
    
    # define projection of tracks, the WGS84 projection that Argos Data is delivered in is the default
    track.filts.sp@proj4string <- CRS(prjtracks)
    
    # transform the track to projection defined for the polygon
    track.filts.sp_proj<-spTransform(track.filts.sp, CRS(projWant))
    
    # create a spatialpolygon from the spatialpolygonsdataframe
    clipper_proj.sp <- as(clipper_proj, "SpatialPolygons")
    
    #### Create index for row with data bounded by desired polygon
    # replace NAs generate for points not in clipper and clipperBuff with 0
    
    # portion of track in the buffered polygon used to create bridge
    track_in.polyBuffer <- over(track.filts.sp_proj, clipperBuff_proj)
    track_in.polyBuffer[is.na(track_in.polyBuffer)] <- 0
    
    # potion of track in the polygon
    track_in.poly <- over(track.filts.sp_proj, clipper_proj.sp)
    track_in.poly[is.na(track_in.poly)] <- 0
    
    #### Add the spatially joined in.out points to the UNPROJECTED spatial data frame.
    track.filts.sp$in_poly <- track_in.poly
    track.filts.sp$in_polyBuffer <- track_in.polyBuffer
    
    # replace col names require(stringr), note will replace all string in header(s) with clipperName!!
    #names(track.filts.sp@data) <- str_replace(string=names(track.filts.sp@data), pattern="in_poly", replacement=as.character(clipperName))
    
    #### Add the spatially joined in.out points to the PROJECTED spatial data frame (for plot in aea proj.)
    track.filts.sp_proj$in_poly <- track_in.poly
    track.filts.sp_proj$in_polyBuffer <- track_in.polyBuffer
    
    Clipper.Plots[[i]]<-
      ggplot()+
      geom_path(data=clipper_proj,aes(x = long, y = lat),color="lightgrey")+
      geom_path(data=clipperBuff_proj,aes(x = long, y = lat),color="darkgrey")+
      geom_point(data=as.data.frame(coordinates(track.filts.sp_proj))[track_in.polyBuffer==1,],aes(x=lon1,y=lat1),color='#0000FF',pch=16, cex=1.3)+
      geom_point(data=as.data.frame(coordinates(track.filts.sp_proj))[track_in.polyBuffer==0,],aes(x=lon1,y=lat1),color='#FFB90F',pch=16, cex=1.3)+
      geom_point(data=as.data.frame(coordinates(track.filts.sp_proj))[track_in.poly==1,],aes(x=lon1,y=lat1),color='#0000FF',pch=16, cex=.5)+
      geom_point(data=as.data.frame(coordinates(track.filts.sp_proj))[track_in.poly==0,],aes(x=lon1,y=lat1),color='#FF3030',pch=16, cex=.5)+
      geom_text(data=as.data.frame(coordinates(track.filts.sp_proj)),aes(x=max(lon1),y=max(lat1)),label=as.character(id))+
      xlab("Longitude")+
      ylab("Latitude")+
      theme_classic()
    
    head(track.filts.sp_proj)
    
    # replace col names require(stringr)
    #names(track.filts.sp_proj@data) <- str_replace(string=names(track.filts.sp_proj@data), pattern="in_poly", replacement=as.character(clipperName))
    
    #### concatenate each track into spatial dataframe (shapefile)
    if (i==1) {
      tracks.filts.sp<-track.filts.sp
    }else{
      tracks.filts.sp<-rbind(tracks.filts.sp, track.filts.sp)
    }
    
    tracks.out<-rbind(tracks.out,track.filts.sp@data)
    
    
    rm(track, track.filts, track.filts.sp_proj, TrackLength)
  }
  
  tracksclipped<-list(track.filts.sp,Clipper.Plots,tracks.out,clipperName)
  names(tracksclipped)<-c("tracks_filt_clip_spatialdf","Clipper.Plots","tracks.out","clipperName")
  return(tracksclipped)
}


