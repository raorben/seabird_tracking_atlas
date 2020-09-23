track_prep<-function(species,
                     year=NA,
                     dir=dir, 
                     dir.in=dir.in, #location of csv files named by each ARGOSID
                     tagtype="ptt", #"ptt","gps","eobs"
                     parameters=parameters,
                     meta=meta){
  
  #cut to deployment time
  #add deployment location if needed; just for ptt data?
  #remove duplicate lat/lon/time rows
  
  #functions needed
  
  paramwant<-subset(parameters,(spp==species & tag==tagtype))
  
  
  meta<-transform(meta, 
                  datetime_deploy_UTC = as.POSIXct(datetime_deploy_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"),
                  datetime_recover_UTC = as.POSIXct(datetime_recover_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"),
                  datetime_start_track_UTC = as.POSIXct(datetime_start_track_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"),
                  datetime_end_track_UTC = as.POSIXct(datetime_end_track_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"))
  
  Track.Plots<-vector("list",length(meta[,1]))
  INFO<-data.frame()
  Tracks<-NULL
  # loop through birds
  for (i in 1:length(meta[,1])) {			  
    #animal.id <- (meta$animal_id[i])
    file_name <- meta$file_name[i]
    STA_id <- meta$STA_id[i]
    
    #### read in track
    track <- read.table(paste(dir.in,"/",file_name,".csv",sep = ""),header=T, sep=",",strip.white=T,stringsAsFactors = F)
    #print ('Total rows')
    #track$utc<-as.character(track$utc)
    TrackLengthOrig<-(length (track[,1]))
    #print (length (track[,1]))
    #allows for two different styles of datetimes in logger files:
    if (grepl(track$utc[1],pattern="/")==TRUE) track<-transform(track, utc= as.POSIXct(utc, tz = "GMT", format = "%m/%d/%Y %H:%M"))
    if (grepl(track$utc[1],pattern="-")==TRUE) track<-transform(track, utc= as.POSIXct(utc, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))
    
    #### cut start/end track based on metadata and insert deployment/recovery locations if specified in metadata
    
    #### cut beginning of track
    ## check if there is a deployment time
    if (!is.na(meta$datetime_deploy_UTC[i])) {
      # cut track to those locations made >= time of deployment 
      track<-track[which(track$utc>=meta$datetime_deploy_UTC[i]),]
    } else if (!is.na(meta$datetime_start_track_UTC[i])) {
      # cut track to those locations made >= time of predefined start time 
      track<-track[which(track$utc>=meta$datetime_start_track_UTC[i]),]
    }
    
    #### cut end of track
    ## check if there is a Recovery time
    if (!is.na(meta$datetime_recover_UTC[i])) {
      # cut track to those locations made <= time of recovery
      track<-track[which(track$utc <= meta$datetime_recover_UTC[i]),]
    } else if (!is.na(meta$datetime_end_track_UTC[i])) {
      # cut track to those locations made <= time of predefined end time 
      track<-track[(track$utc<=meta$datetime_end_track_UTC[i]),]
    }
    
    track$year<-rep(meta$year[i],length(track[,1]))
    track$ptt<-rep(meta$tag_id[i],length(track[,1]))
    
    Tracklength_clipped<-length(track[,1])
    TrackLength_ends_added<-0
    
    if (tagtype=="gps"){track$lc<-"G"}
    
    #### insert rows in track if metadata indicates insert deployment location, inserted rows labelled with lc<-4 and are not filtered
    # does this by adding the first line again, making all values NA, then adds key info back in
    if((!is.na(meta$incl_deploy_loc[i])) & (meta$incl_deploy_loc[i]!=0)) {
      t1=track[1,]
      t1[1,]<-NA
      t1$lc<-4
      #t1$program<-track$program[1]
      t1$tag_id<-track$tag_id[1]
      t1$year<-track$year[1]
      t1$ptt<-track$ptt[1]
      t1$uid<-track$uid[1]-.1
      t1$utc<-meta$datetime_deploy_UTC[i]
      t1$lat1<-as.numeric(meta$lat_deploc[i])
      t1$lon1<-as.numeric(meta$lon_deploc[i])
      track<-rbind(t1,track)
      rm(t1)
      TrackLength_ends_added<-TrackLength_ends_added+1
    }
    
    #### insert rows in track if metadata indicates insert recovery location, inserted rows labelled with lc<-4 and are not filtered
    if ((!is.na(meta$incl_recovery_loc[i])) & (meta$incl_recovery_loc[i]!=0)) {
      t2=track[length(track[,1]),]
      t2[1,]<-NA
      t2$lc<-4
      t2$year<-track$year[length(track[,1])]
      t2$ptt<-track$ptt[length(track[,1])]
      t2$uid<-track$uid[length(track[,1])]+.1
      t2$utc<-meta$datetime_end_track_UTC[i]
      t2$lat1<-as.numeric(meta$lat_end[i])
      t2$lon1<-as.numeric(meta$lon_end[i])
      track<-rbind(track,t2)
      rm(t2)
      TrackLength_ends_added<-TrackLength_ends_added+1
    }
    
    
    info<-data.frame(animal.id,STA_id,vmax,ang[1],ang[2],distlim[1],distlim[2],
                     lcerrref,TrackLengthOrig,Tracklength_clipped,
                     TrackLength_ends_added,latlon0,dupremoved,retained)
    info$animal.id<-as.character(info$animal.id)
    info$lcerrref<-as.character(info$lcerrref)
    
    # bind all info
    INFO<-bind_rows(INFO,info) #was rbind_fill()
    rm(info)
    
    track$sensors<-as.character(track$sensors)
    track<-track%>%dplyr::select(-utcF)#gets rid of utc time as a factor
    
    # bind all filtered tracks
    Tracks<-bind_rows(Tracks,track)
    rm(track)}
  
  Tracks$uid<-1:length(Tracks[,1])
  ouput<-list(Tracks,Track.Plots,INFO)
  names(ouput)<-c("tracks_prep")
  return(ouput)
}