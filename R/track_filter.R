track_filter<-function(species,
                       year=NA,
                       dir=dir, 
                       dir.in=dir.in, #location of csv files named by each ARGOSID
                       tagtype="ptt", #"ptt","gps","eobs"
                       parameters=parameters,
                       meta=meta,
                       lcerrors=lcerrors,
                       lcerrref="costa"){
  
  #takes trimmed, dups removed, 1st/last location added tracks
  # and speed filters!
  
  #functions needed
  require(argosfilter)
  require(ggplot2)
  
  paramwant<-subset(parameters,(spp==species & tag==tagtype))
  
  vmax <- (paramwant$vmaxSA_Tab2_W5ms_x_3SD)    
  #print(paste("maximum velocity = ",vmax))
  ang <- c(paramwant$ang1,paramwant$ang2)
  #print(paste("angles = ",ang))
  distlim <-  c(paramwant$distlim1,paramwant$distlim2)
  #print(paste("distance limit = ",distlim))
  
  # lcerrors
  if(lcerrref=="costa"){
    lcerr<-lcerrors$X68errCosta
  }else{
    if(lcerrref=="douglas"){
      lcerr<-lcerrors$X68errDougMaxredun10
    }}
  
  #### select metadata want
  if (is.na(year)) {
    meta<-meta[meta$species==species & meta$loc_data==1,]
  } else{
    meta<-meta[meta$species==species & meta$year==year & meta$loc_data==1,]
  }
  
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
    animal.id <- (meta$animal_id[i])
    file_name <- meta$file_name[i]
    STA_id <- meta$STA_id[i]
    #print(animal.id)
    
    #### read in track
    track <- read.table(paste(dir.in,"/",file_name,".csv",sep = ""),header=T, sep=",",strip.white=T,stringsAsFactors = F)
    #print ('Total rows')
    #track$utc<-as.character(track$utc)
    TrackLengthOrig<-(length (track[,1]))
    #print (length (track[,1]))
    
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
    
    # initiate track filtered vector
    track$filtered<-rep(0,length(track[,1]))
    
    #### remove all lat-long values = 0
    latlon0=sum(as.numeric(track$lat1 == 0 | track$lon1 == 0))
    track<-track[(track$lat1 != 0 | track$lon1 != 0),]
    #print (c('Total rows after latlon=0 removed',length(track[,1])))
    
    # replace lc with numbers
    track$lc <- as.character(track$lc) # change to characters
    
    # new order of accuracy
    # 3,2,1,0,-1,-2,-9
    track$lc[track$lc == "A"] <- -1
    track$lc[track$lc == "B"] <- -2
    track$lc[track$lc == "Z"] <- -9
    track$lc[track$lc == "G"] <- 99
    track$lc <- as.numeric(track$lc) # change to numbers
    
    
    #### delete duplicate points retaining based on 1. >lc & 2. >nb_mes, or 3. first duplicate, trash others
    track$utcF <- factor(track$utc)
    dups<-  track[track$utcF %in% track$utcF[duplicated(track$utcF)],]
    dups$utcF<-factor(dups$utcF)
    
    if (length(dups[,1])!=0) {
      # identify duplicate records
      dup.freq <- as.data.frame.table (tapply(dups$utcF, dups$utcF, length))
      # k<-1
      for (k in 1:length(dup.freq[,1])) {
        dups.test<-dups[dups$utcF %in% dup.freq$Var1[k],]
        if ((length(unique(dups.test$lc))!=1) & ("lc" %in% names(dups.test))) {
          # first check and retain location higher lc
          track$filtered[which(track$uid==dups.test$uidF[dups.test$lc==min(dups.test$lc)])]="dup" 
          # second if same lc, check and retain location higher num.mes 
        } else if ((length(unique(dups.test$nb_mes))!=1) & ("nb_mes" %in% names(dups.test))) {
          track$filtered[which(track$uid==dups.test$uid[dups.test$nb_mes==min(dups.test$nb_mes)])]="dup" 
          # third if same nb.mess, retain the first duplicate
        } else {
          track$filtered[which(track$uid==dups.test$uid[2:(length(dups.test$uid))])]="dup"
        }
      }
    }
    dupremoved=length(track$filtered[track$filtered=="dup"])
    
    # remove dups
    trackdups<-track
    track<-track[track$filtered!="dup",]
    
    #### filter data using sdafilter [set max speed to equivalent to vmax]
    track$cfilter<- sdafilter (track$lat1, track$lon1, track$utc, track$lc, vmax, ang, distlim)
    
    #### plots retained SDA points on plot in blue	
    Track.Plots[[i]]<-ggplot()+
      geom_path(data=trackdups[trackdups$filtered!="dup",],aes(x=wrap360(lon1),y=lat1),color="lightgrey")+
      geom_path(data=track[which(track$cfilter=="not"),],aes(x=wrap360(lon1),y=lat1),color="blue")+
      geom_point(data=track[1,],aes(x=wrap360(lon1),y=lat1),color="green", cex=1.3)+
      geom_point(data=track[nrow(track),],aes(x=wrap360(lon1),y=lat1),color="red")+
      geom_text(data=track,aes(x=max(wrap360(lon1)),y=max(lat1)),label=((meta$animal_id[i])))+
      xlab("Longitude")+
      ylab("Latitude")+
      theme_classic()
    
    #### remove low quality endlocation records accepted by Fritas filter
    track$filtered[(track$cfilter == "end_location" & track$lc <= -2)]="end_location_rem" ## -2 = LC B
    track$STA_id <- rep(meta$STA_id[i], length(track[,1]))
    
    #### create vector for points kept
    track$keeps <- (as.numeric((track$cfilter=="not") | (track$cfilter=="end_location")) * (1-as.numeric(track$filtered=="end_location_rem")))
    
    retained <- sum(track$keeps)
    filtered<- (length(track[,1]) - sum(track$keeps))
    
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
  names(ouput)<-c("tracks_filt","tf_plots","tf_info")
  return(ouput)
}
