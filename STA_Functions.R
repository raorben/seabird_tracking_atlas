# *****************************
## FUNCTIONS for the California Current Telemetry Atlas
## Original Codes written by Bill Henrey 
## Adapted into functions & scrips by Rachael Orben 
# *****************************


# Filter Tracks -------------------------------------------------------
## 1. cuts track to deployment [meta]
## 2. adds in deployment location as loc 1 if meta is specified [meta]
## 3. add in retrieve location as end loc if meta is specified [meta]
## 4. removes all lat-long values = 0
## 5. replaces ARGOS location quality with numbers (not sure what happens with GPS locations) 
## 6. deletes duplicate points retaining based on 1. >lc & 2. >nb_mes, or 3. first duplicate, trash others
## 7. filter data using sdafilter [set max speed to equivalent to vmax] [parameters]
## 8. removes low quality end locations that weren't culled with earlier criteria
## 9. returns a list:
#        [[1]] tracks (with all tracks)
#        [[2]] plots for QAQC as a list 
#        [[3]] table of # points removed etc.

# should be universal for additional columns since plyr::rbind.fill is used to stack the tracks.

# swaps between loc1 and loc2 need to have been done (RAO thinks)<-add in
# tracks need to have columns: utc,ptt,tag_id,year,uid,lat1,lon1,lat2,lon2

track_prep_filter<-function(species,
                            year=NA,
                            dir=dir, 
                            dir.in=dir.in, #location of csv files named by each ARGOSID
                            tagtype="ptt", #"ptt","gps","eobs"
                            parameters=parameters,
                            meta=meta,
                            lcerrors=lcerrors,
                            lcerrref="costa"){
  
  
  # FILTERING SATELLITE TAG DATA USING THE ARGOSFILTER PACKAGE
  # (Using location data edited by the swap filter in the STAT package)
  # ********************************************************************
  
  # for SDA filtering of Argos data where:
  # doppler mirrors have been filtered (switched) via Seaturle.org's Algorithm
  # inputs
  # folder containing .csv's of bird movements with time(UTC, 6/19/2008 8:20),lat (dd),lon (dd),lc,nb_mess
  # where file name is unique id
  # table of filter parameters
  # table of location errors, (default is lcerrors from Costa et al 2010)
  # outputs
  # .csv of time lat long
  # FilterParametersUsed_ - species,vmax,ang1,ang2,distlim1,distlim2
  # SummaryFriedasErrors_ - summary of individual errors, animal.id,meanerr
  # SummaryFriedasFiltered_ -animal.ids1,lc.1,end_location,not,removed
  
  #INPUTS:
  # species<-set species AUO Code, e.g. "COMU"
  # year<-NA, "2015" #only set up for one year at a time
  # plot<-TRUE # set plot option to review plots TRUE or FALSE
  # dir=dir, #where CCESTA files are stored
  # dir.in=dir.in, #location of csv files named by each ARGOSID
  # tagtype="ptt", #"ptt","gps","eobs"
  
  ##DIRECTORIES FOR RELATIONAL TABLES WITH INFO FOR ALL SPECIES:
  #parameters=parameters,     #parameters.csv
  #meta=meta,                 #PTT_metadata_all.csv
  #lcerrors=lcerrors,         #lcerrors.csv
  #lcerrref="costa" #errors for ARGOS, not sure what happens if GPS tags are selected...
  
  ##### Parameters table for speed, distance, and angle filtering
  #### NOTE: if parameters vary by species be sure to annotate the output tablenames to reflect this!!!
  # vmax=max velocity in m/s,  vmaxSA_Tab2_W5ms_x_3SD = values from Spear and Ainley, note: 22.22 = 80kph; 19.44 = 70 kph; 16.66 = 60 kph
  # ang=angle or spikes to filter out at successive distlim: ang <- c(15, 25) the angle of the spike that is removed, this is Freitas default
  # distlim= distance between successive points to use ang1 vs ang2: ang <- c(2500, 5000) this is Freitas default
  
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
    ptt_deploy_id <- meta$ptt_deploy_id[i]
    #print(animal.id)
    
    #### read in track
    track <- read.table(paste(dir.in,"/",file_name,".csv",sep = ""),header=T, sep=",",strip.white=T,stringsAsFactors = F)
    track$collab1_point_contact_name<-meta$collab1_point_contact_name[meta$animal_id==animal.id]
    track$deploy_site<-meta$deploy_site[meta$animal_id==animal.id]
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
      t1$collab1_point_contact_name<-meta$collab1_point_contact_name[meta$animal_id==animal.id]
      t1$deploy_site<-meta$deploy_site[meta$animal_id==animal.id]
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
      t2$collab1_point_contact_name<-meta$collab1_point_contact_name[meta$animal_id==animal.id]
      t2$deploy_site<-meta$deploy_site[meta$animal_id==animal.id]
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
    
    
    #### remove low quality endlocation records accepted by Fritas filter
    track$filtered[(track$cfilter == "end_location" & track$lc <= -2)]="end_location_rem" ## -2 = LC B
    track$ptt_deploy_id <- rep(meta$ptt_deploy_id[i], length(track[,1]))
    
    #### create vector for points kept
    track$keeps <- (as.numeric((track$cfilter=="not") | (track$cfilter=="end_location")) * (1-as.numeric(track$filtered=="end_location_rem")))
    
    retained <- sum(track$keeps)
    filtered<- (length(track[,1]) - sum(track$keeps))
    
    #### plots retained SDA points on plot in blue	
    Track.Plots[[i]]<-ggplot()+
      geom_path(data=trackdups[trackdups$filtered!="dup",],aes(x=wrap360(lon1),y=lat1),color="lightgrey")+
      geom_path(data=track[which(track$keeps==1),],aes(x=wrap360(lon1),y=lat1),color="blue")+
      geom_point(data=track[1,],aes(x=wrap360(lon1),y=lat1),color="green", cex=1.3)+
      geom_point(data=track[nrow(track),],aes(x=wrap360(lon1),y=lat1),color="red")+
      geom_text(data=track,aes(x=max(wrap360(lon1)),y=max(lat1)),label=((meta$animal_id[i])))+
      xlab("Longitude")+
      ylab("Latitude")+
      theme_classic()
    
    #### screen output		
    #print(c("TrackLengthOrig",TrackLengthOrig,"Tracklength_clipped",Tracklength_clipped,
    #        "TrackLength_ends_added",TrackLength_ends_added,
    #        "latlong=0",latlon0,"dup removed", 
    #        dupremoved,"filtered",filtered,
    #        "retained",retained))
   
    info<-data.frame(animal.id,ptt_deploy_id,vmax,ang[1],ang[2],distlim[1],distlim[2],
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
    animal.id <- (meta$animal_id[i])
    file_name <- meta$file_name[i]
    ptt_deploy_id <- meta$ptt_deploy_id[i]
    #print(animal.id)
    
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
  
    
    info<-data.frame(animal.id,ptt_deploy_id,vmax,ang[1],ang[2],distlim[1],distlim[2],
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
    ptt_deploy_id <- meta$ptt_deploy_id[i]
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
    track$ptt_deploy_id <- rep(meta$ptt_deploy_id[i], length(track[,1]))
    
    #### create vector for points kept
    track$keeps <- (as.numeric((track$cfilter=="not") | (track$cfilter=="end_location")) * (1-as.numeric(track$filtered=="end_location_rem")))
    
    retained <- sum(track$keeps)
    filtered<- (length(track[,1]) - sum(track$keeps))
    
    info<-data.frame(animal.id,ptt_deploy_id,vmax,ang[1],ang[2],distlim[1],distlim[2],
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


tf_filt_sum<-function(tracks){
  require(reshape2)
  #INPUTS:
  #tracks is any Freitas filtered track file with the columns
  #'lc','ptt_deploy_id','keeps'
  
  tracks$ptt_deploy_id<-as.factor(tracks$ptt_deploy_id)
  tracks$lc<-as.factor(tracks$lc)
  tracks$keeps<-as.character(as.logical(tracks$keeps))
  tracks$keeps[tracks$keeps=="TRUE"]<-"retained"
  tracks$keeps[tracks$keeps=="FALSE"]<-"filtered"
  
  # organize data by ptt
  # melt the data
  tracks$meas <- rep(1, length(tracks[,1]))
  tracks.m<-melt(tracks, c("ptt_deploy_id", "lc","filtered","keeps"),"meas")
  
  # cast the data (make the pivot)
  filter.results<-dcast(tracks.m, ptt_deploy_id + lc ~ keeps + value , sum)
  return(filter.results)}


tf_filt_error<-function(tracks,filt_sum,lcerrors,lcerrref="costa"){
  #INPUTS:
  #tracks is any Freitas filtered track file with the columns 'lc','ptt_deploy_id','keeps'
  #data from of location errors
  #lcerrref=which errors to use (only costa and douglas as options currently)
  
  filter.results<-filt_sum
  
  indiv.error<-merge(filter.results, lcerrors, by = 'lc', all = FALSE)
  
  # lcerrors
  if(lcerrref=="costa"){
    lcerr<-indiv.error$X68errCosta
  }else{
    if(lcerrref=="douglas"){
      lcerr<-indiv.error$X68errDougMaxredun10
    }}
  
  indiv.error$error.prod<-lcerr*indiv.error$retained_1
  
  indiv.error.m<-melt(indiv.error, id=c("ptt_deploy_id", "lc", "retained_1", "error.prod"), measure = c("error.prod", "retained_1"))
  indiv.error.results<-dcast(indiv.error.m, ptt_deploy_id ~ variable, sum)
  
  indiv.error.results$mean.error.track<-indiv.error.results$error.prod/indiv.error.results$retained_1
  ## get mean error for the dataset (because the PTT is a factor, the leading zeros throw an error that should be ignored)
  indiv.error.results
  #<-rbind(indiv.error.results,
                             #c(0,0,0,mean(indiv.error.results$mean.error.track, na.rm=T)),
                             #c(0,0,0,sd(indiv.error.results$mean.error.track,na.rm=T)),
                             #c(0,0,0,length(indiv.error.results[,1])))
}



# Polygons ------------------------------------------------------
#### Read in shapefile(s) for indexing data, files = (1) shapefile (2) shapefile with buffer
# lme for selecting data in a given lme
# lme buffer for selecting data that will be processed (rasterized), thus resulting file extent will
# excede boundaries of lme
# to get complete info on projection and fields in shapefile use: ogrInfo(dsn=dsn,layer='lmes.Buf100km')

# 1. Uses clipPolyList.csv to find, project, and make a buffer for a selected polygon
# the buffer size was originally determined by a column in the list 
# (so you can read the file when you load it), but the buffer(km) was added as a specification
# in the function for more flexiabilty

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

# Tracks in Poly / on Grid ------------------------------------------------

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
  
  all_tracks$utc<-as.POSIXct(format(strptime(as.character(all_tracks$utc), "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S"), tz = "GMT")
  
  ids<-unique(all_tracks$uniID)
  Clipper.Plots<-vector("list",length(ids))
  tracks.out<-data.frame()
  
  for (i in 1:length(ids)) {
    id <- ids[i]
    track <- all_tracks[all_tracks$uniID==id,]
    #year.id <- track$year[1]
    
    track.filts <- track%>%dplyr::filter(keeps==1)
    TrackLength<-nrow(track.filts)
   
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


calc_leavetimesegs<-function(hrs=8,#### set hrs for minimum gap in second (converted to sec with time gap used create new segment each time animal leaves and returns in to box)
                             tracks,
                             clipperName){
  require(trip)
  require(dplyr)
  minGap<-3600 * hrs
  
  # create row index
  ridx<-seq(from = 1, to = nrow(tracks), by = 1)
  tracks<-cbind(ridx, tracks)  

  tracks.want<-tracks%>%dplyr::filter(in_polyBuffer==1)
  
  tracks.want$id3<-trip::sepIdGaps(tracks.want$uniID, tracks.want$utc, minGap=minGap)
  
  tracks$id3<-tracks$in_poly
  
  fixes <- match(tracks$ridx,tracks.want$ridx,nomatch=0)
  tracks$id3[fixes!=0] <- tracks.want$id3[fixes]
  
  tracks$seg_id<-tracks$id3
  
  #remove variables used for this function
  tracks<-tracks%>%dplyr::select(-id3,ridx)
  
  tracks<-tracks%>%filter(seg_id!=0)
  return(tracks)
}

# Brownian Bridges ---------------------------------------------------------

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
  # catchall to remove duplicate datetimes within ptt_deploy_id
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



bb_individuals<-function(bb_probabilitydensity=bb, #Output from IndividualBB
                         tracksums=tracksums.out,
                         cellsize=3000){
  ## NOTE: ud estimates have been scaled by multiplying each value by cellsize^2 to make prob vol that sums to 1.00 accross pixel space
  #individuals also may be split across groups (year, season) if the tracks were segmented useing these 
  
  require(adehabitatHR) 
  require(SDMTools)

  #grp.meta<-data.matrix(meta[grping.var])
  bb<-bb_probabilitydensity
  burst<-names(bb)
  tracksums$bbref<-1:nrow(tracksums)

  # get unique groups (use tracksums b/c these points are contained in polygon - not a tracks will necessarily be represented in a given polygon)
  (grp.ids<-unique(tracksums$timegrp))

  #### initialize lists to house data by desired grouping variable (group.uniq)
  # list to house uds normalized by track duration
  bbindis <- vector ("list", length(grp.ids))

  #### loop through groups
  for (h in 1:length(grp.ids)) {
    grp.id<-grp.ids[h]
    #tracksums.want<-tracksums[which(tracksums$grp==grp.ids[k]),]
    tracksums.want<-tracksums%>%dplyr::filter(timegrp==grp.id)
    
    # create summary table of # of segments from each track
    (track.freq<-tracksums.want%>%group_by(uniID)%>%
      dplyr::summarize(n=n_distinct(seg_id),minbb=min(bbref),maxbb=max(bbref)))

    # initialize lists to house data for segment based on deploy_id
    ud.track <- vector("list", nrow(track.freq))
    track.days <- vector ("list", nrow(track.freq))

    # sum up segments for each track
    # run through track.freq table summing segments >1
    for (j in 1:nrow(track.freq)) {
      if (track.freq$n[j]==1) {
        # operation for only one segment in polygon
        bbIndx<-track.freq$minbb[j]
        ud.seg<-bb[[bbIndx]]
        a<- slot(ud.seg,"data")
        slot(ud.seg,"data")<-a*(cellsize^2)
        ud.track[[j]]<-ud.seg

        # get number of track days (in decimal days)
        track.days[[j]]<-tracksums.want$days[tracksums.want$uniID==track.freq$uniID[j]]
        #paste(paste("bird:",track.freq$uniID[j],
        #            "segnum:",track.freq$n[j],
        #            "area:",sum(slot(ud.track[[j]],"data")[,1])))
      } else {
        # get multiple segments
        days.segs<-tracksums.want$days[tracksums.want$uniID==track.freq$uniID[j]]
        bbIndx.segs<-seq(from=track.freq$minbb[track.freq$uniID==track.freq$uniID[j]],
            to=track.freq$maxbb[track.freq$uniID==track.freq$uniID[j]])

        # list to house each segment
        ud.segs.new <- vector ("list", length(bbIndx.segs))
        
        for (k in 1:length(bbIndx.segs)) {
          bbIndx<-bbIndx.segs[k]
          ud.seg <- bb[[bbIndx]]
          # weigh each segment it's proportion of total hours tracked within the clipperName (Freiberg 20XX paper)
          a<- slot(ud.seg,"data")*(days.segs[k]/sum(days.segs))
          #slot(ud.seg,"data")<-a
          ud.segs.new[[k]] <- a
        }

        # adds the segments from one bird together into one UD
        spdf<-Reduce("+",ud.segs.new)*(cellsize^2)
        sum(spdf)
        estUDsum<-ud.seg#steal UD formatting from 
        slot(estUDsum,"data")<-spdf
        
        ud.track[[j]]<-estUDsum
        # get number of track days
        track.days[[j]]<-sum(days.segs)}
        #print(paste(j,k,sum(slot(ud.track[[j]],"data"))))
      }

    names(ud.track)<-track.freq$uniID
    class(ud.track) <- "estUDm" 
    bbindis[[h]]<-ud.track
  }
  names(bbindis)<-grp.ids
  return(bbindis)
}




bb_countindinum_gridcell<-function(bbindis,
                                   clipper_list=clipper_list){
  (grp.ids<-names(bbindis))
    
    hab<-clipper_list$rast
    fullgrid(hab)<-TRUE
    hab@data[hab@data==0]<-1
  
  for (h in 1:length(grp.ids)) {
    
    estUDm<-bbindis[[h]]
    for (j in 1:length(estUDm)){
      
    udspdf <- estUDm2spixdf(bbindis[[h]]) ## ud is an object of the class estUDm ## Convert it to SpatialPixelsDataFrame
    fullgrid(udspdf) <- TRUE ## Convert the original map to fullgrid (i.e. SpatialGridDataFrame)
    
    #sum so all individuals equal one
    resu <- lapply(1:ncol(udspdf), function(i) {
      udspdf[[i]] * hab[[1]] / sum(udspdf[[i]] * hab[[1]])
    })
    sum(resu[[1]],resu[[2]])
    resu <- as.data.frame(resu)
    names(resu) <- names(udspdf@data)
    names(resu)
    head(resu)
    resu[resu>0]<-1
    numindi<-rowSums(resu)
    #str(udspdf)
    #head(udspdf@data)
    udspdf@data[udspdf@data>0]<-1
    numindi<-rowSums(udspdf@data)
    }
    re <- as.data.frame(numindi)
    udspdf@data <- re
    estUDm[[h]]<- udspdf@data
    image(udspdf["numindi"],zlim = c(0,20),border = grey(1))
    }
}
  



bb_sumbygroup<-function(bbindis,
                        tracksums.out){
  (grp.ids<-names(bbindis))
  
  grp.estUDm<-vector("list", length(grp.ids))
  for (h in 1:length(grp.ids)) {
    
    # multiply new ud by (# of decimal days of each track/sum decimal days all tracks for that group)
    estUDm<-bbindis[[h]]

    (track_info<-tracksums.out%>%
      filter(timegrp==grp.ids[[h]])%>%
      group_by(uniID)%>%
      dplyr::summarize(n=n_distinct(seg_id), days=sum(days)))
    
    (alldays<-sum(track_info$days))
    
    ## ud is an object of the class estUDm ## Convert it to SpatialPixelsDataFrame
    udspdf <- estUDm2spixdf(estUDm)
    ## Convert the original map to fullgrid (i.e. SpatialGridDataFrame)
    fullgrid(udspdf) <- TRUE 

    # run through all rasters to sum by grouping variable
    #     # calculate ud weighted by track.days, weigh each track it's proportion of
    #     # total hours tracked within the clipperName (Freiberg 20XX paper)
    #ncol(udspdf) is specifying processing for each individual (1 column / individual)
    densitys <- lapply(1:ncol(udspdf), function(i) {
      udspdf[[i]] * track_info$days[[i]] / alldays
           })
    names(densitys)<-track_info$uniID #add ids back on - otherwise it looks crazy
    
    densitys <- as.data.frame(densitys)
      head(densitys)
      head(den_sum<-rowSums(densitys))
      den_sum <- as.data.frame(den_sum)
    udspdf@data <- den_sum
    fullgrid(udspdf) <- FALSE
    #image(udspdf["densitys"])
    
    #reclass as estUDm
    re <- lapply(1:ncol(udspdf), function(i) {
      so <- new("estUD", udspdf[,i])
      so@h <- list(h=0, meth="specified")
      so@vol <- FALSE
      return(so)})
    
    names(re) <- names(udspdf)
    class(re) <- "estUDm"
    image(re)

    grp.estUDm[[h]]<-re
  }
  names(grp.estUDm)<-grp.ids
  class(grp.estUDm) <- "estUDm" 
  return(grp.estUDm)
}




# plots -----------------------------------------------------------

sta_quickplot<-function(bbgroups,
                        clipper_list=clipper_list,
                        dir,
                        species,
                        tracks_filt=tracks_filt,
                        xlim=c(-130,-121),
                        ylim=c(37,49)){
  
  
  require(ggplot2)
  states<-map_data('state') 
  states_sub<-states[states$region%in%c("washington","oregon","california","alaska"),]
  w2hr<-map_data('world')
  w2hr_sub<-w2hr[w2hr$region%in%c("USA","Russia","Mexico","Canada","North Korea","South Korea"),]
  
  #get the clipper ready
  clipper_wgs84<-clipper_list$clipper
  (prjWant<-clipper_list$projWant)
  clipper_proj<-clipper_list$clipper_proj
  
  grp.ids<-names(bbgroups)
  
  for (h in 1:length(names(bbgroups))){
    # make the estUD into a raster
    a<-bbgroups[[h]]
    allgrps.indiv.raster<-raster(a$den_sum)
    
    # add projection to raster
    proj4string(allgrps.indiv.raster)<-CRS(prjWant)
    proj4string(allgrps.indiv.raster)
    
    #cellStats(allgrps.indiv.raster,'sum')
    
    # crop raster to clipper
    r2 <- crop(allgrps.indiv.raster, extent(clipper_proj))
    allgrps.indiv.raster.clip <- raster::mask(r2, clipper_proj)
    
    ## Check that it worked
    #plot(allgrps.indiv.raster.clip)
    #plot(clipper_proj, add=TRUE, lwd=2)
    
    # take clipped raster and reproject in WGS84 for viz
    allgrps.indiv.raster.clip.wgs84<-projectRaster(allgrps.indiv.raster.clip,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    # make into points
    bb.df.wgs84 <- data.frame(rasterToPoints(allgrps.indiv.raster.clip.wgs84))
    colnames(bb.df.wgs84)<-c("Longitude","Latitude","Density")
    
    # take sqrt of Density to better highlight high use areas
    bb.df.wgs84$Density[bb.df.wgs84$Density==0]<-NA
    bb.df.wgs84$Density.n<-sqrt(bb.df.wgs84$Density)
    
    
    A<-
      ggplot() +
      geom_raster(data=fortify(bb.df.wgs84)%>%dplyr::filter(Density.n>0.0001), aes(y=Latitude, x=Longitude,fill=Density.n)) +
      scale_fill_gradientn(colors=c("#3096C6", "#9EC29F","#F2EE75", "#EE5B2F","#E7292A"),name="sqrt(Density.n)") +
      geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
      geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
      geom_polygon(data=clipper_wgs84,aes(long,lat,group=group),fill="NA",color="black",size=.5)+
      theme_bw() +
      coord_equal() +
      coord_fixed(ratio=1.7,xlim = xlim,ylim=ylim)+
      theme(axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16, angle=90),
            axis.text.x = element_text(size=8),
            axis.text.y = element_text(size=8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    B<-ggplot() +
      geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
      geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
      geom_path(data=tracks_filt%>%
                  filter(timegrp==grp.ids[[h]]),
                aes(x=lon1,y=lat1,group=tag_id,color=as.factor(tag_id)),size=0.4)+
      geom_polygon(data=clipper_wgs84,aes(long,lat,group=group),fill="NA",color="black",size=.4)+
      theme_bw() +
      coord_equal() +
      coord_fixed(ratio=1.7,xlim = xlim,ylim=ylim)+
      theme(axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16, angle=90),
            axis.text.x = element_text(size=8),
            axis.text.y = element_text(size=8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    plotAB<-gridExtra::grid.arrange(A,B,ncol=2)
    ggsave(plotAB,filename = paste0(dir,"species/",species,"/",grp.ids[h],"_",species,"_",clipper_list$clipperName,".png"),width=10,dpi = 300)
    
  }
}


sta_rangeplot<-function(bbgroups,
                        clipper_list=clipper_list,
                        dir,
                        species,
                        tracks_filt=tracks_filt){
  
  
  require(ggplot2)
  states<-map_data('state') 
  states_sub<-states[states$region%in%c("washington","oregon","california","alaska"),]
  w2hr<-map_data('world')
  w2hr_sub<-w2hr[w2hr$region%in%c("USA","Russia","Mexico","Canada","North Korea","South Korea"),]
  
  #get the clipper ready
  clipper_wgs84<-clipper_list$clipper
  (prjWant<-clipper_list$projWant)
  clipper_proj<-clipper_list$clipper_proj
  
  grp.ids<-names(bbgroups)
  
  for (h in 1:length(names(bbgroups))){
    # make the estUD into a raster
    a<-bbgroups[[h]]
    allgrps.indiv.raster<-raster(a$den_sum)
    
    # add projection to raster
    proj4string(allgrps.indiv.raster)<-CRS(prjWant)
    proj4string(allgrps.indiv.raster)
    
    #cellStats(allgrps.indiv.raster,'sum')
    
    # crop raster to clipper
    r2 <- crop(allgrps.indiv.raster, extent(clipper_proj))
    allgrps.indiv.raster.clip <- raster::mask(r2, clipper_proj)
    
    ## Check that it worked
    #plot(allgrps.indiv.raster.clip)
    #plot(clipper_proj, add=TRUE, lwd=2)
    
    # take clipped raster and reproject in WGS84 for viz
    allgrps.indiv.raster.clip.wgs84<-projectRaster(allgrps.indiv.raster.clip,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    # make into points
    bb.df.wgs84 <- data.frame(rasterToPoints(allgrps.indiv.raster.clip.wgs84))
    colnames(bb.df.wgs84)<-c("Longitude","Latitude","Density")
    
    # take sqrt of Density to better highlight high use areas
    bb.df.wgs84$Density[bb.df.wgs84$Density==0]<-NA
    bb.df.wgs84$Density.n<-sqrt(bb.df.wgs84$Density)
    
    datat=tracks_filt%>%
      filter(timegrp==grp.ids[[h]])
    xlim<-c(min(wrap360(datat$lon1)),max(wrap360(datat$lon1)))
    ylim<-c(min(datat$lat1),max(datat$lat1))
    
    A<-
      ggplot() +
      geom_path(data=tracks_filt%>%
                  filter(timegrp==grp.ids[[h]]),
                aes(x=wrap360(lon1),y=lat1,group=tag_id,color=as.factor(tag_id)),size=0.4)+
      geom_raster(data=fortify(bb.df.wgs84)%>%dplyr::filter(Density.n>0.0001), aes(y=Latitude, x=wrap360(Longitude),fill=Density.n)) +
      scale_fill_gradientn(colors=c("#3096C6", "#9EC29F","#F2EE75", "#EE5B2F","#E7292A"),name="sqrt(Density.n)") +
      geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="black",color="grey60",size=0.1)+
      geom_polygon(data=states_sub,aes(wrap360(long),lat,group=group),fill="black",color="grey60",size=0.1)+
      geom_polygon(data=clipper_wgs84,aes(wrap360(long),lat,group=group),fill="NA",color="black",size=.5)+
      theme_bw() +
      coord_equal() +
      coord_fixed(ratio=1.7,xlim = xlim,ylim=ylim)+
      theme(axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16, angle=90),
            axis.text.x = element_text(size=8),
            axis.text.y = element_text(size=8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  
    
    plotAB<-gridExtra::grid.arrange(A,ncol=1)
    ggsave(plotAB,filename = paste0(dir,"species/",species,"/",grp.ids[h],"_",species,"_",clipper_list$clipperName,"rangeplot.png"),width=10,dpi = 300)
    
  }
}


wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}


# export to .asc ----------------------------------------------------------
# Export Individual BB ASCII files for ArcMap: a couple of options
#fix error if directory doesn't exist
#changed with depretiation of adehabitat - > what outputs do we want?
#ExportASCII_SegmentBB(SegmentBB, species, clipperName,cellsize=cellsize3km, dir)


# ### export .shp with tracking data for all bird.ids (makes a folder)
#tracks.filts.sp<-tracksclipped[[1]]
#clipperName<-tracksclipped[[4]]
#  writeOGR(obj=tracks.filts.sp,dsn=paste(dir,"species/",species,"/",species,'_all_pts_Freitas_in',clipperName, sep = ""),
#           layer=paste(species,'all_pts_Freitas_in',clipperName, sep = ""),
#           overwrite_layer='T', driver="ESRI Shapefile")

# ## and find the contours
# ver <- getverticeshr(re, percent = 50, standardize = TRUE)
# ver2 <- getverticeshr(re, percent = 90, standardize = TRUE)
# 
# proj4string(ver) <- CRS(proj4string(mptyrs))
# proj4string(ver2) <- CRS(proj4string(mptyrs))
# 
# ver50 <- spTransform(ver, CRS("+init=epsg:4326"))
# ver90 <- spTransform(ver2, CRS("+init=epsg:4326"))
# writePolyShape(ver50,file.path(output,"RHP_CHICK_50"))
# writePolyShape(ver90,file.path(output,"RHP_CHICK_90"))


