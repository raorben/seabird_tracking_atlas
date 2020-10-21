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
  require(lubridate)
  
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
  
  #meta<-transform(meta, 
  #                datetime_deploy_UTC = as.POSIXct(datetime_deploy_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"),
  #                datetime_recover_UTC = as.POSIXct(datetime_recover_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"),
  #                datetime_start_track_UTC = as.POSIXct(datetime_start_track_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"),
  #                datetime_end_track_UTC = as.POSIXct(datetime_end_track_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"))
  
  Track.Plots<-vector("list",length(meta[,1]))
  
  INFO<-data.frame()
  Tracks<-NULL
  missingbirds<-NULL
  # loop through birds
  for (i in 1:length(meta[,1])) {			  
    #animal.id <- (meta$animal_id[i])
    file_name <- meta$file_name[i]
    STA_id <- meta$STA_id[i]
    #print(animal.id)
    
    #### read in track
    file1<-list.files(path = dir.in,recursive = TRUE,pattern=file_name)
    if (length(file1)==0) missingbirds<-rbind(missingbirds,file_name)
    if (length(file1)==0) next
    
    #option one uses exact match with metadata
    if (stringr::str_detect(file_name,file1[1])==TRUE){
      track <- read.table(paste(dir.in,"/",file_name,".csv",sep = ""),
                          header=T, sep=",",strip.white=T,stringsAsFactors = F)
    }else {track <- read.table(paste(dir.in,"/",file1[1],sep = ""),
                               header=T, sep=",",strip.white=T,stringsAsFactors = F)}
    
    
    track$collab1_point_contact_name<-meta$collab1_point_contact_name[i]
    track$deploy_site<-meta$deploy_site[i]
    
    #print ('Total rows')
    #track$utc<-as.character(track$utc)
    TrackLengthOrig<-(length (track[,1]))
    #print (length (track[,1]))
    #allows for two different styles of datetimes in logger files:
    if (grepl(track$utc[1],pattern="/")==TRUE) track<-transform(track, utc= as.POSIXct(utc, tz = "UTC", format = "%m/%d/%Y %H:%M"))
    if (grepl(track$utc[1],pattern="-")==TRUE) track<-transform(track, utc= as.POSIXct(utc, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
    
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
    
    track$year<-rep(meta$deploy_year[i],length(track[,1]))
    track$ptt<-rep(meta$tag_id[i],length(track[,1]))
    
    Tracklength_clipped<-length(track[,1])
    TrackLength_ends_added<-0
    
    
    if (tagtype=="gps"){track$lc<-"G"}
    if(length(track$lc)==0){print("Check tag type")}
    
    #### insert rows in track if metadata indicates insert deployment location, inserted rows labelled with lc<-4 and are not filtered
    # does this by adding the first line again, making all values NA, then adds key info back in
    if((!is.na(meta$incl_deploy_loc[i])) & (meta$incl_deploy_loc[i]!=0)) {
      t1=track[1,]
      #t1[1,]<-NA
      t1$lc<-4
      #t1$program<-track$program[1]
      t1$tag_id<-track$tag_id[1]
      t1$year<-track$year[1]
      t1$ptt<-track$ptt[1]
      track<-fncols(track, "uid")
      t1$uid<-track$uid[1]-.1
      t1$utc<-meta$datetime_deploy_UTC[i]
      t1$lat1<-as.numeric(meta$lat_deploc[i])
      t1$lon1<-as.numeric(meta$lon_deploc[i])
      
      t1$collab1_point_contact_name<-meta$collab1_point_contact_name[i]
      t1$deploy_site<-meta$deploy_site[i]
      
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
      
      t2$collab1_point_contact_name<-meta$collab1_point_contact_name[meta$STA_id==meta$STA_id[i]]
      t2$deploy_site<-meta$deploy_site[meta$STA_id==meta$STA_id[i]]
      
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
    track<-track%>%filter(is.na(utc)==FALSE)
    track$cfilter<- sdafilter (track$lat1, track$lon1, track$utc, track$lc, vmax, ang, distlim)
    
    
    #### remove low quality endlocation records accepted by Fritas filter
    track$filtered[(track$cfilter == "end_location" & track$lc <= -2)]="end_location_rem" ## -2 = LC B
    track$STA_id <- rep(meta$STA_id[i], length(track[,1]))
    
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
      geom_text(data=track,aes(x=max(wrap360(lon1)-.5),y=max(lat1)),label=((meta$animal_id[i])))+
      geom_text(data=track,aes(x=max(wrap360(lon1)-.5),y=max(lat1-1)),label=((meta$STA_id[i])))+
      xlab("Longitude")+
      ylab("Latitude")+
      theme_classic()
    
    #### screen output		
    #print(c("TrackLengthOrig",TrackLengthOrig,"Tracklength_clipped",Tracklength_clipped,
    #        "TrackLength_ends_added",TrackLength_ends_added,
    #        "latlong=0",latlon0,"dup removed", 
    #        dupremoved,"filtered",filtered,
    #        "retained",retained))
    
    info<-data.frame(animal_id=meta$animal_id[i],STA_id,vmax,ang[1],ang[2],distlim[1],distlim[2],
                     lcerrref,TrackLengthOrig,Tracklength_clipped,
                     TrackLength_ends_added,latlon0,dupremoved,retained)
    info$animal_id<-as.character(info$animal_id)
    info$lcerrref<-as.character(info$lcerrref)
    
    # bind all info
    INFO<-bind_rows(INFO,info) #was rbind_fill()
    rm(info)
    
    #for tags without extra sensors this adds that column in
    #for tags without extra sensors this adds that column in
    if("sensors.1" %in% names(track)==TRUE){
      track <- track%>%dplyr::select(-sensors.1)
    }
    if("sensors" %in% names(track)==FALSE){
      track$sensors<-NA
    }
    
    track$sensors<-as.character(track$sensors)
    track<-track%>%dplyr::select(-utcF)#gets rid of utc time as a factor
    
    #Removes Tag ID as an identifier
    track$tag_id<-NA
    track<-track%>%dplyr::select(-tag_id)  
    
    if(length(track$UniID_gap)>0){
      track$UniID_gap<-as.character(track$UniID_gap)}
    
    # bind all filtered tracks
    Tracks<-bind_rows(Tracks,track)
    rm(track)}
  
  #Track.Plots1 = Track.Plots[-which(sapply(Track.Plots, is.null))]
  
  Tracks$uid<-1:length(Tracks[,1])
  ouput<-list(Tracks,Track.Plots,INFO, missingbirds)
  names(ouput)<-c("tracks_filt","tf_plots","tf_info","missing")
  return(ouput)
}