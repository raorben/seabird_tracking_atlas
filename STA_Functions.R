# *****************************
## FUNCTIONS for the California Current Telemetry Atlas
## Original Codes written by Bill Henrey 
## Adapted into functions & scrips by Rachael Orben 
# *****************************


# trackfilter -------------------------------------------------------
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

trackfilter<-function(species,
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
  #require(plyr)
  require(ggplot2)
  
  paramwant<-subset(parameters,(spp==species & tag==tagtype))
  
  vmax <- (paramwant$vmaxSA_Tab2_W5ms_x_3SD)    
  print(paste("maximum velocity = ",vmax))
  ang <- c(paramwant$ang1,paramwant$ang2)
  print(paste("angles = ",ang))
  distlim <-  c(paramwant$distlim1,paramwant$distlim2)
  print(paste("distance limit = ",distlim))
  
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
  # loop through birds
  for (i in 1:length(meta[,1])) {			  
    animal.id <- (meta$animal_id[i])
    file_name <- meta$file_name[i]
    ptt_deploy_id <- meta$ptt_deploy_id[i]
    print(animal.id)
    
    #### read in track
    track <- read.table(paste(dir.in,"/",file_name,".csv",sep = ""),header=T, sep=",",strip.white=T)
    print ('Total rows')
    track$utc<-as.character(track$utc)
    TrackLengthOrig<-(length (track[,1]))
    print (length (track[,1]))
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
    
    # initiate track filtered vector
    track$filtered<-rep(0,length(track[,1]))
    
    #### remove all lat-long values = 0
    latlon0=sum(as.numeric(track$lat1 == 0 | track$lon1 == 0))
    track<-track[(track$lat1 != 0 | track$lon1 != 0),]
    print (c('Total rows after latlon=0 removed',length(track[,1])))
    
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
    
    #### screen output		
    print(c("TrackLengthOrig",TrackLengthOrig,"Tracklength_clipped",Tracklength_clipped,
            "TrackLength_ends_added",TrackLength_ends_added,
            "latlong=0",latlon0,"dup removed", 
            dupremoved,"filtered",filtered,
            "retained",retained))
    info<-data.frame(animal.id,ptt_deploy_id,vmax,ang[1],ang[2],distlim[1],distlim[2],
                     lcerrref,TrackLengthOrig,Tracklength_clipped,
                     TrackLength_ends_added,latlon0,dupremoved,retained)
    
    # bind all info
    INFO<-bind_rows(INFO,info) #was rbind_fill()
    rm(info)
    
    track$sensors<-as.character(track$sensors)
    track<-track%>%dplyr::select(-utcF)#gets rid of utc time as a factor
    
    # bind all filtered tracks
    if (i==1) {tracks<-track
    } else {tracks<-bind_rows(tracks,track)}
    rm(track)}
  
  tracks$uid<-1:length(tracks[,1])
  ouput<-list(tracks,Track.Plots,INFO)
  return(ouput)
}






# Sum-Freitas_filtered ---------------------------------------------------------

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




# Sum-Freitas_errorRetained --------------------------------------------

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
  indiv.error.results<-rbind(indiv.error.results,
                             c(0,0,0,mean(indiv.error.results$mean.error.track, na.rm=T)),
                             c(0,0,0,sd(indiv.error.results$mean.error.track,na.rm=T)),
                             c(0,0,0,length(indiv.error.results[,1])))
}






# PolygonPrep ------------------------------------------------------
#### Read in shapefile(s) for indexing data, files = (1) shapefile (2) shapefile with buffer
# lme for selecting data in a given lme
# lme buffer for selecting data that will be processed (rasterized), thus resulting file extent will
# excede boundaries of lme
# to get complete info on projection and fields in shapefile use: ogrInfo(dsn=dsn,layer='lmes.Buf100km')

# 1. Uses clipPolyList.csv to find, project, and make a buffer for a selected polygon
# the buffer size was originally determined by a column in the list 
# (so you can read the file when you load it), but the buffer(km) was added as a specification
# in the function for more flexiabilty

PolygonPrep<-function(rno,clipPolyList=clipPolyList, dir=dir,plot="on",bufferkm=33.6){
  
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
  if (clipPolyList$mult_polygons[rno]==1) {
    clipper<-subset(clipper,(clipper$LME_NAME)==as.character(clipPolyList$poly_want[rno]))
  }
  
  if (clipPolyList$mult_polygons[rno]==1) {
    clipper<-clipper[clipper@data$Territory1 == "Alaska", ]
  }
  
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
  
  CLIPPERS<-list(clipper,clipper_proj,clipperBuff_proj,projWant,clipperName)
  return(CLIPPERS)
}






# PolygonClip ----------------------------------------------------------
## Bill Henry  USGS WERC
## April 20, 2015
##
## For PTT Tracking data:
## 1. flags points a lying within a given polygon
## 
## Inputs
## dir.in: input directory
## dir.out: ouput directory
## indiv.shp.out (optional for shape file output)
## year.shp.out (optional for shape file output)
## species: AOU
## meta: metadata table
## clipPolyList: list of polygon and associated projection to use for clipping, polygons in WGS84 and transformed to appropriate projection in R
## .csv of "All_tracks_and_data_", essentially time/lat/long and filtering status (keeps = binary field)
##
## Outputs
## shapefiles of: individuals track
## shapefiles of: all tracks
## tracksinpoly.csv: annotated list of filtered track data with column for each polygon that holds
## vector with 1=in polygon, 0 = not in polygon
##
## * note cannot overwrite existing shapefiel with writeOGR, so if updating shapefiles - delete previous versions first
##

PolygonClip<-function(all_tracks=tracks,# tracks<-output[[1]] from function SDAFreitas_CCESTA
                             CLIPPERS=CLIPPERS,
                             dir.out=dir,
                             prjtracks="+proj=longlat +ellps=WGS84 +datum=WGS84"){ #default projection: the WGS84 projection that Argos Data is delivered in
  
  #Extraction of CLIPPER List from PolygonPrep_CCESTA
  clipper<-CLIPPERS[[1]]
  clipper_proj<-CLIPPERS[[2]]
  clipperBuff_proj<-CLIPPERS[[3]]
  projWant<-CLIPPERS[[4]]
  clipperName<-CLIPPERS[[5]]
  
  all_tracks$utc<-as.POSIXct(format(strptime(as.character(all_tracks$utc), "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S"), tz = "GMT")
  
  ptt_deploy_ids<-unique(all_tracks$ptt_deploy_id)
  Clipper.Plots<-vector("list",length(ptt_deploy_ids))
  tracks.out<-data.frame()
  
  for (i in 1:length(ptt_deploy_ids)) {
    ptt_deploy_id <- ptt_deploy_ids[i]
    track <- all_tracks[all_tracks$ptt_deploy_id==ptt_deploy_id,]
    year.id <- track$year[1]
    
    print(c("track number",i,"of",length(ptt_deploy_ids)))
    print(c("ptt_deploy_id",ptt_deploy_id))
    
    track.filts <- track[track$keeps==1,]
    TrackLength<-length(track.filts[,1])
    print(c("TrackLength",TrackLength))
    
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
    names(track.filts.sp@data) <- str_replace(string=names(track.filts.sp@data), pattern="in_poly", replacement=as.character(clipperName))
    
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
      geom_text(data=as.data.frame(coordinates(track.filts.sp_proj)),aes(x=max(lon1),y=max(lat1)),label=as.character(ptt_deploy_id))+
      xlab("Longitude")+
      ylab("Latitude")+
      theme_classic()
    
    head(track.filts.sp_proj)
    
    # replace col names require(stringr)
    names(track.filts.sp_proj@data) <- str_replace(string=names(track.filts.sp_proj@data), pattern="in_poly", replacement=as.character(clipperName))
    
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
  return(tracksclipped)
}





# PolygonClip_segmenttime ---------------------------------------------------------

segmentleavetime<-function(hrs=8,#### set hrs for minimum gap in second (converted to sec with time gap used create new segment each time animal leaves and returns in to box)
                                         tracks,
                                         clipperName){
  require(trip)
  #### hrs: NOTE THIS VALUE FOR EACH SPECIES/CLIPPER COMBO THAT YOU RUN - Data retained for Bridge are sensitive to this value and it is not recorded ELSEWHERE
  # create row index
  ridx<-seq(from = 1, to = nrow(tracks), by = 1)
  tracks<-cbind(ridx, tracks)  
  minGap<-3600 * hrs
  tracks$utc<-as.POSIXct(format(strptime(as.character(tracks$utc), "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S"), tz = "GMT")
  
  tracks.want<-subset(tracks,(tracks[,clipperName])==1)
  
  tracks.want[,paste(clipperName,"_id3",sep="")]<-sepIdGaps(tracks.want$ptt_deploy_id, tracks.want$utc, minGap=minGap)
  
  tracks[,paste(clipperName,"_id3",sep="")]<-tracks[,clipperName]
  
  fixes <- match(tracks$ridx,tracks.want$ridx,nomatch=0)
  tracks[,paste(clipperName,"_id3",sep="")][fixes!=0] <- tracks.want[,paste(clipperName,"_id3",sep="")][fixes]
  
  #### create decimal number for deploy ids with more than one entry/exit of polygon
  #### WARNING! this makes 1_1 =1_10, 1_2 = 1_20 etc., original codes, but switch to use character _id3 instead
  tracks[,paste(clipperName,"_id2",sep="")]<-as.numeric(sub("_", ".", tracks[,paste(clipperName,"_id3",sep="")]))
  return(tracks)
}






# Multiple plot function --------------------------------------------------

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# mapcont -   set mapcont to mapcontour ---------------------------------------------------------

#mapcont -   set mapcont to mapcontour
mapcont <- function (x,y,contour,cellsize) {
  x[y<contour]<- x[y<=contour] * cellsize^2; 
  x[y>contour]<-0; return(x)}   # set cellsize        


# SegmentBB ---------------------------------------------------------

segmentBB<-function(ptt, #tracking data
                           clipperName, #e.g. "PACSEA_buff33_coastclip", must match what you have used.
                           CLIPPERS, #output from: PolygonPrep_CCESTA with desired polygon
                           speed,
                           id.out = "99999", # to manually exclude birds or segments "99999" excludes none
                           contour=99.999, # the maximum contour to return, use 99.999 for 100 ud contours
                           sig2=3000,#, the second smoothing parameter was 3000 m (the approximate mean error for PTT locations)
                           cellsize=3000,#(user option)
                           minNo=2,#minimum number of point to run for the bb - important with small clip areas where tracks are cut up when animal enters and leaves a box
                           id.2= "seg",
                           #all=bird.id, 
                           #all_time=bird.id_timevar, 
                           #seg=clip.name_id2 (run segments of track that are in box then sum them based on number of days tracked)
                           #seg_time = tracks in segment by time grouping var (e.g. month, season), all_time = all tracks, but by a time grouping var (e.g. month, season)
                           tagtype="ptt", #Used to define projection of tracking data
                           meta=meta){ #metadata from PTT_metadata_all.csv for the species
  
  require(sp)
  require(maptools)
  require(raster)
  require(rgdal)
  require(proj4)
  require(ggplot2)
  require(rgeos)
  require(SDMTools)
  require(adehabitatHR)
  
  projWant<-CLIPPERS[[4]]
  clipperNamecheck<-CLIPPERS[[5]]
  if (clipperName != clipperNamecheck){print("Clippers don't match, try again")}
  
  #### get extent of clipper buffer and make grid for KD analysis
  clipperBuff_proj<-CLIPPERS[[3]]; clipper_proj<-CLIPPERS[[2]]
  ext<-extent(clipperBuff_proj[1,1])
  grid.lon <- c(ext@xmin, ext@xmin, ext@xmax, ext@xmax)
  grid.lat <- c(ext@ymax, ext@ymin, ext@ymin, ext@ymax)
  grid.loc <- SpatialPoints(cbind(grid.lon, grid.lat))
  rast <- ascgen(grid.loc, cellsize=cellsize)
  
  #plot(rast)  
  #plot(clipper_proj, col="white",add=T)
  #plot(clipperBuff_proj, add=T, border="gray")
  #plot(ext, col="green",add=T)
  
  #### Do you want: all tracking data, or segments in poly?  
  #### TO DO ALL MORE DATA/CODE SCRUBING NEEDED
  if (id.2=="all") {
    # get frequncies from the ptt deploy_id's
    pttIDFreq<-as.data.frame(table(ptt$ptt_deploy_id))
    print("All IDs and number of loctions"); print(pttIDFreq)
    # select tracks > min #
    pttIDFreq<-pttIDFreq[(pttIDFreq$Var1)!=0 & pttIDFreq$Freq>minNo,]
    pttIDFreq<-pttIDFreq[!pttIDFreq$Var1 %in% id.out,] #manually removes ids if needed
    # subset orginal ptt to those w/ > minNo obs
    ptt <- ptt[ptt$ptt_deploy_id %in% pttIDFreq$Var1,]
    #### tally number of relocations
    pttIDFreq.out<-pttIDFreq[(pttIDFreq$Var1)!=0 & pttIDFreq$Freq>minNo,]
    print("Selected IDs and number of loctions"); print(pttIDFreq.out)} 
  
  if (id.2=="all_time") {
    # get frequncies from the ptt deploy_id's
    ptt$ptt_deploy_id_time<-paste0(ptt$ptt_deploy_id,"_",ptt$time.grp.var)
    pttIDFreq<-as.data.frame(table(ptt$ptt_deploy_id_time))
    print("All IDs and number of loctions"); print(pttIDFreq)
    # select tracks > min #
    pttIDFreq<-pttIDFreq[(pttIDFreq$Var1)!=0 & pttIDFreq$Freq>minNo,]
    pttIDFreq<-pttIDFreq[!pttIDFreq$Var1 %in% id.out,] #manually removes ids if needed
    # subset orginal ptt to those w/ > minNo obs
    ptt <- ptt[ptt$ptt_deploy_id_time %in% pttIDFreq$Var1,]
    #### tally number of relocations
    pttIDFreq.out<-pttIDFreq[(pttIDFreq$Var1)!=0 & pttIDFreq$Freq>minNo,]
    print("Selected IDs and number of loctions"); print(pttIDFreq.out)} 
  
  if (id.2=="seg") {
    # get frequncies from the segments - paste(clipperName,'_id2',sep='')
    pttIDFreq<-as.data.frame(table(ptt[,paste(clipperName,'_id2',sep='')]))
    print("All Segment IDs and number of loctions"); print(pttIDFreq)
    # select segments > min #
    pttIDFreq<-pttIDFreq[(pttIDFreq$Var1)!=0 & pttIDFreq$Freq>minNo,]
    pttIDFreq<-pttIDFreq[!pttIDFreq$Var1 %in% id.out,] #manually removes ids if needed
    # subset orginal ptt to those Buffer_id1 w/ > minNo obs
    ptt <- ptt[ptt[,paste(clipperName,'_id2',sep='')] %in% pttIDFreq$Var1,]
    #### tally number of relocations
    pttIDFreq.out<-as.data.frame(table(floor(ptt[,paste(clipperName,'_id2',sep='')])))
    pttIDFreq.out<-pttIDFreq.out[(pttIDFreq.out$Var1)!=0 & pttIDFreq.out$Freq>minNo,]
    print("Selected IDs and number of loctions"); print(pttIDFreq.out)}
  
  if (id.2=="seg_time") {
    Idx<-which(grepl(paste(clipperName,'_id2',sep=''), names(ptt)))
    ptt <- ptt[ptt[,Idx]!=0,]
    clipid<-paste(clipperName,'_id2',sep='')
    ptt$ptt_deploy_id_time<-paste0(ptt[,paste(clipperName,'_id2',sep='')],"_",ptt$time.grp.var)
    # get frequncies from the segments - paste(clipperName,'_id2',sep='')
    pttIDFreq<-as.data.frame(table(ptt$ptt_deploy_id_time))
    print("All Segment IDs and number of loctions"); print(pttIDFreq)
    # select segments > min #
    pttIDFreq<-pttIDFreq[(pttIDFreq$Var1)!=0 & pttIDFreq$Freq>minNo,]
    pttIDFreq<-pttIDFreq[!pttIDFreq$Var1 %in% id.out,] #manually removes ids if needed
    # subset orginal ptt to those Buffer_id1 w/ > minNo obs
    ptt <- ptt[ptt$ptt_deploy_id_time %in% pttIDFreq$Var1,]
    #### tally number of relocations
    pttIDFreq.out<-pttIDFreq[(pttIDFreq$Var1)!=0 & pttIDFreq$Freq>minNo,]
    print("Selected IDs and number of loctions"); print(pttIDFreq.out)}
  
  #### convert ptt data to spatial, points are brought in WGS84
  tracks.sp <- SpatialPointsDataFrame(coords = ptt[c("lon1","lat1")], data = data.frame(utc = ptt$utc))
  
  # define projection, use the WGS84 projection that Argos Data is delivered in
  if (tagtype=="ptt")
  {tracks.sp@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")}
  
  # transform the track to projection selected for ploygon
  tracks.sp_proj<-spTransform(tracks.sp, CRS(projWant))
  projW<-CRS(projWant)
  # select relevant data for kernel density analysis
  date_time <- as.POSIXct(strptime (tracks.sp_proj@data$utc, "%Y-%m-%d %H:%M:%S"), "GMT")
  loc       <- tracks.sp_proj@coords    # projected coords
  
  if (id.2=="all") {burst<- ptt$ptt_deploy_id} #all birds
  if (id.2=="all_time") {burst<- ptt$ptt_deploy_id_time} #all birds, grouped by time variable
  if (id.2=="seg") {burst<- ptt[,paste0(clipperName,'_id2')]} #birds within a polygon
  if (id.2=="seg_time") {burst<- ptt$ptt_deploy_id_time} #birds within a polygon grouped by a time variable
  ptt_deploy_id<-ptt$ptt_deploy_id
  
  # CREATE TRACKS USING YOUR TIME AND LOCATION DATA FOR KERNELBB ANALYSIS
  track <- as.ltraj(loc, date_time, id=ptt_deploy_id, burst = burst, typeII = TRUE)
  
  track.all<-summary(track)
  track.all$id<-as.numeric(as.vector.factor(track.all$id))
  track.all<-track.all[order(track.all$id),]
  days<-(as.numeric(track.all$date.end-track.all$date.begin))/24
  
  tracksums.out<-cbind(track.all, days) 
  tracksums.out$deploy_id<-floor(tracksums.out$id)
  tracksums.out<- merge(tracksums.out, meta, by.x = "deploy_id", by.y = "ptt_deploy_id" )  
  
  # CALCULATE UDBB WITH YOUR SPECIFIED SPEED AND SMOOTHING TERMS 
  bb <- kernelbb(track, sig1=speed, sig2=sig2, grid = rast, byburst=TRUE)  ## speed in m/s use same for each species and MATCH what you'be done for SDA, 
  #image(bb)
  #plot(getverticeshr(bb, 95), add=TRUE, lwd=2)
  #this possibly done b/c BB has prefilter based on speed, we've already done speed distance and angle
  bbvol = getvolumeUD (bb)
  ## Create a list of raster maps containing ud if the pixel is inside the 100% home range and 0 otherwise
  ## NOTE: ud estimates have been scaled by multiplying each value by 9000^2 to make prob vol that sums to 1.00 accross pixel space
  
  ouput<-list(bb,bbvol,tracksums.out,contour,projW,track)
  return(ouput)
}

# # PlotIndividualBB ---------------------------------------------------------
# 
# PlotSegmentBB<-function(SegmentBB, species, clipperName,cellsize,dir){
#   bb<-SegmentBB[[1]]
#   bbvol<-SegmentBB[[2]]
#   contour=SegmentBB[[4]]
#   tag <- names (bb)
#   #udmap <-lapply(bb, function(x) x$ud)
#   #vmap <-lapply(bbvol, function(x) x$ud)   
#   pdf(paste0(dir,"species/",species,"/",species,"_CCESTA_3_",clipperName,"_IndividualBB_plots.pdf"), onefile = TRUE)
#   for (i in 1:length(tag)) {
#     image(bb[[i]], useRasterImage=TRUE,col=c("light grey", topo.colors(40)))
#     #plot(getverticeshr(bb[[i]], 95), add=TRUE)
#     
#     #image(getvolumeUD(bb[[i]]),col = rev(viridis(15)))
#     #temp=mapcont(udmap[[i]],vmap[[i]], contour,cellsize)
#     #image(temp, col=c("light grey", topo.colors(40)))
#     #text(sum(temp))
#     #print(sum(temp))
#     #a<-getverticeshr(bb, 95)
#     #plot(a[[i]], add=TRUE, lwd=1)
#   }
#   dev.off()
# }

# ExportASCII_IndividualBB ---------------------------------------------------------

ExportASCII_SegmentBB<-function(SegmentBB, species, clipperName,cellsize,dir){
  bb<-SegmentBB[[1]]
  bbvol<-SegmentBB[[2]]
  contour<-SegmentBB[[4]]
  projection<-SegmentBB[[5]]
  print(projection)
  tag <- names (bb)
  udmap <-lapply(bb, function(x) x$UD)
  vmap <-lapply(bbvol, function(x) x$UD)
  
  # export ASCII files for ArcMap
  raster.dir.out<-paste(dir,species,"/2_BB_out/", sep = "")
  for (i in 1:length(tag)) {
    temp=mapcont(bb[[i]],bbvol[[i]], contour,cellsize)
    print (tag[i])
    print (sum(temp)) #each should add to 1, or 0.999999
    udsum<-signif(sum(temp), digits=4)
    # directories business
    file.out<-paste(species,"_IndividualBB_",clipperName,"_",contour,"_sum",udsum,"_",tag[i],".asc",sep="")
    export.asc(temp, paste(raster.dir.out,file.out,sep=""))
  }
}

mapcont <- function (x,y,contour,cellsize) {
  x[y<contour]<- x[y<=contour] * cellsize^2;
  x[y>contour]<-0; return(x)}   # set cellsize

# Export.asc from adehabitat ----------------------------------------------
#Clement Calenge <clement.calenge@oncfs.gouv.fr>.

"export.asc" <- function(x, file)
{
  ## verifications
  if (!inherits(x, "asc")) stop("Non convenient data")
  if (substr(file, nchar(file)-3, nchar(file))!=".asc")
    file<-paste(file, ".asc", sep="")
  
  ## Creates the file header
  file.create(file)
  zz<-file(file, "w")
  nc<-paste("ncols", "         ", nrow(x), sep="")
  nl<-paste("nrows", "         ", ncol(x), sep="")
  xll<-paste("xllcorner", "     ",
             attr(x, "xll")-attr(x, "cellsize")/2, sep="")
  yll<-paste("yllcorner", "     ",
             attr(x, "yll")-attr(x, "cellsize")/2, sep="")
  cs<-paste("cellsize", "      ", attr(x, "cellsize"), sep="")
  nas<-paste("NODATA_value", -9999, sep="  ")
  
  ## write to the file
  writeLines(nc, zz)
  writeLines(nl, zz)
  writeLines(xll, zz)
  writeLines(yll, zz)
  writeLines(cs, zz)
  writeLines(nas, zz)
  
  close(zz) ## close the connection
  
  
  ## replace the missing values, adds newlines at the end
  ## of the rows OF THE MAP (so column of the matrix)
  x[is.na(x)]<--9999
  x<-x[,ncol(x):1]
  x<-rbind(x, rep("\n", ncol(x)))
  
  ## ... and sinks to the file
  sink(file, append=TRUE)
  cat(x)
  sink()
  
}




# BBGroupby  - combines individual BBs into groups ------------------------
grping.var="year"

BBGroupby<-function(species,clipperName,
                    SegmentBB, #Output from IndividualBB
                    resolution="3km",
                    contour = 99.999,
                    id.out = c("99999"),# = c("68019a","68022a3") #to exclude birds or segments "99999" excludes none
                    dir,
                    grping.var="year"){ #### desingated a grouping variable, this can be any variable in your metadata (e.i. year, site~year, site~sex~year)
  ## define grouping unit (BUT NOT TIME except Year)
  
  require(adehabitatHR)
  require(SDMTools)
  
  #meta<-meta[meta$species==species,]
  #grp.meta<-data.matrix(meta[grping.var])
  bb<-SegmentBB[[1]]; bbvol<-SegmentBB[[2]]; tracksums<-SegmentBB[[3]]
  
  #### set grouping variable again
  if (grping.var=="year"){ tracksums$grp<-lubridate::year(tracksums$date.begin)}
  #tracksums$grp <-tracksums[grping.var]
  #  tracksums$grp <-paste(tracksums$year, tracksums$site_abbrev, sep="_")
  
  # get unique groups (use tracksums b/c these points are contained in polygon - not all tracks will necessarily be represented in a given polygon)
  (grp.ids<-as.numeric(as.matrix(unique(tracksums$grp))))
  
  #### initialize lists to house data by desired grouping variable (group.uniq)
  # list to house uds normalized by track duration
  ud.grp.ids <- vector ("list", length(grp.ids))
  # list to house raster of individuals/cell
  noindiv.grp.ids <- vector ("list", length(grp.ids))
  # list to house summary data for each year
  summary.grp.ids <- vector ("list", length(grp.ids))
  
  #### loop through groups
  #grp.id <-1
  for (i in 1:length(grp.ids)) {
    grp.id<-grp.ids[i]
    tracksums.want<-tracksums%>%dplyr::filter(grp==grp.id)
    #tracksums.want<-tracksums[which(tracksums$grp==grp.ids[i]),]
    
    # create summary table of # of segments from each track
    track.freq<-tracksums.want%>%dplyr::group_by(deploy_id)%>%dplyr::summarise(Freq=n())
    track.freq$track.grp<-cbind(rep(grp.ids[i],nrow(track.freq)))
    track.freq.old<-as.data.frame(table(tracksums.want$deploy_id))
    
    # initialize lists to house data for segment based on deploy_id
    ud.track <- vector ("list", length(track.freq$deploy_id))
    track.days <- vector ("list", length(track.freq$deploy_id))
    
    #track.grp<-cbind(rep(grp.ids[i],length(track.freq[,1])))
    summary.grp.ids[[i]]<-track.freq
    
    # sum up segments for each track
    # run through track.freq table summing segments >1
    for (j in 1:length(track.freq$deploy_id)) {
      if (track.freq$Freq[j]==1) {
        # operation for only one segment in polygon (track.freq$Freq[j]==1) == TRUE
        
        tracksums.want$burst[tracksums.want$deploy_id==track.freq$Var1[j]]
        # open .asc
        ud.track[[j]] <- import.asc(paste(dir.in.asc,species,"_IndividualBB_",clipperName,"_",contour,"_sum1_",tracksums.want$burst[tracksums.want$deploy_id==track.freq$Var1[j]],".asc", sep = ""), type = "numeric")
        #ud.track[[j]] <- import.asc(paste(dir.in.asc,species,"_IndividualBB_",clipperName,"_",contour,"_",tracksums.want$id[tracksums.want$deploy_id==track.freq$Var1[j]],".asc", sep = ""), type = "numeric")
        
        # get number of track days (in decimal days)
        track.days[[j]]<-tracksums.want$days[tracksums.want$deploy_id==track.freq$Var1[j]]
        print(paste(j,track.freq$Freq[j],sum(ud.track[[j]])))
      } else {
        # operation for multiple segments in polygon (track.freq$Freq[j]>1) == TRUE
        # get multiple segments
        tracksums.want$id[tracksums.want$deploy_id==track.freq$deploy_id[j]]
        
        segs<-tracksums.want$id[tracksums.want$deploy_id==track.freq$deploy_id[j]]
        bursts<-tracksums.want$burst[tracksums.want$deploy_id==track.freq$deploy_id[j]]
        days.segs<-tracksums.want$days[tracksums.want$deploy_id==track.freq$deploy_id[j]]
        # list to house asc for each segment
        ud.segs.new <- vector ("list", length(segs))
        # k=3
        for (k in 1:length(segs)) {
          # open .asc
          #if (k==1)
          K<-as.character(bursts[k])
          
          idx<-which(names(bb)==K)
          image(bb[[idx]], useRasterImage=TRUE,col=c("light grey", topo.colors(40)))
          ud.seg<-bb[[idx]]
          ud.seg <- raster(paste0(dir.in.asc,species,"_IndividualBB_",clipperName,"_contour_sum1_",K,".asc"), type = "numeric")
          #ud.seg <- import.asc(paste0(dir.in.asc,species,"_IndividualBB_",clipperName,"_",contour,"_",segs[k],".asc"), type = "numeric")
          
          #if (k>1)
          #  ud.seg <- import.asc(paste(dir.in.asc,species,"_IndividualBB_",clipperName,"_",contour,"_sum1_",segs[k],".",k,".asc", sep = ""), type = "numeric")
          #ud.seg <- import.asc(paste(dir.in.asc,species,"_IndividualBB_",clipperName,"_",contour,"_",segs[k],".",k,".asc", sep = ""), type = "numeric")
          
          # weigh each segment it's proportion of total hours tracked within the clipperName (Freiberg 20XX paper)
          ud.segs.new[[k]] <- ud.seg*(days.segs[k]/sum(days.segs))
        }
        # normalize divide by max cell value
        ud.track[[j]]<-Reduce("+",ud.segs.new)
        # get number of track days
        track.days[[j]]<-sum(days.segs)
        print(paste(j,k,sum(ud.track[[j]])))
      }
      
    }
    
    ## sum by grouping variable weight by:
    # a) number of days tracked 
    print(paste("grouping variable = ", grping.var," ",grp.ids[grp.id],sep=""))
    
    # multiply new ud by (# of decimal days of each track/sum decimal days all tracks for that year)
    # initialize rasters
    ud.grp.id<-ud.track[[1]]*0  
    noindiv.grp.id<-ud.track[[1]]*0
    # run through all rasters to sum by grouping variable
    for (l in 1:length(ud.track)) {
      # calculate ud weighted by track.days, weigh each track it's proportion of total hours tracked within the clipperName (Freiberg 20XX paper)
      ud.grp.id<-ud.grp.id + (ud.track[[l]])*(track.days[[l]]/sum(unlist(track.days)))
      # calculate the number of individuals per cell (for weighting upon summing of all years)
      indiv<-ud.track[[1]]*0
      indiv[ud.track[[l]]>0] <-1
      noindiv.grp.id<-noindiv.grp.id+indiv  
    }
    print(paste(grp.ids[grp.id],":",sum(ud.grp.id)))
    
    # save ud weighted by time spent per individual (days tracked)
    ud.grp.ids[[grp.id]]<-ud.grp.id
    
    # save sum individuals per cell
    noindiv.grp.ids[[grp.id]]<-noindiv.grp.id
    
    # export as .acs (ASCII = although Arc does not recognize header info), used to import into arc  
    # create output directory, will not replace if already exists
    dir.create(file.path(paste0(dir,species,"/"),"3_Compiled"),showWarnings=TRUE)
    dir.create(file.path(paste0(dir,species,"/3_Compiled/"),clipperName),showWarnings=TRUE)
    dir.create(file.path(paste0(dir,species,"/3_Compiled/",clipperName,"/"),resolution),showWarnings=TRUE)
    
    write.asc(noindiv.grp.ids[[grp.id]], paste(dir,species,"/3_Compiled/",clipperName,"/",resolution,"/",species,"_",grp.ids[grp.id],"_",resolution,"_ni", sep=""),gz=FALSE)
    write.asc(ud.grp.ids[[grp.id]], paste(dir,species,"/3_Compiled/",clipperName,"/",resolution,"/",species,"_",grp.ids[grp.id],"_",resolution,"_bb", sep=""),gz=FALSE)
    
    
  } 
  
  A<-list(ud.grp.ids,noindiv.grp.ids,summary.grp.ids,grp.ids)
  return(A)
}


# # BBGroupby  - combines individual BBs into groups ------------------------
# 
# BBGroupby_noasc<-function(SegmentBB, #Output from IndividualBB
#                     species,clipperName,
#                     resolution="3km",
#                     contour = 99.999,
#                     id.out = c("99999"),# = c("68019a","68022a3") #to exclude birds or segments "99999" excludes none
#                     dir,
#                     grping.var="year"){ #### desingated a grouping variable, this can be any variable in your metadata (e.i. year, site~year, site~sex~year)
#   ## define grouping unit (BUT NOT TIME except Year)
#   
#   require(adehabitatHR)
#   require(SDMTools)
#   
#   #grp.meta<-data.matrix(meta[grping.var])
#   bb<-SegmentBB[[1]]; bbvol<-SegmentBB[[2]]; tracksums<-SegmentBB[[3]]
#   tracksums$bbref<-1:nrow(tracksums)
#   
#   burst<-names(bb)
#   #### set grouping variable again
#   if (grping.var=="year"){ tracksums$grp<-lubridate::year(tracksums$date.begin)}
# 
#   # get unique groups (use tracksums b/c these points are contained in polygon - not a tracks will necessarily be represented in a given polygon)
#   (grp.ids<-as.numeric(as.matrix(unique(tracksums$grp))))
#   
#   #### initialize lists to house data by desired grouping variable (group.uniq)
#   # list to house uds normalized by track duration
#   #ud.grp.ids <- vector ("list", length(grp.ids))
#   # list to house raster of individuals/cell
#   #noindiv.grp.ids <- vector ("list", length(grp.ids))
#   # list to house summary data for each year
#   #summary.grp.ids <- vector ("list", length(grp.ids))
#   
#   #### loop through groups
#   for (k in 1:length(grp.ids)) {
#     
#     tracksums.want<-tracksums[which(tracksums$grp==grp.ids[k]),]
#     
#     # create summary table of # of segments from each track
#     (track.freq<-tracksums.want%>%group_by(deploy_id,grp)%>%
#       summarize(n=n_distinct(burst),minbb=min(bbref),maxbb=max(bbref)))
#     
#         # initialize lists to house data for segment based on deploy_id
#     ud.track <- vector ("list", nrow(track.freq))
#     track.days <- vector ("list", nrow(track.freq))
#     
#     # sum up segments for each track
#     # run through track.freq table summing segments >1
#     for (j in 1:nrow(track.freq)) {
#       if (track.freq$n[j]==1) {
#         # operation for only one segment in polygon
#         bbIndx<-track.freq$minbb[j]
#         ud.track[[j]]<-bb[[bbIndx]]
#         
#         # get number of track days (in decimal days)
#         track.days[[j]]<-tracksums.want$days[tracksums.want$deploy_id==track.freq$deploy_id[j]]
#         paste(paste("bird:",track.freq$deploy_id[j],
#                     "segnum:",track.freq$n[j],
#                     "area:",sum(slot(ud.track[[j]],"data")[,1])))
#       } else {
#         # get multiple segments
#         days.segs<-tracksums.want$days[tracksums.want$deploy_id==track.freq$deploy_id[j]]
#         bbIndx.segs<-seq(from=track.freq$minbb[track.freq$deploy_id==track.freq$deploy_id[j]],
#             to=track.freq$maxbb[track.freq$deploy_id==track.freq$deploy_id[j]])
#         
#         # list to house each segment
#         ud.segs.new <- vector ("list", length(bbIndx.segs))
# 
#         for (k in 1:length(bbIndx.segs)) {
#           bbIndx<-bbIndx.segs[k]
#           ud.seg <- bb[[bbIndx]]
#           # weigh each segment it's proportion of total hours tracked within the clipperName (Freiberg 20XX paper)
#           a<- slot(ud.seg,"data")*(days.segs[k]/sum(days.segs))
#           slot(ud.seg,"data")<-a
#           ud.segs.new[[k]] <- ud.seg
#         }
#         
#         # normalize divide by max cell value
#         ud.track[[j]]<-Reduce("+",ud.segs.new)
#         # get number of track days
#         track.days[[j]]<-sum(days.segs)
#         print(paste(j,k,sum(slot(ud.track[[j]],"data"))))
#       }
#       
#     }
#   }
#     ## sum by grouping variable weight by:
#     # a) number of days tracked 
#     #print(paste("grouping variable = ", grping.var," ",grp.ids[grp.id],sep=""))
#     
#     # multiply new ud by (# of decimal days of each track/sum decimal days all tracks for that year)
#     # initialize rasters
#     
#     ud.grp.id<-ud.track[[1]] 
#     slot(ud.grp.id,"data")<-data.frame(0)
#     
#     noindiv.grp.id<-ud.track[[1]]
#     length(ud.track[[1]])
#     # run through all rasters to sum by grouping variable
#       # calculate ud weighted by track.days, weigh each track it's proportion of 
#       # total hours tracked within the clipperName (Freiberg 20XX paper)
#     udspdf <- estUDm2spixdf(ud.track[[1]]) ## ud is an object of the class estUDm ## Convert it to SpatialPixelsDataFrame
#     
#     resu <- lapply(1:ncol(ud.track), function(i) {
#       ud.track[[i]] * track.days[[i]] / sum(unlist(track.days))
#     })
#     slot(ud.grp.id,"data")<-(slot(ud.track[[l]],"data"))*(track.days[[l]]/sum(unlist(track.days)))
#       # calculate the number of individuals per cell (for weighting upon summing of all years)
#       indiv<-ud.track[[1]]*0
#       indiv[ud.track[[l]]>0] <-1
#       noindiv.grp.id[ud.track[[l]]>0] <-1
#     }
#     for (l in 2:length(ud.track)) {
#       # calculate ud weighted by track.days, weigh each track it's proportion of 
#       # total hours tracked within the clipperName (Freiberg 20XX paper)
#       ud.grp.id<-ud.grp.id + (slot(ud.track[[l]],"data"))*(track.days[[l]]/sum(unlist(track.days)))
#       # calculate the number of individuals per cell (for weighting upon summing of all years)
#       indiv<-ud.track[[1]]*0
#       indiv[ud.track[[l]]>0] <-1
#       noindiv.grp.id<-noindiv.grp.id+indiv  
#     }
#     print(paste(grp.ids[grp.id],":",sum(ud.grp.id)))
#     
#     # save ud weighted by time spent per individual (days tracked)
#     ud.grp.ids[[grp.id]]<-ud.grp.id
#     
#     # save sum individuals per cell
#     noindiv.grp.ids[[grp.id]]<-noindiv.grp.id
#     
#     # export as .acs (ASCII = although Arc does not recognize header info), used to import into arc  
#     # create output directory, will not replace if already exists
#     dir.create(file.path(paste0(dir,species,"/"),"3_Compiled"),showWarnings=TRUE)
#     dir.create(file.path(paste0(dir,species,"/3_Compiled/"),clipperName),showWarnings=TRUE)
#     dir.create(file.path(paste0(dir,species,"/3_Compiled/",clipperName,"/"),resolution),showWarnings=TRUE)
#     
#     write.asc(noindiv.grp.ids[[grp.id]], paste(dir,species,"/3_Compiled/",clipperName,"/",resolution,"/",species,"_",grp.ids[grp.id],"_",resolution,"_ni", sep=""),gz=FALSE)
#     write.asc(ud.grp.ids[[grp.id]], paste(dir,species,"/3_Compiled/",clipperName,"/",resolution,"/",species,"_",grp.ids[grp.id],"_",resolution,"_bb", sep=""),gz=FALSE)
#     
#     
#   } 
#   
#   A<-list(ud.grp.ids,noindiv.grp.ids,summary.grp.ids,grp.ids)
#   return(A)
# }



# Plot A Raster -----------------------------------------------------------

CCESTArasterplot<-function(clipperName,
                           dir,
                           raster.in.dir,
                           rastername,
                           lat1,
                           lat2,
                           lon1,
                           lon2){
  
  require(ggplot2)
  
  w2hr<-map_data('world')
  w2hr_sub<-w2hr[w2hr$region%in%c("USA","Mexico","Canada","Alaska"),]
  states<-map_data('state') 
  states_sub<-states[states$region%in%c("washington","oregon","california","alaska"),]
  
  #convert the raster to points for plotting
  allgrps.indiv.raster<-raster(paste0(dir,raster.in.dir,rastername))
  cellStats(allgrps.indiv.raster,'sum')
  
  proj4string(allgrps.indiv.raster)<-CRS("+proj=aea +lat_1=30 +lat_2=50 +lat_0=40 +lon_0=-125 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  allgrps.indiv.raster.wgs84<-projectRaster(allgrps.indiv.raster,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  map.p <- rasterToPoints(allgrps.indiv.raster.wgs84)
  
  CLIPPERS<-readRDS(file=paste0(dir,"polygons/CCESTA_",clipperName,".rda")) #list(clipper,clipper_proj,clipperBuff_proj,projWant,clipperName)
  clipper<-CLIPPERS[[1]]
  
  #Now make the map
  #Make the points a dataframe for ggplot
  df <- data.frame(map.p)
  head(df)
  colnames(df)<-c("Longitude","Latitude","Density")
  df[df$Density==0,]<-NA
  
  #df$Density.n<-df$Density/max(df$Density,na.rm=TRUE)
  df$Density.n<-sqrt(df$Density)
  df$Density.d<-NA
  df$Density.d[df$Density.n>=0.75 & df$Density.n<=0.95]<-95
  df$Density.d[df$Density.n<=0.75 & df$Density.n>=0.50]<-75  
  df$Density.d[df$Density.n<=0.50 & df$Density.n>=0.25]<-50  
  df$Density.d[df$Density.n<=0.25]<-25  
  
  ggplot(data=df, aes(y=Latitude, x=Longitude)) +
    geom_raster(aes(fill=Density.n)) +
    scale_fill_gradientn(colors=c("#3096C6", "#9EC29F","#F2EE75", "#EE5B2F","#E7292A"),name="Density.n") +
    geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
    geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
    geom_polygon(data=clipper,aes(long,lat,group=group),fill="NA",color="gray",size=.2)+
    theme_bw() +
    coord_equal() +
    coord_fixed(ratio=1.7,xlim = c(lon1,lon2),ylim=c(lat1,lat2))+
    theme(axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16, angle=90),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  #   geom_raster(data=df, aes(y=Latitude, x=Longitude,fill=as.factor(Density.d))) +
  #   scale_fill_manual(values=c("#3096C6", "#9EC29F","#F2EE75", "#EE5B2F"),name="Density.n") +
  #   geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
  #   geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
  #   geom_polygon(data=clipper,aes(long,lat,group=group),fill="NA",color="gray",size=.2)+
  #   theme_bw() +
  #   coord_equal() +
  #   coord_fixed(ratio=1.7,xlim = c((-130),(-160)),ylim=c(52.5,62))+
  #   theme(axis.title.x = element_text(size=16),
  #         axis.title.y = element_text(size=16, angle=90),
  #         axis.text.x = element_text(size=12),
  #         axis.text.y = element_text(size=12),
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank())
  
  filename1 <- sapply(strsplit(rastername, split='.', fixed=TRUE), function(x) (x[1]))
  ggsave(paste0(dir,filename1,".png"))
}


wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}

