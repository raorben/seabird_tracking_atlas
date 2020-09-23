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
  #class(bbindis)<-"estUD"
  return(bbindis)
}
