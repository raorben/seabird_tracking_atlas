tf_filt_sum<-function(tracks){
  require(reshape2)
  #INPUTS:
  #tracks is any Freitas filtered track file with the columns
  #'lc','STA_id','keeps'
  
  tracks$STA_id<-as.factor(tracks$STA_id)
  tracks$lc<-as.factor(tracks$lc)
  tracks$keeps<-as.character(as.logical(tracks$keeps))
  tracks$keeps[tracks$keeps=="TRUE"]<-"retained"
  tracks$keeps[tracks$keeps=="FALSE"]<-"filtered"
  
  # organize data by ptt
  # melt the data
  tracks$meas <- rep(1, length(tracks[,1]))
  tracks.m<-melt(tracks, c("STA_id", "lc","filtered","keeps"),"meas")
  
  # cast the data (make the pivot)
  filter.results<-dcast(tracks.m, STA_id + lc ~ keeps + value , sum)
  return(filter.results)}