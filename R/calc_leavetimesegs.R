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
  
  tracks.want$id3<-trip::sepIdGaps(tracks.want$uniID, 
                                   tracks.want$utc, 
                                   minGap=minGap)
  
  tracks$id3<-tracks$in_poly
  
  fixes <- match(tracks$ridx,tracks.want$ridx,nomatch=0)
  tracks$id3[fixes!=0] <- tracks.want$id3[fixes]
  
  tracks$seg_id<-tracks$id3
  
  #remove variables used for this function
  tracks<-tracks%>%dplyr::select(-id3,ridx)
  
  tracks<-tracks%>%filter(seg_id!=0)
  return(tracks)
}
