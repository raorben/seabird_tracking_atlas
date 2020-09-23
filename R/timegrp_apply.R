timegrp_apply<-function(tracks_filt,
                        timegrp){
  require(lubridate)
  if(timegrp=="all"){
    tracks_filt$uniID<-paste0(tracks_filt$STA_id,"_",year(tracks_filt$utc),"_",timegrp)
    print(unique(tracks_filt$uniID))
    print(length(unique(tracks_filt$uniID)))
    return(tracks_filt)
  } else if (timegrp=="year"){
    tracks_filt$uniID<-paste0(tracks_filt$STA_id,"_",year(tracks_filt$utc),"_",timegrp)
    print(unique(tracks_filt$uniID))
    print(length(unique(tracks_filt$uniID)))
    return(tracks_filt)
  } else if (timegrp=="season"){
    tracks_filt$month<-month(tracks_filt$utc)
    tracks_filt$season<-NA
    tracks_filt$season[tracks_filt$month==3 |tracks_filt$month==4 |tracks_filt$month==5 ]<-"spring"
    tracks_filt$season[tracks_filt$month==6 |tracks_filt$month==7 |tracks_filt$month==8 ]<-"summer"
    tracks_filt$season[tracks_filt$month==9 |tracks_filt$month==10 |tracks_filt$month==11 ]<-"fall"
    tracks_filt$season[tracks_filt$month==12 |tracks_filt$month==1 |tracks_filt$month==2 ]<-"winter"
    tracks_filt$uniID<-paste0(tracks_filt$STA_id,"_",year(tracks_filt$utc),"_",tracks_filt$season)
    print(unique(tracks_filt$uniID))
    print(length(unique(tracks_filt$uniID)))
    return(tracks_filt)
  } else {
    print("new grouping, please revise codes by adding a new else if level")
    return(tracks_filt)
  }
}
