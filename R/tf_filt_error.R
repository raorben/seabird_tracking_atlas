tf_filt_error<-function(tracks,filt_sum,lcerrors,lcerrref="costa"){
  #INPUTS:
  #tracks is any Freitas filtered track file with the columns 'lc','STA_id','keeps'
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
  
  indiv.error.m<-melt(indiv.error, id=c("STA_id", "lc", "retained_1", "error.prod"), measure = c("error.prod", "retained_1"))
  indiv.error.results<-dcast(indiv.error.m, STA_id ~ variable, sum)
  
  indiv.error.results$mean.error.track<-indiv.error.results$error.prod/indiv.error.results$retained_1
  ## get mean error for the dataset (because the PTT is a factor, the leading zeros throw an error that should be ignored)
  indiv.error.results
  #<-rbind(indiv.error.results,
  #c(0,0,0,mean(indiv.error.results$mean.error.track, na.rm=T)),
  #c(0,0,0,sd(indiv.error.results$mean.error.track,na.rm=T)),
  #c(0,0,0,length(indiv.error.results[,1])))
}

