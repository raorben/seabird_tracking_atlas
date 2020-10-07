unwrap360<-function(lon360){
  lon<-ifelse(lon360 > 180, -360 + lon360, lon360)
  return(lon)}