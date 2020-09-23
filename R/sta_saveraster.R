sta_saveraster<-function(bbgroups,
                         clipper_list=clipper_list,
                         dir,
                         species){
  require(raster)
  
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
    
    # crop raster to clipper
    r2 <- crop(allgrps.indiv.raster, extent(clipper_proj))
    allgrps.indiv.raster.clip <- raster::mask(r2, clipper_proj)
    
    ## Check that it worked
    #plot(allgrps.indiv.raster.clip)
    #plot(clipper_proj, add=TRUE, lwd=2)
    
    rf <- writeRaster(allgrps.indiv.raster.clip, 
                      filename=paste0(dir,"species/",sp,"/",grp.ids[h],"_",sp,"_",clipper_list$clipperName,".asc"), 
                      datatype='ascii', overwrite=TRUE)}
  
}
