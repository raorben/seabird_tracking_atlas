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