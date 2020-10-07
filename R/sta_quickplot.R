sta_quickplot<-function(bbgroups,
                        clipper_list=clipper_list,
                        dir,
                        species,
                        tracks_inpoly.df,
                        bathy2){
  
  
  require(ggplot2)
  require(cowplot)
  require(gridExtra)
  library(RColorBrewer)
  states<-map_data('state') 
  states_sub<-states[states$region%in%c("washington","oregon","california","alaska"),]
  w2hr<-map_data('world')
  w2hr_sub<-w2hr[w2hr$region%in%c("Canada"),]
  
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
    
    rf <- writeRaster(allgrps.indiv.raster.clip, filename=paste0(dir,"species/",sp,"/",grp.ids[h],"_",sp,"_",clipper_list$clipperName,".asc"), datatype='ascii', overwrite=TRUE)
    
    # take clipped raster and reproject in WGS84 for viz
    allgrps.indiv.raster.clip.wgs84<-projectRaster(allgrps.indiv.raster.clip,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    # make into points
    bb.df.wgs84 <- data.frame(rasterToPoints(allgrps.indiv.raster.clip.wgs84))
    colnames(bb.df.wgs84)<-c("Longitude","Latitude","Density")
    
    # take sqrt of Density to better highlight high use areas
    bb.df.wgs84$Density[bb.df.wgs84$Density==0]<-NA
    bb.df.wgs84$Density.n<-sqrt(bb.df.wgs84$Density)
    
    
    A<-ggplot() +
      geom_raster(data=fortify(bb.df.wgs84)%>%dplyr::filter(Density.n>0.0001), aes(y=Latitude, x=Longitude,fill=Density.n)) +
      scale_fill_gradientn(colors=c("#3096C6", "#9EC29F","#F2EE75", "#EE5B2F","#E7292A"),name="Density") +
      geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
      geom_polygon(data=states_sub,aes((long),lat,group=group),fill="black",color="grey60",size=0.1)+
      geom_polygon(data=clipper_wgs84,aes(long,lat,group=group),fill="NA",color="black",size=.5)+
      theme_bw() +
      xlab("Longitude")+
      ylab("Latitude")+
      coord_fixed(ratio=1.7,xlim = c(-130,-121),ylim=c(39,48.3))+
      #guides(fill = guide_colorbar(barwidth = 2, barheight = 10,direction = "vertical"))+
      theme(axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16, angle=90),
            axis.text.x = element_text(size=8),
            axis.text.y = element_text(size=8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position=c(.84,.16),
            #panel.background=element_rect(fill="transparent",colour=NA),
            legend.background = element_rect(fill = "transparent",colour = "transparent"),
            #legend.key = element_rect(fill = "transparent", colour = "transparent"),
            legend.text=element_text(color="white", size=8),
            legend.title = element_text(color="white"))
    
    colourCount = nrow(unique(tracks_filt%>%dplyr::select(STA_id)))+1
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    
    B<-ggplot() +
      geom_tile(data=bathy2,aes(x=wrap360(lon),y=V2,fill=Depth))+
      scale_fill_gradientn(colours = c("grey95", "grey65"),name="Depth") +
      geom_polygon(data=w2hr_sub,aes(wrap360(long),lat,group=group),fill="black",color="grey60",size=0.1)+
      geom_polygon(data=states_sub,aes(wrap360(long),lat,group=group),fill="black",color="grey60",size=0.1)+
      geom_path(data=tracks_inpoly.df%>%filter(timegrp==names(bbgroups)[h]),
                aes(x=wrap360(lon1),y=lat1,group=STA_id,color=as.factor(STA_id)),size=0.2)+
      scale_color_manual(values = getPalette(colourCount))+
      geom_polygon(data=clipper_wgs84,aes(wrap360(long),lat,group=group),fill="NA",color="black",size=.5)+
      annotate("text",x=wrap360(-129.8),y=48,label=sp,color="white",size=10,hjust = 0)+
      annotate("text",x=wrap360(-123.7),y=42.8,label="Bird sample size",color="white",size=5,hjust = 0)+
      xlab("Longitude")+
      ylab("Latitude")+
      coord_fixed(ratio=1.7,xlim = c(wrap360(-130),wrap360(-121)), ylim=c(39,48.3))+
      scale_x_continuous(breaks=c(230,232.5,235,237.5),
                         labels=c("-130.0", "-127.5", "-125.0","-122.5"))+
      theme_bw() +
      theme(axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16, angle=90),
            axis.text.x = element_text(size=8),
            axis.text.y = element_text(size=8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none")
    
    tracks_inpoly.df$month<-month(tracks_inpoly.df$utc)
    tracks_inpoly.df$date<-date(tracks_inpoly.df$utc)
    
    birdsMO<-unique(tracks_inpoly.df%>%dplyr::select("uniID","timegrp","in_poly","date","month"))
    birdsMO.dys<-tracks_inpoly.df%>%group_by(uniID, timegrp,in_poly,month)%>%
      summarise(n=n_distinct(date))
    birdsMO.dys1<-birdsMO.dys%>%
      group_by(in_poly,timegrp,month)%>%
      summarise(mean=mean(n),sd=sd(n))
    
    sample.size.inset<-ggplot()+
      geom_bar(data=unique(birdsMO%>%filter(timegrp==names(bbgroups)[h])%>%
                             dplyr::select("uniID","month","in_poly")), 
               aes(x=month, group=in_poly,fill=as.factor(in_poly)))+
      scale_fill_manual(values = c("grey80","lightblue"))+
      geom_point(data=birdsMO.dys1%>%filter(timegrp==names(bbgroups)[h]), 
                 aes(y=mean, x=month), color="black", size=.4)+
      geom_errorbar(data=birdsMO.dys1%>%filter(timegrp==names(bbgroups)[h]),
                    aes(x=month, ymin=mean-sd, ymax=mean+sd), 
                    width=.3,position=position_dodge(.9))+
      scale_x_continuous(limits =c(1,12),breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                         labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
      theme_classic()+
      theme(axis.title=element_blank(),
            axis.text = element_text(color="white", size=6),
            axis.ticks = element_line(color="white"),
            axis.line = element_line(color="white"),
            plot.background = element_blank(),#odd box around plots
            panel.background = element_rect(fill = "grey30"),
            #plot.background = element_rect(color="transparent",fill="grey50"),
            legend.position = "none",
            #panel.background = element_rect(fill = "transparent"),
            strip.background = element_blank())+
      facet_wrap(~in_poly,ncol = 1, scales="free_y")
    
    B.with.inset <-
      ggdraw() +
      draw_plot(B) +
      draw_plot(sample.size.inset, x = 0.66, y = .15, width = .3, height = .3)
    
    plotAB<-gridExtra::grid.arrange(B.with.inset ,A,ncol=2)
    ggsave(plotAB,filename = paste0(dir,"species/",sp,"/",grp.ids[h],"_",sp,"_",clipper_list$clipperName,".png"),width=12,height=10, dpi = 300)
    saveRDS(plotAB,paste0(dir,"species/",sp,"/",grp.ids[h],"_",sp,"_",clipper_list$clipperName,".rds"))
    
  }
}
