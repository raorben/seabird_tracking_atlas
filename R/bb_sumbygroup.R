bb_sumbygroup<-function(bbindis,
                        tracksums.out){
  (grp.ids<-names(bbindis))
  
  grp.estUDm<-vector("list", length(grp.ids))
  for (h in 1:length(grp.ids)) {
    
    # multiply new ud by (# of decimal days of each track/sum decimal days all tracks for that group)
    estUDm<-bbindis[[h]]
    
    (track_info<-tracksums.out%>%
        filter(timegrp==grp.ids[[h]])%>%
        group_by(uniID)%>%
        dplyr::summarize(n=n_distinct(seg_id), days=sum(days)))
    
    (alldays<-sum(track_info$days))
    
    ## ud is an object of the class estUDm ## Convert it to SpatialPixelsDataFrame
    udspdf <- estUDm2spixdf(estUDm)
    ## Convert the original map to fullgrid (i.e. SpatialGridDataFrame)
    fullgrid(udspdf) <- TRUE 
    
    # run through all rasters to sum by grouping variable
    #     # calculate ud weighted by track.days, weigh each track it's proportion of
    #     # total hours tracked within the clipperName (Freiberg 20XX paper)
    #ncol(udspdf) is specifying processing for each individual (1 column / individual)
    densitys <- lapply(1:ncol(udspdf), function(i) {
      udspdf[[i]] * track_info$days[[i]] / alldays
    })
    names(densitys)<-track_info$uniID #add ids back on - otherwise it looks crazy
    
    densitys <- as.data.frame(densitys)
    head(densitys)
    head(den_sum<-rowSums(densitys))
    den_sum <- as.data.frame(den_sum)
    udspdf@data <- den_sum
    fullgrid(udspdf) <- FALSE
    #image(udspdf["densitys"])
    
    #reclass as estUDm
    re <- lapply(1:ncol(udspdf), function(i) {
      so <- new("estUD", udspdf[,i])
      so@h <- list(h=0, meth="specified")
      so@vol <- TRUE
      return(so)})
    
    names(re) <- names(udspdf)
    class(re) <- "estUDm"
    image(re)
    
    grp.estUDm[[h]]<-re
  }
  names(grp.estUDm)<-grp.ids
  class(grp.estUDm) <- "estUDm" 
  return(grp.estUDm)
}


