bb_countindinum_gridcell<-function(bbindis,
                                   clipper_list=clipper_list){
  (grp.ids<-names(bbindis))
  
  hab<-clipper_list$rast
  fullgrid(hab)<-TRUE
  hab@data[hab@data==0]<-1
  
  for (h in 1:length(grp.ids)) {
    
    estUDm<-bbindis[[h]]
    for (j in 1:length(estUDm)){
      
      udspdf <- estUDm2spixdf(bbindis[[h]]) ## ud is an object of the class estUDm ## Convert it to SpatialPixelsDataFrame
      fullgrid(udspdf) <- TRUE ## Convert the original map to fullgrid (i.e. SpatialGridDataFrame)
      
      #sum so all individuals equal one
      resu <- lapply(1:ncol(udspdf), function(i) {
        udspdf[[i]] * hab[[1]] / sum(udspdf[[i]] * hab[[1]])
      })
      sum(resu[[1]],resu[[2]])
      resu <- as.data.frame(resu)
      names(resu) <- names(udspdf@data)
      names(resu)
      head(resu)
      resu[resu>0]<-1
      numindi<-rowSums(resu)
      #str(udspdf)
      #head(udspdf@data)
      udspdf@data[udspdf@data>0]<-1
      numindi<-rowSums(udspdf@data)
    }
    re <- as.data.frame(numindi)
    udspdf@data <- re
    estUDm[[h]]<- udspdf@data
    image(udspdf["numindi"],zlim = c(0,20),border = grey(1))
  }
}



