#spatial stuff
library(adehabitatHR)
library(SDMTools)
library(raster)
library(sp)
library(marmap)

#data manipulation
library(stringr)
library(dplyr)
library(trip) #segmentleavetime
library(lubridate)
library(tidyr)
library(widyr)

#plotting
library(ggplot2) #tracksclipped
library(gridExtra)#for pdfs
library(cowplot)#for multi-panel plots

rm(list=ls()) #empty environment

# set directories
if(Sys.info()[7]=="rachaelorben") {dir<-"/Users/rachaelorben/Research/SeabirdTrackingAtlas/"} ##RAO
if(Sys.info()[7]=="rachaelorben") {gitdir<-"/Users/rachaelorben/git_repos/seabird_tracking_atlas/"}

#loads functions
files.sources = list.files(paste0(gitdir,"R"), full.names = TRUE)
sapply(X = files.sources, FUN=source)

#background maps for plotting
states<-map_data('state') 
states_sub<-states[states$region%in%c("washington","oregon","california","alaska"),]
or_sub<-states[states$region%in%c("oregon"),]
w2hr<-map_data('world')
w2hr_sub<-w2hr[w2hr$region%in%c("Canada"),]

#"BRAC","WEGU", saving these for later!
species<-c("BFAL","PFSH","SOSH","STAL","NOFU","COMU","RTLO","PALO","BRAC","LAAL","WEGU")

grps<-data.frame(timegrp="all",clipperName=c("PNW_wUSEEZ","Oregon_wUSEEZ"))

estUDvol_list <- vector(mode = "list", length = length(species)*2)
names(estUDvol_list) <- c(paste0(species,"_PNW_wUSEEZ"),paste0(species,"_Oregon_wUSEEZ"))

SP_Contours<-NULL
Polys_Over<-NULL
for (i in 1:nrow(grps)){
  clipperName<- grps$clipperName[i]
  timegrp<- grps$timegrp[i]

  clipper_list<-readRDS(file=paste0(dir,"polygons/",clipperName,".rda"))
  projWant<-clipper_list$projWant
  clip<-fortify(spTransform(clipper_list$clipper_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))
  buf<-fortify(spTransform(clipper_list$clipperbuff_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))


estUD_list <- vector(mode = "list", length = length(species))
names(estUD_list) <- species

for (k in 1:length(species)){
  sp<-species[k]
  estUD_list[[sp]]<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_3_groups.rda"))
  }

CON<-NULL
polys<-NULL
for (j in 1:length(species)){
  sp<-species[j]
  estUD<-estUD_list[[sp]]
  estUD$all$den_sum@proj4string<-CRS(projWant)
  estUD$all$den_sum@vol = FALSE
  image(estUD)
  estUD.ud.vol <- getvolumeUD(estUD$all$den_sum, standardize=TRUE)
  estUD.con<-getverticeshr(estUD.ud.vol,percent = 50)
  estUD.con@data$id<-sp

  estUD.con.wgs84<-spTransform(estUD.con,CRS=CRS("+proj=longlat +ellps=WGS84"))
  
  sp_poly<-paste0(sp,"_",clipperName)
  estUDvol_list[[sp_poly]]<-estUD.con.wgs84
  
  dat<-fortify(estUD.con.wgs84)
  dat$species<-sp
  CON<-rbind(CON,dat)
  if (j==1) polys<-estUD.con.wgs84
  if (j>1) polys<-bind(polys,estUD.con.wgs84)
  }

#grouping variables for ggplot
CON$sp_piece<-paste0(CON$species,"_",CON$piece)
CON$clip_grp<-paste0(clipperName,"_",timegrp)

r<-polyCount(polys,Res = 0.01)
f<-as.data.frame(r,xy = TRUE)
f$clip_grp<-paste0(clipperName,"_",timegrp)
head(f)

SP_Contours<-rbind(SP_Contours,CON)
Polys_Over<-rbind(Polys_Over,f)
}



# Oregon EEZ plots --------------------------------------------------------
clipperName<-"Oregon_wUSEEZ"
clipper_list<-readRDS(file=paste0(dir,"polygons/",clipperName,".rda"))
projWant<-clipper_list$projWant
clip<-fortify(spTransform(clipper_list$clipper_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))
buf<-fortify(spTransform(clipper_list$clipperbuff_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))

A<-ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=SP_Contours%>%filter(clip_grp=="Oregon_wUSEEZ_all"),
               aes(x=long,y=lat,group=sp_piece,fill=species,color=species),alpha=.2)+
  geom_path(data=clip,aes(x=long,y=lat), color="grey50")+
  geom_path(data=buf,aes(x=long,y=lat), color="grey75")+
  coord_fixed(ratio=1.7,xlim = c((-130),(-122)),ylim=c(41,47))+
  labs(fill = "Species", color="Species") +
  xlab("Longitude")+
  ylab("Latitude")+
  theme_classic()

B<-ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_tile(data = Polys_Over%>%
              filter(is.na(layer)==FALSE)%>%
              filter(clip_grp=="Oregon_wUSEEZ_all") , 
            aes(x = x, y = y, fill = as.factor(layer))) +
  scale_fill_viridis_d(direction = (-1)) +
  geom_path(data=clip,aes(x=long,y=lat), color="grey50")+
  geom_path(data=buf,aes(x=long,y=lat), color="grey75")+
  labs(fill = "Core Area\nOverlap\n(# species)")+
  coord_fixed(ratio=1.7,xlim = c((-130),(-122)),ylim=c(41,47))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_classic()

quartz(width=10,height=6)
OR<-grid.arrange(A,B,nrow=1)
ggsave(OR,filename = paste0(dir,"Allspecies","Oregon_wUSEEZ_","all",".png"),width=10,height=6, dpi = 300)

# PNW  --------------------------------------------------------------------
clipperName<-"PNW_wUSEEZ"
clipper_list<-readRDS(file=paste0(dir,"polygons/",clipperName,".rda"))
projWant<-clipper_list$projWant
clip<-fortify(spTransform(clipper_list$clipper_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))
buf<-fortify(spTransform(clipper_list$clipperbuff_proj,CRS=CRS("+proj=longlat +ellps=WGS84")))


C<-ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=SP_Contours%>%
                 filter(clip_grp=="PNW_wUSEEZ_all"),
               aes(x=long,y=lat,group=sp_piece,fill=species,color=species),alpha=.2)+
  #scale_color_viridis_d(direction = (-1)) +
  #scale_fill_viridis_d(direction = (-1)) +
  geom_path(data=clip,aes(x=long,y=lat), color="grey50")+
  geom_path(data=buf,aes(x=long,y=lat), color="grey75")+
  labs(fill = "Species", color="Species") +
  coord_fixed(ratio=1.7,xlim = c((-130),(-122)),ylim=c(40,49))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_classic()


D<-ggplot()+
  geom_polygon(data=w2hr_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_polygon(data=states_sub,aes((long),lat,group=group),fill="grey60",color="grey60",size=0.1)+
  geom_tile(data = Polys_Over%>%
              filter(is.na(layer)==FALSE)%>%
              filter(clip_grp=="PNW_wUSEEZ_all") , 
            aes(x = x, y = y, fill = as.factor(layer))) +
  scale_fill_viridis_d(direction = (-1)) +
  labs(fill = "Core Area\nOverlap\n(# species)")+
  geom_path(data=clip,aes(x=long,y=lat), color="grey50")+
  geom_path(data=buf,aes(x=long,y=lat), color="grey75")+
  coord_fixed(ratio=1.7,xlim = c((-130),(-122)),ylim=c(40,49))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_classic()

quartz(width=10,height=6)
PNW<-grid.arrange(C,D,nrow=1)
ggsave(PNW,filename = paste0(dir,"Allspecies","PNW_wUSEEZ","_","all",".png"),width=10,height=6, dpi = 300)


# sample sizes inside polys -----------------------------------------------
TSUM<-NULL
for (i in 1:nrow(grps)){
  clipperName<- grps$clipperName[i]
  timegrp<- grps$timegrp[i]  
  
  for (k in 1:length(species)){
    sp<-species[k]
    segments<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_1_segments.rda"))
    segments<-readRDS(file=paste0(dir,"species/",sp,"/",sp,"_",clipperName,"_",timegrp,"_bb_1_segments.rda"))
    tracksums.out<-segments$tracksums.out
    tracksums.out$species<-sp
    tracksums.out$clipper<-clipperName
    tracksums.out$timegrp<-timegrp
    TSUM<-rbind(TSUM,tracksums.out)
    }
}

(a<-TSUM%>%group_by(timegrp,clipper,species)%>%
  summarise(nBirds=n_distinct(uniID),
            nSegs=n_distinct(seg_id)))




# count number of sp:sp shared grid cells ---------------------------------
sp_polyies<-names(estUDvol_list)


SP_polyct<-NULL
for (i in 1:length(sp_polyies)){
  sp_poly<-sp_polyies[i]
  n<-str_split(sp_poly,pattern='_')
  sp<-n[[1]][1]
  polname<-n[[1]][2]
    
  polyCt_raster<-r
  sp_con<-estUDvol_list[[i]]
  
  r2 = mask(polyCt_raster,sp_con)
  r2.df<-as.data.frame(r2,xy=TRUE)
  r2.df$sp<-sp
  r2.df$poly<-polname
  SP_polyct<-rbind(SP_polyct,r2.df)
}
  

# Oregon ------------------------------------------------------------------
SP_polyct.sum_OR<-SP_polyct%>%group_by(x,y)%>%
  filter(is.na(layer)==FALSE)%>%
  filter(poly=="Oregon")%>%
  pivot_wider(id_cols = c(x,y), 
              names_from = c(sp),
              values_from = c("layer"))

SP_polyct.sum_OR$group<-1:nrow(SP_polyct.sum_OR)

SP_polyct.sum.grp_OR<-SP_polyct.sum_OR%>%pivot_longer(cols = BFAL:WEGU,
                             names_to = c("sp"),
                             values_to = "layer")

SP_polyct.SG_OR<-SP_polyct.sum.grp_OR%>%
  filter(is.na(layer)==FALSE)%>%
  ungroup()%>%
  select(-x,-y)
                                                    
SP_polyct.SG_counts_OR<-pairwise_count(SP_polyct.SG_OR, sp, group)


# NCC ---------------------------------------------------------------------

SP_polyct.sum_NCC<-SP_polyct%>%group_by(x,y)%>%
  filter(is.na(layer)==FALSE)%>%
  filter(poly=="PNW")%>%
  pivot_wider(id_cols = c(x,y), 
              names_from = c(sp),
              values_from = c("layer"))

SP_polyct.sum_NCC$group<-1:nrow(SP_polyct.sum_NCC)

SP_polyct.sum.grp_NCC<-SP_polyct.sum_NCC%>%pivot_longer(cols = BFAL:WEGU,
                                                      names_to = c("sp"),
                                                      values_to = "layer")

SP_polyct.SG_NCC<-SP_polyct.sum.grp_NCC%>%
  filter(is.na(layer)==FALSE)%>%
  ungroup()%>%
  select(-x,-y)

SP_polyct.SG_counts_NCC<-pairwise_count(SP_polyct.SG_NCC, sp, group)

SP_polyct.SG_counts_NCC<-SP_polyct.SG_counts_NCC%>%filter(item1!="BFAL")
SP_polyct.SG_counts_NCC<-SP_polyct.SG_counts_NCC%>%filter(item1!="BFAL")

quartz(width=5,height=4)
ggplot()+
  geom_tile(data=SP_polyct.SG_counts_NCC, 
            aes(x=item1, y=item2, col = n, fill = n, label = n)) +
  geom_text(col = "black") +
  scale_fill_gradient2(name="Count",low = "white", mid = "yellow", high = "red") +
  scale_color_gradient2(name="Count",low = "white", mid = "yellow", high = "red")+
  geom_abline(intercept = 0, slope = 1)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,vjust = .8),
        axis.title = element_blank())
quartz.save(file = paste0(dir,"PairedSpecies","NCC_wUSEEZ","_","all",".png"), dpi = 300)

quartz(width=5,height=4)
ggplot()+
  geom_tile(data=SP_polyct.SG_counts_OR, 
            aes(x=item1, y=item2, col = n, fill = n, label = n)) +
  geom_text(col = "black") +
  scale_fill_gradient2(name="Count",low = "white", mid = "light blue", high = "dark blue") +
  scale_color_gradient2(name="Count",low = "white", mid = "light blue", high = "dark blue")+
  geom_abline(intercept = 0, slope = 1)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,vjust = .8),
        axis.title = element_blank())
quartz.save(file = paste0(dir,"PairedSpecies","_OR_wUSEEZ","_","all",".png"),dpi = 300)

# groups of three ---------------------------------------------------------
SP_polyct.SG34_OR<-SP_polyct.SG_OR%>%filter(layer>2)
SP_polyct.SG34_OR$poly<-"OR"
SP_polyct.SG34_NCC<-SP_polyct.SG_NCC%>%filter(layer>2)
SP_polyct.SG34_NCC$poly<-"NCC"
SP_polyct.SG34<-rbind(SP_polyct.SG34_OR,SP_polyct.SG34_NCC)

grps<-unique(SP_polyct.SG34$group)
plys<-unique(SP_polyct.SG34$poly)

multi_grps<-NULL
for (k in 1:length(plys)){
  SP_polyct.SG34.p<-SP_polyct.SG34%>%filter(poly==plys[k])
  
for (i in 1:length(grps)){
  g<-grps[i]
  indis<-SP_polyct.SG34.p%>%filter(group==g)
  indis<-indis%>%arrange(sp)
  n=nrow(indis)
  sps<-paste(indis$sp, collapse = "_")
  dat<-data.frame(sp_group=sps,gridcell=g,n=n,poly=plys[k])
  multi_grps<-rbind(multi_grps,dat)
}    
}

multi_grps_sum<-multi_grps%>%filter(n==3)%>%
  group_by(sp_group)%>%
  summarise(n=n())

quartz(height=5,width=7)
ggplot()+
  geom_histogram(data=multi_grps%>%filter(n>2),
                 aes(x=sp_group,color=poly,fill=poly),stat="count",position="dodge" )+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90,vjust = .1),
        axis.title = element_blank())
quartz.save(file = paste0(dir,"MultiGroupOverlapSpecies","_","all",".png"),dpi = 300)

data=multi_grps%>%filter(n>2)
sps<-as.character(unique(data$sp_group))
unique(unlist(stringr::str_split(sps,pattern="_")))

data=multi_grps%>%filter(n==4)
sps<-as.character(unique(data$sp_group))
unique(unlist(stringr::str_split(sps,pattern="_")))

