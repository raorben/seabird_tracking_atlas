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
  if (j==1) polys<-estUD.con
  if (j>1) polys<-bind(polys,estUD.con)
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

a<-TSUM%>%group_by(timegrp,clipper,species)%>%
  summarise(nBirds=n_distinct(uniID),
            nSegs=n_distinct(seg_id))




# count number of sp:sp shared grid cells ---------------------------------
sp_polyies<-names(estUDvol_list)
str()


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
  

SP_polyct%>%filter(poly=="Oregon")%>%filter(poly=="Oregon")

SP_polyct.sum<-SP_polyct%>%group_by(x,y)%>%
  filter(is.na(layer)==FALSE)%>%
  filter(poly=="Oregon")%>%
  pivot_wider(id_cols = c(x,y), 
              names_from = c(sp),
              values_from = c("layer"))

SP_polyct.sum$group<-1:nrow(SP_polyct.sum)

SP_polyct.sum.grp<-SP_polyct.sum%>%pivot_longer(cols = BFAL:WEGU,
                             names_to = c("sp"),
                             values_to = "layer")

pairwise_count(SP_polyct.sum.grp%>%filter(is.na(layer)==FALSE), sp, group)


SP_polyct$YN<-SP_polyct$layer
SP_polyct$YN[is.na(SP_polyct$YN)==TRUE]<-0
SP_polyct$YN[SP_polyct$YN>0]<-1

SP_polyct %>% 
  select(-poly, -layer,-x,-y) %>% 
  spread(value = YN,key=sp) %>% 
  {crossprod(as.matrix(.))} %>% 
  `diag<-`(0)

food_combination <- SP_polyct %>% group_by(sp)%>%
  pull(YN) %>%
  unique() %>%
  combn(2) %>%
  t() %>%
  as_tibble() %>%
  mutate(count = map2_int(V1, V2, 
                          ~sum(apply(SP_polyct.sum %>% select(.x, .y), 1, sum) == 2)))
# apply the function 
count_combos(SP_polyct, 
             group_col1="sp", group_col2="sp", count_col="YN")


nam <- c("IDNum",paste0("Var",1:6))
n <- 5
set.seed(23)
dat <- setNames(data.frame(1:n,replicate(6,sample(0:1,n,replace=TRUE))),nam)
data.frame(table(dat[-1]))

ggplot()+
  geom_point(data=PFSH,aes(x=x,y=y,color=PFSH.l),size=.2)+
  geom_point(data=oBP,aes(x=x,y=y),color="red",alpha=.3,size=.2)
  
BFAL<-rename(BFAL,"BFAL.l"="layer")
PFSH<-rename(PFSH,"PFSH.l"="layer")

sp<-full_join(BFAL,PFSH,by=c("x"="x","y"="y"))

oBP<-sp%>%filter(is.na(BFAL.l)==FALSE & is.na(PFSH.l)==FALSE)

unique(BFAL$layer)
plot(r2)
lines(p)
}
# identify species combinations for the grid cells with 2,3,& 4 sp --------



# example input
df1 <- read.table(text = "
                  Gene      BRCA         THYM         TGHJ
                  ACC         23          21           7
                  XTG         12          13           9
                  CFG         45          4            8", header = TRUE)


library(ggplot2)
library(dplyr)
library(tidyr)

df1 <- read.table(text = "
Gene      BRCA         THYM         TGHJ
ACC         23          21           7
XTG         12          13           9
CFG         45          4            8", header = TRUE)

plotDat <- gather(df1, key = "Gene2", value = "value", -Gene)

ggplot(plotDat, aes(Gene, Gene2, col = value, fill = value, label = value)) +
  geom_tile() +
  geom_text(col = "black") +
  theme_minimal() +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red") +
  scale_color_gradient2(low = "white", mid = "yellow", high = "red")


category <- c('food','food','food','food','food','food','edibles','edibles','edibles','edibles', 'edibles')
location <- c('houston, TX', 'houston, TX', 'las vegas, NV', 'las vegas, NV', 'philadelphia, PA', 'philadelphia, PA', 'austin, TX', 'austin, TX', 'charlotte, NC', 'charlotte, NC', 'charlotte, NC')
item <- c('apple', 'banana', 'apple', 'pear', 'apple', 'pear', 'pear', 'apple', 'apple', 'pear', 'banana')

food_data <- data.frame(cbind(category, location, item), stringsAsFactors = FALSE)

library(tidyverse)

food_data2 <- food_data %>%
  mutate(count = 1) %>%
  spread(item, count, fill = 0) 

food_combination <- food_data %>%
  pull(item) %>%
  unique() %>%
  combn(2) %>%
  t() %>%
  as_tibble() %>%
  mutate(count = map2_int(V1, V2, 
                          ~sum(apply(food_data2 %>% select(.x, .y), 1, sum) == 2)))

# View the result
food_combination
# A tibble: 3 x 3
V1     V2 count
<chr>  <chr> <int>
  1  apple banana     2
2  apple   pear     4
3 banana   pear     1


library("dplyr")

# a function to apply to `food_data` from the original post 
count_combos <- function(df, group_col1, group_col2, count_col){ 
  
  # use `combn()` to get all the unique pairs from the `$items` col
  combos <- t(combn(sort(unique(df[[count_col]])), 2)) %>% 
    as_data_frame() %>% 
    # initialize an empty column to catch the counts 
    mutate(count=NA)
  
  # create a new df from the colnames passed as args, 
  # (it would be more general to just use the dplyr evaluation system (@_@))
  df <- data_frame(
    group_col1 = df[[group_col1]],
    group_col2 = df[[group_col2]],
    count_col  = df[[count_col]]
  )
  # for each combo of the grouping vars, get a pipe-seperated string of items
  df <- df %>% 
    group_by(group_col1, group_col2) %>% summarize(
      items = paste(unique(count_col), collapse="|")
    ) %>% ungroup()
  
  # for each item pair/combo, get the number of rows of `df` with both items 
  combos$count <- sapply(1:nrow(combos), function(x){
    sum(grepl(combos$V1[x], df$items) & grepl(combos$V2[x], df$items))
  })
  # and return it in a nice df
  return(combos)
}


require(utils)

expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50),
            sex = c("Male","Female"))

x <- seq(0, 10, length.out = 100)
y <- seq(-1, 1, length.out = 20)
d1 <- expand.grid(x = x, y = y)
d2 <- expand.grid(x = x, y = y, KEEP.OUT.ATTRS = FALSE)
object.size(d1) - object.size(d2)
##-> 5992 or 8832 (on 32- / 64-bit platform)


require(data.table) ## 1.9.4+
set.seed(1L)        ## For reproducibility
N = 2724098L
motif = sample(paste("motif", 1:1716, sep="_"), N, TRUE)
id = sample(83509, N, TRUE)
DT = data.table(id, motif)

motif_pairs <- combn(unique(dat$motif), 2)
colnames(motif_pairs) <- apply(motif_pairs, 2, paste, collapse = " ")
motif_pair_counts <- apply(motif_pairs, 2, function(motif_pair) {
  sum(daply(dat[dat$motif %in% motif_pair, ], .(id), function(dat_subset){
    all(motif_pair %in% dat_subset$motif)
  }))
})
motif_pair_counts <- as.data.frame(unname(cbind(t(motif_pairs), motif_pair_counts)))
names(motif_pair_counts) <- c("motif1", "motif2", "count")
motif_pair_counts



df <- stack(cladelist) %>%
  dplyr::rename(clade = "ind", artifact = "values")
df %>%
  widyr::pairwise_count(feature = clade, item = artifact) %>%
  filter(item1 > item2) %>%
  mutate(Pair = paste(item1, item2, sep = "/")) %>%
  dplyr::select(Pair, Count = n) 



library(widyr)

dat <- tibble(group = rep(1:5, each = 2),
              letter = c("a", "b",
                         "a", "c",
                         "a", "c",
                         "b", "e",
                         "b", "f"))

# count the number of times two letters appear together
pairwise_count(dat, letter, group)
pairwise_count(dat, letter, group, sort = TRUE)
pairwise_count(dat, letter, group, sort = TRUE, diag = TRUE)
