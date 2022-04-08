library(sf)
library(dplyr)

require(rgdal)
require(ggplot2)
#makes a polygon for the West Coast Region: Cape Mendicino north to limit of WC EEZ.

#http://www.marineregions.org/downloads.php World EEZ v10 (2018-02-21, 119 MB) 
nc <- st_read(dsn = "/Users/rachaelorben/Downloads/USMaritimeLimitsAndBoundariesSHP/USMaritimeLimitsNBoundaries.shp")
names(nc)
class(nc)
length(nc)
extent(nc)
crs(c)
nc$TYPE
nc<-nc%>%dplyr::select(-FEAT_TYPE,-PUB_DATE,-APPRV_DATE,-AOR,-NOTE,
                       -SUPP_INFO,-TS,-F_EEZ,-SYMBOL,-LEGAL_AUTH,-CZ,-EEZ,-UNILATERAL)
st_crs(nc)
nc_df<-data.frame(nc)
US<-nc%>%dplyr::filter(REGION=="Pacific Coast")%>%
  dplyr::filter(OBJECTID!=1000 & OBJECTID!=998 & OBJECTID!=997& OBJECTID!=982& 
                  OBJECTID!=1198 & OBJECTID!=1091)%>%select(-REGION)%>%
  filter(BOUND_ID!="B0055" & BOUND_ID!="B0098")%>%select(-BOUND_ID)
#unique(US$BOUND_ID)
unique(US$OBJECTID)
plot(US['OBJECTID'])
#plot(US['BOUND_ID'])
plot(US)

WestEZZ<-US
st_geometry_type(US)
WestEZZ.c<-st_combine(WestEZZ)

ggplot(WestEZZ.c) +
  geom_sf(aes(geometry = geometry))
df2 = data.frame(st_coordinates(WestEZZ[,1]))
head(df2)
df2$L1<-as.factor(df2$L1)
df2$L2<-as.factor(df2$L2)
ggplot()+
  geom_path(data=df2,aes(x=X,y=Y,group=L2,color=L2))+facet_wrap(~L2)

df2$order<-as.character(df2$L2)
df2$order[df2$L2==8]<-2
df2$order[df2$L2==4]<-3
df2$order[df2$L2==9]<-4
df2$order[df2$L2==6]<-5
df2$order[df2$L2==7]<-6
df2$order[df2$L2==5]<-7
df2$order[df2$L2==2]<-8
df2$order[df2$L2==3]<-9
df2$order[df2$order==6 & df2$X<(-125)]<-"6.1"
df2$order[df2$order==6 & df2$X>(-125)]<-"6.2"

ggplot()+
  geom_path(data=df2,aes(x=X,y=Y,group=L2,color=as.factor(order)))+
  facet_wrap(~order)

df2$order<-as.numeric(df2$order)
data.61<-df2%>%filter(order==6.1)
data.61<-rev(data.61)
df3<-df2%>%filter(order!=6.1)
df4<-rbind(df3,data.61)

df5<-df4%>%arrange(order)

ggplot()+
  geom_path(data=df5,aes(x=X,y=Y,group=L2,color=as.factor(order)))

ggplot()+
  geom_path(data=df5%>%filter(order==4 | order ==5 | order ==6.1 | order==6.2),
             aes(x=X,y=Y,group=L2,color=as.factor(order)))
ggplot()+
  geom_path(data=df5%>%filter(order!=5),aes(x=X,y=Y,group=L2,color=as.factor(order)))

ggplot()+
  geom_point(data=df5%>%filter(order==7)%>%filter(X<(-118.6632))%>%filter(Y>33.7),
                                                  aes(x=X,y=Y),color="black")+
  geom_point(data=df5%>%filter(order==7)%>%filter(X>(-118.6478)),aes(x=X,y=Y),color="pink")


-118.6632
33.73049

data=df5%>%filter(order==7)%>%filter(X<(-118.6478))%>%filter(Y>33.7)

WestEZZ.c[1]
x = st_polygonize(US)
plot(US, col = 'grey')
title("multilinestring")
plot(x, col = 'grey')
title("polygon")


#this pulls out the biggest polygon - all the others are little and are probably islands
colnames(df5)<-c("lon","lat","G1","G2")
a<-data.frame(df5)

x_coord <- a$lon
y_coord <- a$lat


WC_eez <-  cbind(x_coord,y_coord) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 4326) %>%
  st_sf() 

plot(WC_eez)



plot(st_geometry(WC_eez))

#WC_eez6<-st_buffer(WC_eez5,dist = .1)
#plot(st_geometry(WC_eez6))
