################################
# Global inventory incompletness
# for butterflies 
################################

# load required libraries
library(WriteXLS)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(maptools)
library(dplyr)
library(raster)
library(vegan)
library(data.table)

# source required functions
source("/mnt/data1tb/Dropbox/Lepidopteraglobal/scripts/functions.R")

latlon<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
world<-readShapePoly("/mnt/data1tb/Dropbox/Lepidopteraglobal/shapefiles/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp",proj4string=latlon)
world<-spTransform(world,CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))

# read in data
spec1<-fread("/mnt/data1tb/Dropbox/Lepidopteraglobal/data/butterflydataonly.csv")

#########################
# Data cleaning
#########################

# Filter 1: basis of record
exclude<-c("FOSSIL_SPECIMEN","LITERATURE","LIVING_SPECIMEN","MACHINE_OBSERVATION",
"MATERIAL_SAMPLE")
spec2<-spec1[!spec1$basisofrecord %in% exclude,]

# Filter 2: only records with year and day
spec3<-subset(spec2,!is.na(year))
spec4<-subset(spec3,!is.na(day))
spec4$counter<-1

# Filter 3: exclude duplicates
duplicates<-ddply(spec4,.(decimallatitude,decimallongitude,year,day,species),nrow)
#write.csv(duplicates,file="/mnt/data1tb/Dropbox/Lepidopteraglobal/data/duplicates.csv",row.names=F)
#spec1<-data.frame(species=unique(spec4$species))
#write.csv(spec1,file="/mnt/data1tb/Dropbox/Lepidopteraglobal/data/spec1.csv",row.names=F)

# Filter 4: taxonomic uncertainty

###########################
# Trend analysis by country
###########################

# aggregate data by countries
countrydat<-aggregate(counter~year+countrycode,data=spec4,FUN=sum)

# only data from 1900 onwards
countrydat<-subset(countrydat,year > 1889)

# trends
u0016<-trendsmg(inputdf=countrydat,min=1900,max=2016)
u5016<-trendsmg(inputdf=countrydat,min=1950,max=2016)
u50<-trendsmg(inputdf=countrydat,min=1900,max=1950)
u70<-trendsmg(inputdf=countrydat,min=1950,max=1970)
u90<-trendsmg(inputdf=countrydat,min=1970,max=1990)
u16<-trendsmg(inputdf=countrydat,min=1990,max=2016)

# create data frames for ggplot2
u0016df<-dfcreatemg(inputdf=u0016,inputmap=world)
u5016df<-dfcreatemg(inputdf=u5016,inputmap=world)
u50df<-dfcreatemg(inputdf=u50,inputmap=world)
u70df<-dfcreatemg(inputdf=u70,inputmap=world)
u90df<-dfcreatemg(inputdf=u90,inputmap=world)
u16df<-dfcreatemg(inputdf=u16,inputmap=world)

u0016df$period<-"1900-2016"
u5016df$period<-"1950-2016"
u50df$period<-"1900-1950"
u70df$period<-"1950-1970"
u90df$period<-"1970-1990"
u16df$period<-"1990-2016"

# combine data together
combined<-rbind(u0016df,u5016df,u50df,u70df,u90df,u16df)

# create discrete legend 
combined1<-legrank(inputdf=combined,ntiles=5,colname=c("trend"))

# re-arrange factor levels
combined1$rankd<-factor(combined1$rankd,rev(levels(combined1$rankd)[c(5,2,1,3,4)]))
combined1$period<-factor(combined1$period,levels(as.factor(combined1$period))[c(2,4,1,3,5,6)])

# create maps and save
mypal<-brewer.pal(5,"Spectral")

p1<-ggplot(combined1)+aes(long,lat,group=group,fill=rankd) +geom_polygon() +geom_path(color="black",size=0.1) +coord_equal()+theme(legend.title=element_blank())+theme(panel.background = element_rect(fill =NA , colour = NA))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(axis.text.x = element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank())+labs(x ="",y="")+facet_wrap(~period,scales="free",ncol=2)+theme(plot.margin=unit(c(0,0,0,0),"mm"))+scale_fill_manual(values=mypal)

ggsave(filename="/mnt/data1tb/Dropbox/Lepidopteraglobal/figures/trends.JPG",plot =p1,width=9,height=7)
 
###########################
# Inventory incompleteness
##########################

# read in data
grid1<-raster("/mnt/data1tb/Dropbox/Lepidopteraglobal/grids/grid100km.asc")
grid2<-raster("/mnt/data1tb/Dropbox/Lepidopteraglobal/grids/grid500km.asc")

# polygon for overlay
world1<-dfcreatemg(inputmap=world)

# calculate incompletness
results100km<-compsac(inputdf=spec4,inraster=grid1)
results500km<-compsac(inputdf=spec4,inraster=grid2)

# combine data frame together
results100km$resolution<-"110x110km resolution"
results500km$resolution<-"550x550km resolution"

combined<-rbind(results500km,results100km)

# create discrete legend
combined1<-legrank(inputdf=combined,ntiles=6,colname=c("slope"))

# create maps and save
mypal<-(brewer.pal(6,"Spectral"))

p2<-ggplot(data = combined1, aes(x = x, y = y, fill = rankd)) +geom_raster()+coord_fixed(1.1)+labs(x ="",y="")+theme(axis.text.x = element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank())+theme(plot.margin=unit(c(0,0,0,0),"mm"))+theme(legend.title=element_blank())+scale_fill_manual(values=mypal)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(strip.text.x = element_text(size=12, face="bold"),strip.background = element_rect())+theme(panel.background = element_rect(fill =NA , colour = NA))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+geom_polygon(data=world1,aes(long,lat,group=group), colour="black", fill="white",alpha=0,size=0.2)+facet_wrap(~resolution,ncol=1)+theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

ggsave(filename="/mnt/data1tb/Dropbox/Lepidopteraglobal/figures/incompletness.JPG",plot =p2,width=7,height=7)


