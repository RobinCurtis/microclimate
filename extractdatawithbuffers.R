library(sf)
library(raster)
library(rgdal)
library(maptools)
library(parallel)

detectCores()
mc.cores=36


list.files("C:/Users/robcu/Desktop/Mfmicro/") 
list.files("C:/Users/robcu/Desktop/Mfmicro/MfwebsMar2018points")
dtm <- raster("C:/Users/robcu/Desktop/Mfmicro/PredBMarch2018DEM.tif")
SRdtm <- raster("C:/Users/robcu/Desktop/Mfmicro/SR_PredBMarch2018.tif")

buffers <- st_read("C:/Users/robcu/Desktop/Mfmicro/randombuffers.gpkg")

butterflypoints <- st_read("C:/Users/robcu/Desktop/Mfmicro/MfwebsMar2018points/MFwebsMarch2018.shp")
#randompoints <- st_read("C:/Users/robcu/Desktop/Mfmicro/predBrandompoints/randompoints.shp")
randompoints <- st_read("C:/Users/robcu/Desktop/Mfmicro/predBrandomlandpoints/PredBrandomland.shp")

randombuffer<-extract(dtm,randompoints, buffer=0.2, fun=mean)
butterflybuffer<-extract(dtm,butterflypoints, buffer=0.2, fun=mean)
r <- data.frame(randombuffer,butterflybuffer)
boxplot(r$randombuffer,r$butterflybuffer)
t.test(r$randombuffer,r$butterflybuffer, paired=T)
boxplot(r)

#SR example
randombuffer<-extract(SRdtm,randompoints, buffer=0.2, fun=mean)
butterflybuffer<-extract(SRdtm,butterflypoints, buffer=0.2, fun=mean)
r <- data.frame(randombuffer,butterflybuffer)
boxplot(r$randombuffer,r$butterflybuffer)
t.test(r$randombuffer,r$butterflybuffer, paired=T)
boxplot(r)

aspect <- terrain(SRdtm, opt="aspect", unit='degrees')
plot(aspect)
hist(aspect)
hist(a$butterflyaspect)
randomaspect<-extract(aspect,randompoints, buffer=0.2, fun=mean)
butterflyaspect<-extract(aspect,butterflypoints, buffer=0.2, fun=mean)
a <- data.frame(randomaspect,butterflyaspect)
boxplot(a)
t.test(a$randomaspect,a$butterflyaspect, paired=T)

###
aspect <- terrain(dtm, opt="aspect", unit='degrees')
plot(aspect)
hist(aspect)
hist(a$butterflyaspect)
randomaspect<-extract(aspect,randompoints, buffer=0.2, fun=mean)
butterflyaspect<-extract(aspect,butterflypoints, buffer=0.2, fun=mean)
a <- data.frame(randomaspect,butterflyaspect)
boxplot(a)
t.test(a$randomaspect,a$butterflyaspect, paired=T)
