#12092019
# 30m resolution


## set working directory here, rest is good to run
setwd("D:/R labs/forestfire_lab3")

#install required packages
install.packages('GISTools')
install.packages('mapview')
install.packages('raster')
install.packages('rdgal')


#load library
library(GISTools)
library(raster)
library(rgdal)
library(mapview)

#read required data and reporject
elev<-raster('./elev.tif')
crs(elev)

ndvi<-raster('./ndvi.tif')
crs(ndvi)

landcover<-raster('./landcover.tif')
crs(landcover)

roads<-readOGR('.','roads')
crs(roads)

#viewing the layers
# plot(ndvi)
# plot(elev)
# plot(landcover)
# plot(roads)


##variables
#NDVI >=200 :1 or 0
#landcover [8-13]:1 or 0
#slope >35:1 or 0
# aspect south west is vulnerable
# 150m from road is vulnerable
# Areas within 500 meters water bodies are of low concern



#preparing weight layers
# 1 ndvi
ndvi_w<-ndvi
ndvi_w[ndvi_w<200]<-0
ndvi_w[ndvi_w!=0]<-1
plot(ndvi_w,main="ndvi filtered")

# 2 landcover
landcover_w<-landcover
landcover_w[landcover_w>=0 & landcover_w<=7]<-0
landcover_w[landcover_w>=8 & landcover_w<=13]<-1
plot(landcover_w,main="landcover filtered")

#3 water
water<-landcover
water[water!=1]<-NA
water_b<-buffer(water,500)
water_b[is.na(water_b)]<-0
plot.new()
plot(water_b,main="water with buffer filtered")

#4 slope
slope<-terrain(elev, opt='slope',unit = 'degrees')
slope_w<-slope
slope_w[slope_w<=35.0]<-0
slope_w[slope_w!=0]<-1
plot(slope_w,main="slope filtered")

#5 aspect
aspect<-terrain(elev, opt='aspect',unit = 'degrees')
aspect_w<-aspect

aspect_w[aspect_w>=202.5 & aspect_w<=247.5]<-999
aspect_w[aspect_w!=999]<-0
aspect_w[aspect_w==999]<-1
plot(aspect_w,main="aspect filtered")


#6 road buffer
buffer<-buffer(roads,150)
# plot(buffer)
buffer_r<-rasterize(buffer,landcover)
buffer_r[is.na(buffer_r)]<-0
plot(buffer_r,main="road buffer")

#combining all
weighted_sum<-ndvi_w + landcover_w + water_b + slope_w + aspect_w + buffer_r
plot(weighted_sum,main="final vulnerability index map")


#mapview display
mapview(landcover)+mapview(elev)+mapview(roads)+mapview(ndvi)+
  mapview(ndvi_w)+mapview(landcover_w)+mapview(water_b)+mapview(slope_w)+mapview(water)+mapview(aspect_w)+mapview(buffer_r)+
  mapview(weighted_sum)

writeRaster(weighted_sum,filename = 'firevul_output', format='GTiff',overwrite=TRUE)
