#10092019


#dim() to check the dimensions of matrices
# help() to acess help

#subsetting matrix <- dem[500:1000,500:1000]


########################################################################

#calcuate mean elevation and slope at census bock level

install.packages('raster')
install.packages('rgdal')
install.packages('GISTools')
install.packages('mapview')


library(raster)
library(rgdal)
library(GISTools)
library(mapview)

#read data
blocks<-readOGR('.',layer='cbg')
plot(blocks)
crs(blocks)

#read raster
dem<-raster('./dem.tif')
plot(dem)
crs(dem)
#reproject raster
dem_p<-projectRaster(dem,res=30,crs=crs(blocks))
crs(dem_p)

##zonal statistics
#rasterize
blocks@data<-cbind(blocks@data,newid=1:nrow(blocks@data))
blocks_r<- rasterize(blocks, dem_p, field = blocks$newid)
plot(blocks_r)

#creat slope
slope<-terrain(dem_p, opt='slope',unit = 'degrees')
plot(slope)
plot(blocks,add=T)

#Use zonal statistics, zonal(), to calculate percent of vegetation within each census block. 
ele<-zonal(dem_p, blocks_r, fun='mean')
colnames(ele)[1]<-'newid'
colnames(ele)[2]<-'elevation'
ele<-data.frame(ele)

slo<-zonal(slope,blocks_r,fun='mean')
colnames(slo)[1]<-'newid'
colnames(slo)[2]<-'slope'
slo<-data.frame(slo)

#merging the columns
output<-merge(blocks,ele,by='newid')
output<-merge(output,slo,by='newid')

valid_output<-output[!is.na(output$elevation) & !is.na(output$slope),]
mapview(valid_output,z='elevation')+mapview(valid_output,z='slope')+mapview(output)


#writing output
writeOGR(output,'.','output',driver = 'ESRI Shapefile',overwrite_layer = T)


#aspect
aspect<-terrain(dem_p,opt="aspect",unit='degrees')
plot(aspect)
