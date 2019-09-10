#05072019

install.packages('raster')
install.packages('rgdal')
install.packages('GISTools')
install.packages('mapview')


library(raster)
library(rgdal)
library(GISTools)
library(mapview)


#read data
nlcd<-raster('./raster/nlcd.tif')
plot(nlcd)
nlcd

pophu<-readOGR('./shapefile',layer='y2010_51_pophu')
plot(pophu)
crs(pophu)


#reprojecting
pophu_p <- spTransform(pophu, crs(nlcd))

#accesing data and adding column area
mydata<-pophu_p@data

mydata<-cbind(mydata,newid=1:nrow(mydata))


#calculate housing density
blockarea<-gArea(pophu_p,byid = TRUE)/1e6
hd<-pophu_p$HOUSING10/blockarea

mydata<-cbind(mydata,housingden=hd)
pophu_p@data<-mydata

#reclassify nlcd .: Vegetation (code=1) and non-vegetation (code=0). 
nlcd[nlcd==41|nlcd==42|nlcd==43|nlcd==52|nlcd==71|nlcd==90|nlcd==95]=1
nlcd[nlcd!=1]=0
plot(nlcd)

mapview(nlcd)

# Calculate percent of vegetation within each census block
pophu_raster <- rasterize(pophu_p, nlcd, field = pophu_p$newid)
plot(pophu_raster)

#Use zonal statistics, zonal(), to calculate percent of vegetation within each census block. 
veg_mean<-zonal(nlcd, pophu_raster, fun='mean')
veg_mean
colnames(veg_mean)

# Change column name 'zone' to 'newid'
colnames(veg_mean)[1]<-'newid'
colnames(veg_mean)[2]<-'pveg'

head(veg_mean)

# Join veg_mean to pophu_p. 
output<-merge(pophu_p,veg_mean,by='newid')

# recode these NA value as -9999 
output$pveg[is.na(output$pveg)] <- -9999
mapview(output,zcol='pveg')

# export the polygon
writeOGR(output,'.','output',driver = 'ESRI Shapefile',overwrite_layer = T)

# Identify census blocks as WUIs (i.e., with >= 50% of vegetation cover AND > 6.17 housing units/ km2)
wui<-output[output$pveg>0.5&output$ housingden>6.17,]


# : Plot wui layer and export it as a shapefile 
mapview(nlcd) + mapview(wui)
plot(nlcd)
plot(wui,add=TRUE)
writeOGR(obj=wui, dsn=".", layer='wui', driver="ESRI Shapefile",overwrite_layer=T)

###################################################################################################

#lab questions

# Question 1:For Montgomery County, how many census blocks are identified as wildland-urban interface? 
count_wui<- length(wui)


# Question 2: Please generate a WUI map for Montgomery County
nlcdsub <- mask(nlcd, pophu_p)
plot(nlcdsub)
plot(wui,add=TRUE)

install.packages('prettymapr')
library(prettymapr)
addnortharrow(pos = "topright", padin = c(0.1, 0.1), scale = 0.5,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")

addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
######
# Question 3: Please revise your r script and generate a WUI map for Albemarle County. Housing unit data is available through tabblock2010_51_pophu.shp. NLCD for entire VA is included in your raster folder: nlcd_va_utm17

#read data
nlcd<-raster('./raster/nlcd_va_utm17.tif')

pophu<-readOGR('./shapefile',layer='tabblock2010_51_pophu')

#filtering the Albemarle County
pophu_s<-pophu[pophu$COUNTYFP10=='003',]
plot(pophu_s)
mapview(pophu_s)

#reprojecting
pophu_p <- spTransform(pophu_s, crs(nlcd))

#clipping the nlcd
nlcd<-crop(nlcd,extent(pophu_p))
plot(nlcd)
plot(pophu_p,add=T)
mapview(nlcd)+mapview(pophu_p)

#accesing data and adding column area
mydata<-pophu_p@data

mydata<-cbind(mydata,newid=1:nrow(mydata))


#calculate housing density
blockarea<-gArea(pophu_p,byid = TRUE)/1e6
hd<-pophu_p$HOUSING10/blockarea

mydata<-cbind(mydata,housingden=hd)
mydata<-cbind(mydata,area=blockarea)
pophu_p@data<-mydata

#reclassify nlcd .: Vegetation (code=1) and non-vegetation (code=0). 
nlcd[nlcd==41|nlcd==42|nlcd==43|nlcd==52|nlcd==71|nlcd==90|nlcd==95]=1
nlcd[nlcd!=1]=0
plot(nlcd)

mapview(nlcd)

# Calculate percent of vegetation within each census block
pophu_raster <- rasterize(pophu_p, nlcd, field = pophu_p$newid)
plot(pophu_raster)

#Use zonal statistics, zonal(), to calculate percent of vegetation within each census block. 
veg_mean<-zonal(nlcd, pophu_raster, fun='mean')
veg_mean
colnames(veg_mean)

# Change column name 'zone' to 'newid'
colnames(veg_mean)[1]<-'newid'
colnames(veg_mean)[2]<-'pveg'

head(veg_mean)

# Join veg_mean to pophu_p. 
output<-merge(pophu_p,veg_mean,by='newid')

# recode these NA value as -9999 
output$pveg[is.na(output$pveg)] <- -9999
mapview(output,zcol='pveg')

# export the polygon
writeOGR(output,'.','output_Albemarle',driver = 'ESRI Shapefile',overwrite_layer = T)

# Identify census blocks as WUIs (i.e., with >= 50% of vegetation cover AND > 6.17 housing units/ km2)
wui<-output[output$pveg>0.5&output$ housingden>6.17,]


# : Plot wui layer and export it as a shapefile 
mapview(nlcd) + mapview(wui,zcol='pveg')+mapview(wui,zcol='housingden')+mapview(output)
#count and map
count_wui<-length(wui)
plot(mask(nlcd,pophu_p))
plot(wui,add=TRUE)

library(prettymapr)
addnortharrow(pos = "topright", padin = c(0.1, 0.1), scale = 0.5,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")

addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")


writeOGR(obj=wui, dsn=".", layer='wui_Albemarle', driver="ESRI Shapefile",overwrite_layer=T)





