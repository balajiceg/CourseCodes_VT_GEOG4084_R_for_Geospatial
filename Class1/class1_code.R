#notes section--------------

# Date:27082019
# 
# Project url of prof:
#   http://128.173.134.106:3838/vamap/
#   http://128.173.134.106:3838

# shiny for web mapp creations

# for r markdown goto File->New R notebook
# for html export select Save->Kirt to html

#end of notes --------------------

# user R version > 3.5.0
install.packages('raster')
install.packages('rgdal')
install.packages('mapview')

library(raster)
library(rgdal)
library(mapview)

#setting working directory
setwd("C:/Users/GeogLabCommonAccess/Desktop/Class1")

#reading data and ploting
elevation<-raster('elev.tif')
plot(elevation)

ndvi<-raster('ndvi.tif')
plot(ndvi)


#interactive map over leaflet
mapview(elevation)

#vectors in R
X<-c(1,2,3,-4)



#r plots
# random numbers
x1<-rnorm(100)
y1<-rnorm(100)
plot(x1,y1)

#pch-defiens the symbol
plot(x1,y1,pch=1,col='red')
plot(x1,y1,pch=2,col='red')


