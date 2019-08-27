#notes section--------------

# Date:27082019
# 
# Project url of prof:
#   http://128.173.134.106:3838/vamap/
#   http://128.173.134.106:3838

# shiny for web mapp creations

# for r markdown goto File->New R notebook
# for html export select Save->Kirt to html

#Book for reference -> Introdution to R for Spatial Analysis and Mapping

#end of notes --------------------

# user R version > 3.5.0
install.packages('raster')
install.packages('rgdal')
install.packages('mapview')

library(raster)
library(rgdal)
library(mapview)

#setting working directory
setwd("C:/Users/GeogLabCommonAccess/Desktop/Inclass_export/r_class1")

#reading data and ploting
elevation<-raster('elev.tif')
plot(elevation)

ndvi<-raster('ndvi.tif')
plot(ndvi)


#interactive map over leaflet
mapview(elevation)

#vectors in R
X<-c(1,2,3,-4)
Y<-c('vt','geography','cnre')
Y[2]
Y[1:2]

#matrices in R
m <- matrix(c(1,2,3,3,2,1),nrow=3,ncol=2)
#column
m[,1]


#dataframe
n <- c(2, 3, 5)  
s <- c('aa', 'bb', 'cc')  
b <- c(TRUE, FALSE, TRUE)  
df = data.frame(n, s, b)
#sepcific column access
df$n


#r plots
# random numbers
x1<-rnorm(100)
y1<-rnorm(100)
plot(x1,y1)

#pch-defiens the symbol
plot(x1,y1,pch=1,col='red')
plot(x1,y1,pch=2,col='red')


x2<- seq(0,6,len=100)
y2<- sin(x2)
plot(x2,y2,type='l',lwd=3,col='darkgreen')

y2r<- y2+rnorm(100,0,0.1)
points(x2,y2r,pch=16,col='darkred')
