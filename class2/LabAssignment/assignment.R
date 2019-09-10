# change r markdown to r
# library(knitr)
# purl("Class2/LabAssignment/assignment.Rmd")

## ------------------------------------------------------------------------
install.packages('rgdal')
install.packages('raster')	
install.packages("GISTools")
install.packages('rgeos')
install.packages('maptools')
install.packages('mapview')


## ------------------------------------------------------------------------
library(raster)
library(rgdal)
library(GISTools)
library(rgeos)
library(maptools)
library(mapview)


## ------------------------------------------------------------------------
pophu<-readOGR('.',layer='y2010_51_pophu')



## ------------------------------------------------------------------------
plot(pophu)


## ------------------------------------------------------------------------
choropleth(pophu,pophu$POP10)

## ------------------------------------------------------------------------
choropleth(pophu,pophu$HOUSING10)


## ------------------------------------------------------------------------
pophu_sub <- pophu[pophu$POP10>100,]


## ------------------------------------------------------------------------
nlcd<-raster('nlcd.tif')
nlcd


## ------------------------------------------------------------------------
plot(nlcd)
mapview(nlcd)


## ------------------------------------------------------------------------
nlcd
is.factor(nlcd)
levels(nlcd)


## ------------------------------------------------------------------------
Montgomery <- countries[countries$NAME=='Montgomery',] 
nlcdsub <- crop(nlcd, extent(Montgomery))
plot(nlcdsub)
plot(Montgomery,add=TRUE)



## ------------------------------------------------------------------------
nlcdsub <- mask(nlcdsub, Montgomery)
plot(nlcdsub)


## ------------------------------------------------------------------------
length(nlcdsub[nlcdsub==11])


## ------------------------------------------------------------------------
forest <- nlcdsub
forest[forest>=41&forest<=43]=1
forest[forest!=1]=0
plot(forest)


## ------------------------------------------------------------------------
countries<-readOGR('.',layer='counties')


## ------------------------------------------------------------------------
sum(countries$POP2000)
mean(countries$POP2000)


## ------------------------------------------------------------------------
sum(countries$POP2003)
mean(countries$POP2003)


## ------------------------------------------------------------------------
M_P<-countries[countries$NAME=="Montgomery"|countries$NAME=="Pulaski",]
plot(M_P)+polygonsLabel(M_P,M_P$NAME)


## ------------------------------------------------------------------------
nlcd<-raster('nlcd.tif')
Montgomery <- countries[countries$NAME=='Montgomery',] 
nlcdsub <- crop(nlcd, extent(Montgomery))
nlcdsub <- mask(nlcdsub, Montgomery)
plot(nlcdsub)


## ------------------------------------------------------------------------
length(nlcdsub[nlcdsub==41])
length(nlcdsub[nlcdsub==41])*xres(nlcdsub)*yres(nlcdsub)/1e6


## ------------------------------------------------------------------------
length(nlcdsub[nlcdsub==42])
length(nlcdsub[nlcdsub==42])*xres(nlcdsub)*yres(nlcdsub)/1e6


## ------------------------------------------------------------------------
length(nlcdsub[nlcdsub==43])
length(nlcdsub[nlcdsub==43])*xres(nlcdsub)*yres(nlcdsub)/1e6


## ------------------------------------------------------------------------
urban=nlcdsub
urban[urban>=21&urban<=24]=1
urban[urban!=1]=0
plot(urban)

