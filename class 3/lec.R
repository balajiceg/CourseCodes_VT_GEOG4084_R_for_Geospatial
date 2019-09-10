#03092019 
# histogram
# 
# intersction and select

# importing library
install.packages('raster')
install.packages('rgdal')
install.packages('mapview')
library(raster)
library(rgdal)
library(mapview)


# read shape file
pophu_p<-readOGR('.','pophu')


#  read attributes
mydata<-pophu_p@data
mydata<-cbind(mydata,reclass=0)

# reclassify
mydata$reclass[mydata$POP10>500]<-1
mydata$reclass[mydata$POP10>100 & mydata$POP10<=500]<-2
mydata$reclass[mydata$POP10<=100]<-3
# update the shape file
pophu_p@data<-mydata

# write the shape file
writeOGR(obj=pophu_p, dsn=".", layer='pophu_p_reclass', driver="ESRI Shapefile",overwrite_layer=T)

#rendring using symbols
mapview(pophu_p,zcol = c("reclass"))

#making maps
roads<-readOGR('.','road100k_l_va121')
#seing the categories
levels(roads$RTTYP)
road_palette <- c("blue", "green", "grey", "purple")
plot(roads,col=road_palette)
# legend
legend('bottomright',legend = levels(roads$RTTYP),fill=road_palette)
#northarrow and scale bar
install.packages('prettymapr')
library(prettymapr)
addnortharrow()
addscalebar()


#ploting with custom options
mycounty<-readOGR('.',layer='mycounty')

plot(mycounty,main='Roads')
road_palette <- c("blue", "green", "grey", "purple")
plot(roads,col=road_palette,add=T)
legend('topleft',legend = levels(roads$RTTYP),fill = road_palette,bty = "n")

addnortharrow(pos = "topright", padin = c(0.1, 0.1), scale = 0.5,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")

addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")

## ploting rasters
#you need to install FedData library first
install.packages('FedData')
# this data is used to grab nlcd data from server directly
library(FedData)
DEM <- get_ned(template=mycounty, label='test1')
DEM_p<-projectRaster(DEM,res=30,crs=crs(mycounty))
DEM_p<-mask(DEM_p,mycounty)
terrain.colors()
#margin specs
par(mar = c(1, 1, 1, 1))
plot(DEM_p, col=terrain.colors(100),  axes=FALSE,main='DEM')

addnortharrow(pos = "topright", padin = c(0.1, 0.1), scale = 0.5,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")

addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")