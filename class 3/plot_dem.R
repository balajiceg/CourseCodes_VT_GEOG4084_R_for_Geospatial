#you need to install FedData library first
library(FedData)
DEM <- get_ned(template=mycounty, label='test1')
DEM_p<-projectRaster(DEM,res=30,crs=crs(mycounty))
DEM_p<-mask(DEM_p,mycounty)

par(mar = c(1, 1, 1, 1))
plot(DEM_p, col=terrain.colors(100),  axes=FALSE,main='DEM')

library(prettymapr)
addnortharrow(pos = "topright", padin = c(0.1, 0.1), scale = 0.5,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")

addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")