mycounty<-readOGR('.',layer='mycounty')
roads<-readOGR('.','road100k_l_va121')



plot(mycounty,main='Roads')
road_palette <- c("blue", "green", "grey", "purple")
plot(roads,col=road_palette,add=T)
legend('topleft',legend = levels(roads$RTTYP),fill = road_palette,bty = "n")


library(prettymapr)
addnortharrow(pos = "topright", padin = c(0.1, 0.1), scale = 0.5,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")

addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")