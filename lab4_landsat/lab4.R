#19092019
# getValues(raster_variable) //to get the raster values as vector 
# alf<-data.frame(band1=getValues(landsat))

install.packages('raster')
install.packages('GISTools')
install.packages('rgdal')
install.packages('randomForest')
install.packages('mapview')

library(raster)
library(GISTools)
library(rgdal)
library(randomForest)
library(mapview)

#reading the image
landsat_full<-stack('./landsat.tif')

landsat<-landsat_full
plot(landsat)
crs(landsat)

#reading the training data
training_poly<-readOGR(dsn='.',layer='trainingdata')
plot(training_poly)
crs(training_poly)

#rasterizing training data
training_poly_ras<-rasterize(training_poly,landsat[[1]],field=training_poly$Id)
plot(training_poly_ras)
mapview(landsat[[1]])+mapview(training_poly_ras)

#converting raster into dataframe using the training data alone
training_matrix<-landsat[training_poly_ras>0]
training_df<-data.frame(training_matrix)
training_df$label<-as.factor(training_poly_ras[training_poly_ras>0])


#creating and training rf model

rfmodel <- randomForest(label ~. , data = training_df)

#entire dataset
all_df<-landsat[landsat[[1]]>=0]
all_df<-data.frame(all_df)

#predicting the data

rf_pred <- predict(rfmodel, all_df)

#converting it to image
output_label<-training_poly_ras
output_label<-setValues(output_label, rf_pred)
output_label[landsat[[1]]==0 & landsat[[2]]==0 & landsat[[3]]==0 ]<-NA
plot(output_label,col=c('red','yellow','green','blue'),legend = FALSE)
legend("topleft",legend = c("Urban", "Cropland","forest","water"),fill =c('red','yellow','green','blue'))

#writing the output image
writeRaster(output_label,filename = 'classified', format='GTiff',overwrite=TRUE)

#calculating area
cell_area<-xres(landsat)*yres(landsat)/1e6
#urban
length(output_label[output_label==1])*cell_area
#cropland
length(output_label[output_label==2])*cell_area
#forest
length(output_label[output_label==3])*cell_area
#water
length(output_label[output_label==4])*cell_area
