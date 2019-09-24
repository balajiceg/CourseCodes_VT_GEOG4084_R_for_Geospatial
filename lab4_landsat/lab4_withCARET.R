#24092019

install.packages('raster')
install.packages('GISTools')
install.packages('rgdal')
install.packages('caret')
install.packages('e1071')
install.packages('mapview')

library(raster)
library(GISTools)
library(rgdal)
library(randomForest)
library(caret)
library(e1071)
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
train_val_df<-data.frame(training_matrix)
train_val_df$label<-as.factor(training_poly_ras[training_poly_ras>0])



#train validation spliting
sample <- createDataPartition(train_val_df$label, p=0.75, list=FALSE)
train_df<-train_val_df[sample,]
val_df<-train_val_df[-sample,]

#creating and training rf model
control <- trainControl(method='cv', number=10)
metric <- 'Accuracy'
fit.cart <- train(label ~. , data=train_df, method='rf',trControl=control, metric=metric)

#validation , confusion matrix
predicted <- predict(fit.cart, val_df)
confusionMatrix(predicted, val_df$label)

#summary of traied model
plot(fit.cart$finalModel)


#entire dataset
all_df<-landsat[landsat[[1]]>=0]
all_df<-data.frame(all_df)

#predicting the data
rf_pred <- predict(fit.cart, all_df)

#converting it to image
output_label<-training_poly_ras
output_label<-setValues(output_label, rf_pred)
output_label[landsat[[1]]==0 & landsat[[2]]==0 & landsat[[3]]==0 ]<-NA
plot(output_label,col=c('red','yellow','green','blue'),legend = FALSE)
legend("topleft",legend = c("Urban", "Cropland","forest","water"),fill =c('red','yellow','green','blue'))

#writing the output image
writeRaster(output_label,filename = 'classified_caret', format='GTiff',overwrite=TRUE)

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
