# Date:29082019

# raster matrices [1,1] starts from the top left of the image
# T can be used insted of TRUE , F can be used for Falsepop
# and operator & , or operator |

# What is the difference between @ and $? @ is used for objects while dollor can only be used for dataframes and named list

# Read csv and summarize
df<-read.csv('pop2010.csv')
summary(df)

# sum of population
sum(df$POP10)

#ploting
plot(df$HOUSING10,df$POP10)

# importing library
install.packages('raster')
install.packages('rgdal')
library(raster)
library(rgdal)

a<-raster('nlcd.tif')
a

#reading shapefile

#(folder,filename)
pophu<-readOGR('.','y2010')
pophu
plot(pophu)

#accessing the attributes of the shape file
pophu@data

#total popution of area
sum(pophu$POP10)
sum(pophu@data$POP10)

#propjection 
crs(pophu)
plot(pophu)


nlcd<-raster('nlcd.tif')
#polting one layer on another, but doesnt work due to different projections
plot(nlcd,add=TRUE)

#reprojection  of vector
pophu_utm=spTransform(pophu,crs(nlcd))


#projection of raster
tm1<-raster("tm1.tif")
crs(tm1)
tm1_utm=projectRaster(tm1,res=30,crs=crs(nlcd))


#plotting again
crs(pophu_utm)
plot(nlcd)
plot(pophu_utm,add=TRUE)



#Add a field for area calculation
mydata<-pophu_utm@data
mydata<-cbind(mydata,newarea=0)

#importing rgeos
install.packages('rgeos')
library(rgeos)
#if by id not give it will calculate the total area
mydata$newarea<-gArea(pophu_utm,byid=T)
View(mydata$newarea)


#updating back the shape file 
pophu_utm@data<-mydata

writeOGR(obj=pophu_utm, dsn="./outputs", layer='pophu_utm', driver="ESRI Shapefile",overwrite_layer=T)

#summary statistics and selecting by attributes
mean(pophu_utm$POP10)
min(pophu_utm$POP10)
max(pophu_utm$POP10)

plot(pophu_utm$POP10)

a<-pophu_utm[pophu_utm$POP10>500,]
plot(a)
length(a)

#two criteria
a<-pophu_utm[pophu_utm$POP10>500 & pophu_utm$HOUSING10>5,]
length(a)


#selecting by locations
country<-readOGR('./LabAssignment','counties')
crs(country)
country_p<-spTransform(country,crs(pophu_utm))
s_intersect<-intersect(pophu_utm, country_p)

#reclass
mydata<-pophu_utm@data
  #creating new column
mydata<-cbind(mydata,reclass=0)

mydata$reclass[mydata$POP10>500]<-1
mydata$reclass[mydata$POP10>100 & mydata$POP10<500]<-2
mydata$reclass[mydata$POP10<=500]<-3
  #update the shape file back 
pophu_utm@data<-mydata

View(pophu_utm@data)
