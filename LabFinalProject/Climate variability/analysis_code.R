#install the required packages
install.packages('raster')
install.packages('GISTools')
install.packages('rgdal')
install.packages('mapview')
install.packages('rgeos')
install.packages("analytics")

# Load required libraries
library(raster)
library(GISTools)
library(rgdal)
library(mapview)
library(rgeos)
library(analytics)

#read virgina shape file
virginia<-readOGR("./virginia_shp",layer="tl_2016_51_cousub")

#function to clip stack
clip_virginia<-function(x) mask(crop(x,virginia),virginia)

#function to check numbers
numbers_only <- function(x) !grepl("\\D", x)


#read elnino and lanina files
el_nino_months <- read.csv("./el_nino.csv")
la_nina_months <- read.csv("./la_nina.csv")


# extract prism data
types_list<-list.files('./prism_data/',full.names = T)
for(type in types_list){
  for(year in list.files(type,full.names = T)){
    for(days in list.files(year, full.names = T)){
      unzip(days, exdir=type)
    }
  }
}


#read the filenames for precipitation
ppt_files<- data.frame(list.files('./prism_data/ppt/',pattern = '.bil$', full.names = T))
colnames(ppt_files)<-c("filename")
ppt_files$filename <- as.character (ppt_files$filename)
#extract the date from filename
ppt_files<-data.frame(ppt_files[numbers_only(substr(ppt_files$filename,41,46)),]);colnames(ppt_files)<-c("filename")
#seperate the el nino and la nino files seperately using the elnino lanina months
el_nino_ppt_files<-subset(ppt_files,substr(filename,41,46) %in% el_nino_months$year_month )
la_nina_ppt_files<-subset(ppt_files,substr(filename,41,46) %in% la_nina_months$year_month )


#read the filenames for temperature
tmean_files<- data.frame(list.files('./prism_data/tmean/',pattern = '.bil$', full.names = T))
colnames(tmean_files)<-c("filename")
tmean_files$filename <- as.character (tmean_files$filename)
#extract the date from filename
tmean_files<-data.frame(tmean_files[numbers_only(substr(tmean_files$filename,45,50)),]);colnames(tmean_files)<-c("filename")
#seperate the el nino and la nino files seperately using the elnino lanina months
el_nino_tmean_files<-subset(tmean_files,substr(filename,45,50) %in% el_nino_months$year_month )
la_nina_tmean_files<-subset(tmean_files,substr(filename,45,50) %in% la_nina_months$year_month )

#create the stack files, clip it to virginia
el_nino_tmean_stack<-clip_virginia(stack(as.character(el_nino_tmean_files$filename)))
el_nino_ppt_stack<-clip_virginia(stack(as.character(el_nino_ppt_files$filename)))

la_nina_tmean_stack<-clip_virginia(stack(as.character(la_nina_tmean_files$filename)))
la_nina_ppt_stack<-clip_virginia(stack(as.character(la_nina_ppt_files$filename)))




#convert raster into df for temerature
el_nino_tmean<- t(getValues(el_nino_tmean_stack))
la_nina_tmean<- t(getValues(la_nina_tmean_stack))

el_nino_tmean<-cbind(el_nino_tmean,data.frame(months=rownames(el_nino_tmean)))
la_nina_tmean<-cbind(la_nina_tmean,data.frame(months=rownames(la_nina_tmean)))

# extract the months data from the file name
el_nino_tmean$months<-as.numeric(substr(el_nino_tmean$months,30,31))
la_nina_tmean$months<-as.numeric(substr(la_nina_tmean$months,30,31))

# aggregate the means as per months for el nino and la nina
el_nino_tmean_months<-aggregate(el_nino_tmean, list(el_nino_tmean$months), mean,na.action = na.omit)
la_nina_tmean_months<-aggregate(la_nina_tmean, list(la_nina_tmean$months), mean,na.action = na.omit)
# find the difference between the elnino and la nina
tmean_diff<- el_nino_tmean_months - la_nina_tmean_months
tmean_diff<-t(tmean_diff)
tmean_diff<-tmean_diff[3:nrow(tmean_diff)-1,]

# save the plots
for(i in 1:12){
  png(file=paste("./plots/tmean/",i,".png",sep=""),width=800, height=580)
  a<-el_nino_tmean_stack$PRISM_tmean_stable_4kmM3_195106_bil
  a<-setValues(a,tmean_diff[,i])
  plot(a,main=i)
  dev.off()
}


# repeat the above code for precipitation difference computation between elnino and lanina3
el_nino_ppt<- t(getValues(el_nino_ppt_stack))
la_nina_ppt<- t(getValues(la_nina_ppt_stack))

el_nino_ppt<-cbind(el_nino_ppt,data.frame(months=rownames(el_nino_ppt)))
la_nina_ppt<-cbind(la_nina_ppt,data.frame(months=rownames(la_nina_ppt)))

el_nino_ppt$months<-as.numeric(substr(el_nino_ppt$months,28,29))
la_nina_ppt$months<-as.numeric(substr(la_nina_ppt$months,28,29))

el_nino_ppt_months<-aggregate(el_nino_ppt, list(el_nino_ppt$months), mean,na.action = na.omit)
la_nina_ppt_months<-aggregate(la_nina_ppt, list(la_nina_ppt$months), mean,na.action = na.omit)
ppt_diff<- el_nino_ppt_months - la_nina_ppt_months
ppt_diff<-t(ppt_diff)
ppt_diff<-ppt_diff[3:nrow(ppt_diff)-1,]


for(i in 1:12){
  png(file=paste("./plots/ppt/",i,".png",sep=""),width=800, height=580)
  a<-el_nino_ppt_stack$PRISM_ppt_stable_4kmM2_195106_bil
  a<-setValues(a,ppt_diff[,i])
  plot(a,main=i)
  dev.off()
}


