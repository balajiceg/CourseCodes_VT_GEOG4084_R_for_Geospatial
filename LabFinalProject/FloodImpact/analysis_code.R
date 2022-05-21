# installing required packages
install.packages('raster')
install.packages('rgdal')
install.packages('mapview')
install.packages('GISTools')
install.packages('rgeos')
install.packages('gdalUtils')
install.packages('geosphere')
install.packages('plotly')
install.packages('FedData')

#loading required packages
library(raster)
library(rgdal)
library(GISTools)
library(rgeos)
library(geosphere)
library(gdalUtils)
library(plotly)
library(mapview)
library(FedData)


# untar the ndvi
untar('./GTiff.tar.gz',exdir = "ndvi_16day")

#read ndvi
ndvi_tiffs<-list.files(path="./ndvi_16day/",pattern="_NDVI.tif",full.names=T)
ndvi_stack<-stack(ndvi_tiffs)

#read flood rgb  and bring all to same extent
flood<-stack('./2017USA4510Detail2.tif')
ndvi_stack<-resample(crop(ndvi_stack,flood),flood)
flood<-mask(flood,ndvi_stack$MOD13Q1.A2014001.h09v06.006.2015272061127_250m_16_days_NDVI)

# nlcd download and crop
nlcd<-get_nlcd(flood$X2017USA4510Detail2.1,'HOUSTON')
# nlcd<-raster('./HOUSTON_nlcd/NLCD/HOUSTON_NLCD_2011_landcover.tif')
nlcd<-projectRaster(nlcd,flood,res=res(flood),crs=crs(flood))
nlcd<-mask(nlcd,flood$X2017USA4510Detail2.1)

# seperate flooded and nonflooded
flooded<-flood$X2017USA4510Detail2.1
flooded[!is.na(flooded)]<-NA
normal_water<-flooded
flooded[flood[[1]]>200 & flood[[3]]<100]<-1
normal_water[flood[[3]]>210 & flood[[1]]<100]<-1
non_flooded<-flood$X2017USA4510Detail2.1
non_flooded[normal_water==1 | flooded==1]<-NA

# extract vegetation cover using nlcd
veg_mask<-nlcd
veg_mask[nlcd>40 & nlcd<90]<-1
veg_mask[veg_mask!=1]<- NA

# ndvi mean over flooded areas
ndvi_flooded<-mask(ndvi_stack,mask(flooded,veg_mask))
ndvi_flooded_df<-t(getValues(ndvi_flooded))
mean_ndvi<-rowMeans(ndvi_flooded_df,na.rm = T)

#extract dates from the ndvi df
r_names<-rownames(ndvi_flooded_df)
r_dates<-as.Date(paste(substr(r_names,10,13),"01","01",sep="-"))+as.numeric(substr(r_names,14,16))-1


# ndvi mean over non flooded
ndvi_non_flooded<-mask(ndvi_stack,mask(non_flooded,veg_mask))
ndvi_non_flooded_df<-t(getValues(ndvi_non_flooded))
mean_ndvi_non_flooded<-rowMeans(ndvi_non_flooded_df,na.rm = T)

#plot the times series plot
plot_ly(y=mean_ndvi_non_flooded,name="non flooded regions",x=r_dates,mode='lines',type="scatter") %>%
  add_trace(y = mean_ndvi, name = 'flooded regions', mode = 'lines') %>%
  layout(title="Scaled Mean NDVI" ,xaxis = list(title = "Date", range = c(r_dates[60],r_dates[length(r_dates)])))

#save the df as in RDS format
my_data<-data.frame(date=r_dates,flooded=mean_ndvi,non_flooded=mean_ndvi_non_flooded)
saveRDS(my_data,'ndvi.RDS')


mean_ndvi_non_flooded=my_data$non_flooded
mean_ndvi=my_data$flooded
r_dates=my_data$date


######### DNB analysis

# read the nlcd again
nlcd<-raster('./HOUSTON_nlcd/NLCD/HOUSTON_NLCD_2011_landcover.tif')
# referecne rasters for the hdfs- to get the transformation and projection 
v05ref<-raster('./v05ref.tif')
v06ref<-raster('./v06ref.tif')

# read night light data for two different frames
v05_tiffs<-list.files(path="./dnb/",pattern="h08v05",full.names=T)
v06_tiffs<-list.files(path="./dnb/",pattern="h08v06",full.names=T)
v05_stack<-stack(v05_tiffs)
v06_stack<-stack(v06_tiffs)

# set extent for the stacks from the reference file
extent(v05_stack)<-extent(v05ref)
crs(v05_stack)<-crs(v05ref)
extent(v06_stack)<-extent(v06ref)
crs(v06_stack)<-crs(v06ref)

# mosaic the stacks and write the mosaic geotiff 
dnb<-mosaic(crop(v05_stack,flood[[1]]),crop(v06_stack,flood[[1]]),fun=mean)
names(dnb)<-names(v05_stack)
writeRaster(dnb,filename = 'day_light_stack', format='GTiff',overwrite=TRUE)
dnb<-stack('./day_light_stack.tif')

# reproject the nlcd raster
nlcd<-projectRaster(nlcd,dnb,res=res(dnb),crs=crs(dnb),method="ngb")

# extract and plot the urban regions
urban<-nlcd
urban[urban>20 & urban< 40]<-1
urban[urban!=1]<-NA
plot(urban)

# mask the light intensity over urban region
dnb_urban<-mask(dnb,urban)
dnb_urban_df<-t(getValues(dnb_urban))
rownames(dnb_urban_df)<-names(v06_stack)

# extract the date from the dnb df
r1_names<-rownames(dnb_urban_df)
r1_dates<-as.Date(paste(substr(r1_names,10,13),"01","01",sep="-"))+as.numeric(substr(r1_names,14,16))-1

# compute mean over the urban regions for each time frame
mean_urban<-rowMeans(dnb_urban_df,na.rm = T)

# generate time series plot 
plot_ly(y=mean_urban,name="Night Light Intensity",x=r1_dates,mode='lines+markers',type="scatter") %>%
layout(title="Night Light Intensity")


#plot and save the night light data for the 35 days
s<-sd(getValues(dnb_urban),na.rm =T)
m<-mean(getValues(dnb_urban),na.rm=T)
s_dnb_urban<-(dnb_urban-m)/s

s_dnb_urban[s_dnb_urban>4]<-NA

dir.create('./plots')
for(i in 1:36){
  png(file=paste("./plots/",r1_dates[i],".png",sep=""),width=1200, height=970)
  plot(s_dnb_urban[[i]],main=r1_dates[i], breaks = seq(from = -2, to =4, by = 0.5),col=terrain.colors(20)[3:20])
  dev.off()
}

