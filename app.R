# Load required Libraries
library(raster)
library(GISTools)
library(rgdal)
library(mapview)
library(leaflet)
library(shiny)
library(plotly) #interactive plots



# read required data from public folder ---------------
diff_ppt<-stack('./www/diff_ppt_list.tif')

diff_tmean<-stack('./www/diff_tmean_list.tif')

el_nino_tmean<-stack('./www/el_nino_tmean_list.tif')
la_nina_tmean<-stack('./www/la_nina_tmean_list.tif')

el_nino_ppt<-stack('./www/el_nino_ppt_list.tif')
la_nina_ppt<-stack('./www/la_nina_ppt_list.tif')
#------------------------------------------------------



# convert raster to df in order to optimize the ploting--------
ppt<-cbind(getValues(el_nino_ppt),getValues(la_nina_ppt))
tmean<-cbind(getValues(el_nino_tmean),getValues(la_nina_tmean))
#------------------------------------------------------


# Prepare mapview raster ------------------------------------------
full_stack<-stack(c(diff_tmean,diff_ppt))
months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
names(full_stack)<-c(paste(months,'temp',sep=' '),paste(months,'ppt',sep=' '))
#------------------------------------------------------

# UI for the webpage----------------------------------
ui<-fluidPage(
  verticalLayout( leafletOutput("map"),splitLayout(
    plotlyOutput('plot1'),plotlyOutput('plot2'))
  ))
#------------------------------------------------------


# server function--------------------------------------
server<- function(input,output){
  m<-mapview(full_stack) 
  output$map<-renderLeaflet ({
    m@map
  })
  
  # read the click coordinate and plot
  observeEvent(input$map_click, {
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    idx<-cellFromXY(el_nino_tmean,xy=c(clng,clat))
    
    output$plot1<-renderPlotly({})
    
    a<-data.frame(el_nino=ppt[idx,1:12],la_nina=ppt[idx,13:24])
    rownames(a)<-c(1:12)
    a$months<-factor( 1:12,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
    
    output$plot1<-renderPlotly({
      plot_ly(a, x = ~months, y = ~el_nino, type = 'bar', name = 'El Nino') %>%
        add_trace(y = ~la_nina, name = 'La Nina') %>%
        layout(yaxis = list(title = 'Precipitation (mm)'), barmode = 'group')
    })
    
    
    b<-data.frame(el_nino=tmean[idx,1:12],la_nina=tmean[idx,13:24])
    rownames(b)<-c(1:12)
    b$months<-factor( 1:12,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
    
    output$plot2<-renderPlotly({
      plot_ly(b, x = ~months, y = ~el_nino, type = 'bar', name = 'El Nino') %>%
        add_trace(y = ~la_nina, name = 'La Nina') %>%
        layout(yaxis = list(title = 'Temperature (C)'), barmode = 'group',showlegend = FALSE)
    })
  })
}
#------------------------------------------------------


# start the server
shinyApp(ui=ui,server=server)