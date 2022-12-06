library(shiny)
library(plotly)
library(shinythemes)
library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(maps)
library(leaflet)
library(unikn)
library(ggplot2)
library(usmap)
library(tidyr)
library(reshape2) 
library(ggsignif)
library(gplots)

restaurant_attr <- read_csv("restau_all.csv")

us_cities <- read_csv("uscities_cleaned.csv")

file_list <- list.files('ANOVA_city')
for (i in file_list){
  file_str <- strsplit(i,split='.csv', fixed=TRUE)[[1]][1]
  file_str_df <- paste('df_',file_str,sep='')
  assign(file_str_df, melt(read_csv(paste('ANOVA_city/',i,sep=''))))
    
}


# extract term ------------------------------------------------------------
  

server <- function(input, output, session) {
  
  id <- reactive({
    input$map_marker_click$id
  })
  
  filteredData <- reactive({
    restaurant_attr[restaurant_attr$review_count >= input$range[1] & restaurant_attr$review_count <= input$range[2],]
  })
  
  colorpal <- reactive({
    colorNumeric(c('#af8dc3','#f7f7f7','#7fbf7b'),restaurant_attr$review_count)
  })
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~review_count*3, weight = 1, color = "#af8dc3",
                 fillColor = ~pal(review_count), fillOpacity = 0.7, popup = ~paste("<h4 style='color:#1F618D'>",restaurant_attr$name,"</h4>",
                                                                                   "Review Count:", restaurant_attr$review_count, "<br>",
                                                                                   "Rating:", restaurant_attr$stars, "<br>",
                                                                                   "Address:", restaurant_attr$address)
      )
  })
  
  observe({
    proxy <- leafletProxy("map", data = filteredData())
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
      pal <- colorpal()
      proxy %>% addLegend(position = "topright",
                          pal = pal, values = ~review_count
      )
    
  })
  
  map_proxy <- leafletProxy("map")
  observeEvent(input$city_confirm, {
    output$text_city <- renderText({ paste('For',input$City_selection) })
    
    output$text_city_review <- renderText({ us_cities[us_cities$name == input$City_selection,]$review_rec })
    output$text_city_stars <- renderText({ us_cities[us_cities$name == input$City_selection,]$stars_rec })
    output$distPlot <- renderPlot({
      plot(rnorm(10))
    })
      lat_city <- us_cities[us_cities$name == input$City_selection,]$lat
      long_city <- us_cities[us_cities$name == input$City_selection,]$long
      map_proxy %>%
        flyTo(lng = long_city, lat = lat_city, zoom = 9)
    
    output$map_Recommendation <- renderLeaflet({
      lat_city <- us_cities[us_cities$name == input$City_selection,]$lat
      long_city <- us_cities[us_cities$name == input$City_selection,]$long
      restaurant_map_city <- leaflet(height=800, width=800) %>%
        
        setView( long_city, lat_city,9) %>% 
        
        addTiles() %>% 
        
        addCircleMarkers(lng = restaurant_attr$longitude,
                         lat = restaurant_attr$latitude, 
                         popup = paste("<h4 style='color:#1F618D'>",restaurant_attr$name,"</h4>",
                                       "Review Count:", restaurant_attr$review_count, "<br>",
                                       "Rating:", restaurant_attr$stars, "<br>",
                                       "Address:", restaurant_attr$address),
                         radius = 10,
                         clusterOptions = markerClusterOptions())
      restaurant_map_city
    })
    output$cityPlot <- renderPlotly({
      file_str <- paste('df_',input$City_selection,'_review',sep='')
      file_df <- get(file_str)
      title_cityplot = paste(input$City_selection,'Restaurant Reviews')
      ggplotly(ggplot(file_df,aes(x=variable,y=value,fill=variable))+
        labs(y='Review Count',x='Restaurant Category')+
        ggtitle(title_cityplot)+
        scale_fill_discrete(labels=c('Chinese', 'Japanese', 'Southeast Asia'))+
        geom_boxplot(alpha = 1,
                     outlier.color = "black" 
        )+ 
          scale_x_discrete(labels = c("Chinese", "Japanese", "Southeast Asia"))+
        theme_bw()+
          scale_fill_brewer(palette="Set2")+
        theme(
          plot.title = element_text(color="#1F618D", size=16, face="bold",hjust = 'center'),
          axis.text.x = element_text(vjust = 0.5),
          legend.position='none'
        )
    )
    })
    output$cityPlotStars <- renderPlotly({
      file_str <- paste('df_',input$City_selection,'_stars',sep='')
      file_df <- get(file_str)
      title_cityplot = paste(input$City_selection,'Restaurant Ratings')
      ggplotly(ggplot(file_df,aes(x=variable,y=value,fill=variable))+
        labs(y='Rating',x='Restaurant Category')+
        ggtitle(title_cityplot)+
          scale_x_discrete(labels = c("Chinese", "Japanese", "Southeast Asia"))+
        geom_boxplot(alpha = 1,
                     outlier.color = "black")+
        theme_bw()+
          scale_fill_brewer(palette="Set2")+
        theme(
          plot.title = element_text(color="#1F618D", size=16, face="bold",hjust = 'center'),
          legend.position='none')
    )
    })
    output$cityDensityReview <- renderPlotly({
      file_df_city <- restaurant_attr[restaurant_attr$city == input$City_selection,]
      title_cityplot = paste(input$City_selection,'Restaurant Review Count')
      Category <- file_df_city$type
      ggplotly(ggplot(data=file_df_city, aes(x=review_count,group=Category, fill=Category))+
                 labs(y='Density',x='Review Count')+
                 ggtitle(title_cityplot)+
                 geom_density(alpha=0.8)+
                 theme_bw()+
                 scale_fill_brewer(palette="Set2")+
                 theme(
                   plot.title = element_text(color="#1F618D", size=14, face="bold",hjust = 'center'))
      )
    })
    output$cityDensityStars <- renderPlotly({
      file_df_city <- restaurant_attr[restaurant_attr$city == input$City_selection,]
      title_cityplot = paste(input$City_selection,'Restaurant Ratings')
      Category <- file_df_city$type
      ggplotly(ggplot(data=file_df_city, aes(x=stars,group=Category, fill=Category))+
                 labs(y='Density',x='Ratings')+
                 ggtitle(title_cityplot)+
                 geom_density(alpha=0.8)+
                 theme_bw()+
                 scale_fill_brewer(palette="Set2")+
                 theme(
                   plot.title = element_text(color="#1F618D", size=14, face="bold",hjust = 'center'))
      )
    })
    
    
    
    
  })
  
  
  output$map <- renderLeaflet({
    restaurant_map <- leaflet(height=800, width=800) %>%
      setView(-96, 37.8, 4) %>% 
      
      addTiles() %>% 
      
      addCircleMarkers(lng = restaurant_attr$longitude,
                       lat = restaurant_attr$latitude, 
                       popup = paste("<h4 style='color:#1F618D'>",restaurant_attr$name,"</h4>",
                                               "Review Count:", restaurant_attr$review_count, "<br>",
                                               "Rating:", restaurant_attr$stars, "<br>",
                                                "Address:", restaurant_attr$address),
                       radius = 10,
                       clusterOptions = markerClusterOptions())
    restaurant_map
  })
  
  output$cityOverview <- renderPlotly({
    Category <- restaurant_attr$type
    City <- fct_rev(fct_infreq(restaurant_attr$city))
    ggplotly(ggplot(restaurant_attr,aes(x = City,fill = Category)) + 
               ggtitle('Current Restaurant Count In Each City')+
               scale_fill_brewer(palette="Set2")+ylab('Restaurant Count')+
      geom_bar(stat = 'count')+coord_flip()+
      theme(
        plot.title = element_text(color="#1F618D", size=16, face="bold",hjust = 'center')
      )
      
      )
  })

}

