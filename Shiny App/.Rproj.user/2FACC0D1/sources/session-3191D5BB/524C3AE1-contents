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
print(file_list)
for (i in file_list){
  file_str <- strsplit(i,split='.csv', fixed=TRUE)[[1]][1]
  file_str_df <- paste('df_',file_str,sep='')
  assign(file_str_df, melt(read_csv(paste('ANOVA_city/',i,sep=''))))
}

# extract term ------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("darkly"),
  headerPanel(h1("Asian Restaurants",align = "center")),
  tags$head(tags$style(
    HTML(
      "
      @import url('//fonts.googleapis.com/css?family=Rancho');
      h1 {
      font-family: 'Rancho';
      font-weight: 500;
      line-height: 1.5;
      font-size: 60px;
      }
      "
    )
  )),
  sidebarLayout(
    position = "left",
    sidebarPanel(width = 4,
                 style = paste0("height: 85vh"), 
                 selectInput("City_selection", 
                             label = h3("Select city from the list: "), 
                             choices = c("",us_cities$name),
                             selected = NULL,
                             width = "100%"),
                 
                 fluidRow(align="center",
                   actionButton("city_confirm", "Generate Result!", class = "btn-success")
                 ),
                 fluidRow(
                   column(12, plotlyOutput("cityPlot", height = "300px"),style='padding:5px;')
                 ),
                 fluidRow(
                   column(12, plotlyOutput("cityPlotStars", height = "300px"),style='padding:5px;')
                 ),
                
                 
                 ),
    mainPanel(tabsetPanel(
      tabPanel("Goal",icon = icon("regular fa-file-pen"),
               h1(),
               fluidRow(column(width = 10, wellPanel(
               h3("In this project, we analyzed Yelp's dataset in an attempt to provide business owners with useful business insights. Our goal is to make recommendations on what types of new restaurants business owners can open in each city.",style = 'line-height: 1.5;')),wellPanel(
               h3("If you want to open a new Asian restaurant, you can make a city selection on the left sidebar, and then we will give you a recommendation! ",icon("regular fa-face-smile-wink"),style = 'color:#fcbf49;line-height: 1.5;'))))
      ),
      
      tabPanel(
        "Map",icon = icon("solid fa-location-dot"),
        wellPanel(style = "height:120px;width:400px",
          sliderInput(
            "range",
            "Review Count",
            min(restaurant_attr$review_count),
            max(restaurant_attr$review_count),
            value = range(restaurant_attr$review_count),
            step = 10,width='400px'
          ),
        ),
        leafletOutput("map", width = "800px",height = "600px")
        
      ),
      tabPanel("Recommendation",icon = icon("regular fa-thumbs-up"),
               h2("Our Recommendation",style = 'font-weight: bold;'),
               h2(textOutput('text_city'),style = 'color:#fcbf49; font-weight: bold;'),
               h1(),
               h4(textOutput('text_city_review'),style = 'line-height: 1.5;'),
               h1(),
               
               h4(textOutput('text_city_stars'),style = 'line-height: 1.5;'),
               #fluidRow(column(width = 4, wellPanel("Average review count: ")),column(width = 4, wellPanel("Average rating: "))),
               leafletOutput("map_Recommendation", width = "800px",height = "400px")
               ),
      tabPanel("Restaurant Overview",icon = icon("solid fa-shop"),
               plotlyOutput("cityOverview", height = "450px"),
               h6(''),
               fluidRow(column(width = 6, plotlyOutput("cityDensityReview", height = "300px")),
                               column(width = 6, plotlyOutput("cityDensityStars", height = "300px")))
      ),
      tabPanel("Contact Info",icon = icon("regular fa-envelope"),
               h1(),
               h4(paste("If you have any questions, feel free to contact us for details.")),
               HTML("Email: "),a("zhao485@wisc.edu",href = "zhao485@wisc.edu")
      ),
    ),)
  ),
)
