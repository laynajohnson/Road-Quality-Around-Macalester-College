
library(shiny)
library(shinythemes)
library(shinyWidgets)

library(tidyverse)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(RColorBrewer)
library(htmltools)


# Reading in data and creating function for map
feb19 <- read_csv("data/cleaned-data/combined-clean/feb19-clean.csv")
feb27 <- read_csv("data/cleaned-data/combined-clean/feb27-clean.csv")
mar06 <- read_csv("data/cleaned-data/combined-clean/mar06-clean.csv")
mar13 <- read_csv("data/cleaned-data/combined-clean/mar13-clean.csv")
mar20 <- read_csv("data/cleaned-data/combined-clean/mar20-clean.csv")
mar27 <- read_csv("data/cleaned-data/combined-clean/mar27-clean.csv")
apr03 <- read_csv("data/cleaned-data/combined-clean/apr03-clean.csv")

mapping <- function(combined_data, title) {
  # Making it spatial
  combined_data <- st_as_sf(combined_data, coords = c("location_longitude", "location_latitude"), crs = 4326)
  
  # legend details
  labels <- c("Comfortable (0 - 0.315)", "Fairly Uncomfortable (0.315 - 1.6)", "Uncomfortable (1.6 - 2)", "Extreme Discomfort (2+)")
  bins <- c(0, 0.315, 1.6, 2.0, 4)
  
  pal <- colorBin( # creating palette for map and legend
    palette = brewer.pal(4, "Reds"),
    domain = c(0, 2.5),
    bins = bins,
    na.color = "#ccc"
  )
  
  combined_data$comfort_level <- cut( # creating new variable called comfort level to make the legend breaks
    combined_data$L2_norm,
    breaks = c(-Inf, 0.315, 1.6, 2.0, Inf),
    labels = c("Comfortable", "Fairly Uncomfortable", "Uncomfortable", "Extreme Discomfort"),
    right = TRUE
  )
  
  # Leaflet map with magnitude of acceleration
  map <- leaflet(combined_data, options = leafletOptions(zoomControl = TRUE)) %>%
    addProviderTiles("CartoDB.Positron") %>% #ESRI basemap
    addCircles( # adding points from recording
      radius = 1,
      color = ~pal(L2_norm), # using palette made before
      weight = 5,
      opacity = 0.9,
      popup = ~paste0("<b>", comfort_level, "</b>",
                      "<br>", street_name,
                      "<br>Magnitude of Acceleration: ", round(L2_norm, 2))
    ) %>%
    
    addControl(
      html = paste0(
        "<div style='
        all: unset;
        font-size:25px;
        font-family: 'Lato', sans-serif;
        color: black;
        display: block;
      '>",
        "Road Quality Around Macalester College on ", title, ", 2025"),  
      position = "topleft"
    )%>%
    
    addLegend(
      colors = pal(bins[-length(bins)]),  # skip last bin
      labels = labels,
      title = "Comfort Level (m/s<sup>2</sup>)",
      opacity = 1,
      position = "topleft"
    ) %>%
    addControl(
      html = paste0(
        "<div style='
      all: unset;
      font-size:10px;
      line-height:1.2;
      color: black;
      display: block;
      text-shadow:
        -1px -1px 0 #e8e8e8,
         1px -1px 0 #e8e8e8,
        -1px  1px 0 #e8e8e8,
         1px  1px 0 #e8e8e8;
    '>",
        "Cartographer: Alayna Johnson, Macalester 25'<br> Acceleration and location data collected using <br> Sensor Logger smartphone application on iPhone 14",
        "</div>"
      ),
      position = "bottomleft",
      className = "fieldset {
    border: 0;
}") %>%
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        map.zoomControl.setPosition('bottomright');
      }
    ")
  
  return(map)
}


# Define UIs for application 
ui <- fluidPage(
  theme = shinytheme("simplex"),
  
  # creating the title
  div(
    style = "background-color: #803025; padding: 20px;",
    h1("An Analysis of Road Quality Around Macalester College",
       style = "color: #f3ede6; font-size: 40px; margin: 0;")
  ),
  
  navbarPage(
    title = NULL,
    id = "tabs",
    position = "static",
    fluid = TRUE,
    
    tags$head(
      tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                      height: 15px;
                      padding-top: 10px;
                      font-size: 20px;
                      }'))
    ),
    
    tabPanel("The Project",
             mainPanel(
               h1("Introduction"),
               h5("test")
             )),
    
    tabPanel("The Data",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 h2("some text")
               ),
               mainPanel(
                 fluidRow()
               )
             )),
    
    tabPanel("Analysis and Exploration",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 selectInput(
                   inputId = "date_choice",
                   label = "Choose a date:",
                   choices = c(
                     "February 19th" = "feb19",
                     "February 27th" = "feb27",
                     "March 6th" = "mar06",
                     "March 13th" = "mar13",
                     "March 20th" = "mar20",
                     "March 27th" = "mar27",
                     "April 3rd" = "apr03"
                   ),
                   selected = "feb19"
                 )
               ),
               mainPanel(
                 fluidRow(
                   column(width = 11, leafletOutput("mag_map"), height = 300)
                 )
               )
             )),
    
    tabPanel("Implications",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 h2("some text")
               ),
               mainPanel(
                 fluidRow()
               )
             ))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  dataset_list <- list(
    feb19 = feb19,
    feb27 = feb27,
    mar06 = mar06,
    mar13 = mar13,
    mar20 = mar20,
    mar27 = mar27,
    apr03 = apr03
  )
  
  date_titles <- c(
    feb19 = "February 19th",
    feb27 = "February 27th",
    mar06 = "March 6th",
    mar13 = "March 13th",
    mar20 = "March 20th",
    mar27 = "March 27th",
    apr03 = "April 3rd"
  )
  
  output$mag_map <- renderLeaflet({
    req(input$date_choice)
    selected_data <- dataset_list[[input$date_choice]]
    selected_title <- date_titles[[input$date_choice]]
    mapping(selected_data, title = selected_title)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
