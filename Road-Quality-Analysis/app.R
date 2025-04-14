
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(bslib)

library(tidyverse)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
# library(viridis)

# creating custom theme for shiny app
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "united", # bootswatch theme
  bg = "#edebeb",        # background
  fg = "#1e1e1e",        # text
  primary = "#803025",    
  base_font = font_google("Ubuntu"),
  heading_font = font_google("Merriweather"),
  code_font = font_google("Fira Code")
)

custom_theme <- bs_add_rules(custom_theme, "
  .navbar {
    background-color: #803025 !important;
  }
  .navbar .navbar-brand,
  .navbar-nav > li > a {
    color: #f3ede6 !important;
  }
")

# Define UIs for application 
ui <- fluidPage(
  theme = custom_theme,  
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
                      height: 20px;
                      padding-top: 0px;
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
                 h2("some text")
               ),
               mainPanel(
                 fluidRow()
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

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
