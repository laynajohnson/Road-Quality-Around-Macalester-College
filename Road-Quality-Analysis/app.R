
library(shiny)
library(shinythemes)
library(shinyWidgets)

library(tidyverse)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(viridis)



# Define UIs for application 
ui <- navbarPage(

  title = h1("An Analysis of Road Quality Around Macalester College", style = 'color: #f3ede6; font-size: 40px; margin-top: -10px'),
  id = "tabs",
  position = "static",
  fluid = TRUE,
  tags$head(
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                    height: 80px;
                    padding-top: 30px
                    }'))
  ),
  theme = shinytheme("flatly"),
  
  tabPanel("Data Protocol",
           mainPanel(
             h1("Data Collection and Management Protocol")
           )),
  tabPanel("Test",
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
