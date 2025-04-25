
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


# Reading data and transforming for line map of roads (the data section in app)

roads <- read_sf("data/raw-data/macalester_road_lines/macalester_road_lines.shp")
roads <- st_transform(roads, crs = 4326)


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
        "Cartographer: Alayna Johnson, Macalester '25<br> Acceleration and location data collected using <br> Sensor Logger smartphone application on iPhone 14",
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

# Temperature graph details

environment_data <- read_csv("data/cleaned-data/environment-data.csv")

environment_data$date <- as.Date(environment_data$date, format = "%m-%d-%Y")

env_long <- environment_data %>%
  pivot_longer(cols = c(high_tempW, high_tempD, low_tempW, low_tempD),
               names_to = "temp_type",
               values_to = "temp") %>%
  mutate(
    category = case_when(
      temp_type == "high_tempW" ~ "Weekly High",
      temp_type == "high_tempD" ~ "Daily High",
      temp_type == "low_tempW"  ~ "Weekly Low",
      temp_type == "low_tempD"  ~ "Daily Low"
    )
  )

color_map <- c(
  "Weekly High" = "#B22222",  
  "Daily High"  = "#f4aa92",  
  "Weekly Low"  = "#456193",  
  "Daily Low"   = "#b3c4e3")  


# Creating the Rolling Magnitude graph with a function

process_and_plot <- function(df, plot_title) {
  # Create base_street
  df$base_street <- case_when(
    str_detect(df$street_name, "Snelling Avenue") ~ "Snelling Avenue",
    str_detect(df$street_name, "Grand Avenue") ~ "Grand Avenue",
    str_detect(df$street_name, "Fairview Avenue") ~ "Fairview Avenue",
    str_detect(df$street_name, "St. Clair Avenue") ~ "St. Clair Avenue",
    str_detect(df$street_name, "Summit Avenue") ~ "Summit Avenue",
    str_detect(df$street_name, "Macalester Street") ~ "Macalester Street",
    str_detect(df$street_name, "Vernon and Cambridge") ~ "Vernon and Cambridge",
    str_detect(df$street_name, "Lincoln Avenue") ~ "Lincoln Avenue",
    TRUE ~ "Other"
  )
  
  # Count how many lines per base_street (the street without direction or lane)
  street_counts <- df %>%
    distinct(base_street, street_name) %>%
    count(base_street, name = "line_count")
  
  # Add color to the data
  df_colored <- df %>%
    left_join(street_counts, by = "base_street") %>%
    group_by(base_street) %>%
    mutate(color = if (first(line_count) == 1) {
      "firebrick"
    } else {
      shades <- colorRampPalette(c("#fcae91", "#990000"))(n_distinct(street_name))
      shades[as.numeric(factor(street_name))]
    }) %>%
    ungroup()
  
  # Create plot of acceleration magnitude
  p <- ggplot(df_colored, aes(x = seconds_elapsed, y = L2_norm, group = street_name)) +
    geom_line(aes(color = color, text = paste0(street_name, "<br> Mag of Acceleration: ", round(L2_norm, 2))),
              alpha = 0.8, show.legend = FALSE) +
    scale_color_identity() +
    facet_wrap(~ base_street, scales = "free_x", ncol = 2) +
    theme_minimal() +
    theme(legend.position = "none") + 
    labs(
      title = paste("Magnitude of Acceleration on", plot_title),
      x = "Time",
      y = "Acceleration Magnitude"
    )
  
  ggplotly(p, tooltip = "text") %>%
    layout(hovermode = "x unified",
           hoverlabel = list(
             font = list(
               size = 12)
               )
           )
}


# Define UIs for application 
ui <- fluidPage(
  theme = shinytheme("simplex"),
  
  # creating the title
  div(
    style = "background-color: #803025; padding: 10px;",
    h1("An Analysis of Road Quality Around Macalester College",
       style = "color: #f3ede6; font-size: 30px; margin: 0;")
  ),
  
  navbarPage(
    title = NULL,
    id = "tabs",
    position = "static",
    fluid = TRUE,
    
    tags$head(
      tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                      padding-top: 10px;
                      font-size: 15px;
                      }'))
    ),
    
    tabPanel("The Project",
             mainPanel(
               h1("Introduction"),
               p("test")
             )),
    
    tabPanel("The Data",
               mainPanel(width = 12,
                     tabsetPanel(
                       tabPanel("Data Methods",
                      h1("--"),
                      h4("How do road quality conditions vary around Macalester College as winter transitions into spring?"),
                      h2("Data Collection"),
                      p("In order to answer this question, we collect data with accelerometer and GPS sensors mounted in a vehicle using the Sensor Logger app on an iOS device (iPhone 14). The app is available for both iOS and Android devices. These sensors are built into the device itself and the app helps to record this data for export and analysis. The phone is attached to a phone-holder system that attaches to the vent of the car and holds the phone in an upright and level position. When using the accelerometer and GPS sensor options, we collect three variables of interest for this study: acceleration in +/- X, Y, and Z directions, time, and location. Additional details about these collected data is included in the Data Cleaning section. Along with these data recorded through the Sensor Logger app, general condition variables are also collected. When measuring and considering road quality, it is important to think about environmental influences that can alter recordings such as weather events and general road conditions (e.g. rain, ice, or unplowed snow). Refer to the end of Collection Details for more information on supplementary data."),
                      h3("Data Details"),
                      p("The data is collected in a mostly weekly schedule as the focus of this study is to analyze how road quality varies as the seasons change. Starting Wednesday, February 19th data is collected every week on Thursdays (day was changed after the first collection) until April 3rd around Macalester College in Saint Paul, Minnesota. This means there are seven total recording sessions. The recordings include acceleration, location, and time data from the main streets around Macalester. These streets include: Grand Ave, Fairview Ave, Snelling Ave, Saint Clair Ave, Summit Ave, Lincoln Ave, Macalester St, Cambridge St, and S Vernon St. These streets are essential to travel around Macalester College and form the study area within the border roads of Fairview Ave, Summit Ave, Snelling Ave, and Saint Clair Ave."),
                      p("The Sensor Logger app starts a recording when you tap the “Start Recording” button in the Logger tab after toggling on your desired sensors. For the purpose of this study, the accelerometer and Location internal sensors were toggled on and everything else left off. To facilitate quality readings across different important streets we start a recording at the “start” of the street within the study area shown in Figure 1. Using this method ensures each side and/or lane of a given street is measured with no trailing collected data points to be removed while processing the data. After minimal testing of the recording methodology, we discovered the locational data is sensitive enough to detect which side of the street you are on. The recording of both sides and all lanes of each street help to make a more robust analysis of the road quality in our study area."),
                      fluidRow(
                        column(width = 6, offset = 3,
                               leafletOutput("road_lines"),
                               style = "margin-bottom: 20px;"
                        )
                      ),
                      fluidRow(
                        column(width = 6, offset = 3,
                               p(em("Figure 1: This leaflet map shows the route that is traveled during data collection and which roads are recorded.")))
                      ),
                      p("On top of the data collected through the Sensor Logger application, we also collect additional information about weather and weather related road conditions. On each week of collection, weather conditions such as temperature and precipitation are recorded in a spreadsheet. Temperature is recorded for daily highs and lows, and weekly highs and lows. These measurements are recorded in favor of averages as potholes can form due to the freeze-thaw cycle as temperatures change between below freezing, above, and back. A week is defined as the past Thursday to the Thursday of recording. If there are any particular road conditions on the day of the reading, this is also mentioned."),
                      h2("Data Management"),
                      p("All data for this study can be found in the github repository linked ",
                        a("here", href = "https://laynajohnson.shinyapps.io/road-quality-analysis/", target = "_blank"),
                        ". Files include quarto documents, knit html files, folders for raw data and cleaned data csv files, and dependencies for the deployed shiny web app. The current file structure contains three main folders: Road_Quality_Analysis, recording_analysis, and test_runs. The first folder holds all of the dependencies for the website created with Shiny in RStudio including all data used in the app and any images. The folder called recording_analysis holds quarto documents, knit html files, and folders for all raw and cleaned data from study recordings. The data folder within this section includes all raw, cleaned data and related metadata. Folder three, test_runs, has similar contents to the previous folder, but for original test data. All data cleaning, analysis, and visualization is done in RStudio using R and CSS and HTML are both used in creating the Shiny application."),
                      p("The csv files containing the raw recording data are exported from the Sensor Logger app directly to the desired folder in the repository. Detailed information about each variable and any transformations is provided in metadata files in the same folder as the data itself. These metadata break down what each variable in the analysis means in plain language and in what portion of the process it was used."),
                      h2("Data Cleaning"),
                      p("In order to ensure accurate and consistent data, we must adjust values for interpretability. Additionally, any unnecessary variables are removed from the final cleaned data. It is important to maintain concise data files which only have important data that is used in analysis. To view the complete raw data, refer to the described folder in the repository."),
                      p("A handheld iOS device, or iPhone, collects acceleration data in a specific way. By default, the device stores up and down acceleration (Y) as negative values for upward acceleration and positive values for downward acceleration, forward and backward acceleration (Z) as negative values for backwards acceleration and positive values for forward acceleration, left and right acceleration (X) as negative values for right acceleration and positive values for left acceleration. To make these values more intuitive, we alter these values to +Y as upward acceleration and +X as right acceleration while keeping +Z as forward acceleration. Reference Figure 2 to view what this transformation looks like."),
                      fluidRow(
                        column(width = 6, offset = 3, style = "margin-bottom: 20px;",
                               tags$figure(
                        tags$img(
                          src = "accelerometer-axes.png",
                          width = 600,
                          alt = "Diagram showing accelerometer axes on iPhone"
                        ),
                        tags$figcaption(em("Figure 2: The image to the left shows how an iOS device measures acceleration in all directions by default. The image on the right shows adjusted acceleration measurements. Image adapted from awesome-sensor-logger github repository."))
                      ))),
                      p("As the phone measures acceleration values in these directions with the device perpendicular to the ground, it must also sit this direction in the vehicle. The device is properly set up in this fashion in order to ensure consistency in data collection. The phone itself rests in a vent mounted holder that can be adjusted to hold the device perpendicular to the ground. This way, the measures shown in the right image of Figure 2 show the proper directions of acceleration after any transformations."),
                      p("In our analysis, we use several different types of visualizations. Line charts showing the magnitude of acceleration of the vehicle on each road and direction of travel use independent data frames each cleaned similarly as described above. These data frames contain the acceleration, location, and time fields for one recording. For interactive maps, the separate data tables are stacked on top of each other by combining the rows to create a longer table. This way, all point data for the entire route is in one place and can easily be visualized together."),
                      p("Any potential errant points occur at the start of data collection (time_elapsed = 0), so these points are removed from the data. There is some variation between points between different recording times on the same roads as speed can be different each drive. This kind of variation is not adjusted for, but should be mentioned.")
                   ),
               tabPanel("Behind the Analysis",
                      mainPanel(width = 12,
                        h1("--"),
                        h2("Data Visualization"),
                        p("These visualizations will show trends and changes over time graphically. There are two main areas of focus for these visualizations: to represent road conditions over time and conditions over space and time. The variables to be used for these visualizations include acceleration, time, and location. You can view these visualizations in the next two tabs."),
                        withMathJax(p("Throught these visualizations, we seek to answer our research question in an exploratory fashion: How do road quality conditions vary around Macalester College as winter transitions into spring? As a proxy to road conditions, we can calculate the smoothed acceleration magnitude of our data. This involves taking the magnitude of our \\(X\\), \\(Y\\), and \\(Z\\) acceleration values in \\(m/s^2\\) as, $$\\|{v}\\|_2 = \\sqrt{X^2 + Y^2 + Z^2}$$ (also called the Euclidean norm) and compute the rolling average of the magnitude values over time. We can learn about the magnitude of acceleration at each point independent of the direction of acceleration. This will help to understand events with significant acceleration in one or more directions and quiets some of the noise from measuring errors.")),
                        h3("Spatial Visualization"),
                        p("For spatial visualization, we created maps which show the magnitude of acceleration values as described before broken into categories using the international ISO 2631-1 Standard defining levels of comfort:"),
                        withMathJax(p("\\(\\|v\\|< 0.315 m/s^2\\) as being comfortable,  \\(0.315m/s^2 < \\|v\\| < 0.8 m/s^2\\) as fairly comfortable, \\(0.8m/s^2 < \\|v\\| < 1.6 m/s^2\\) as uncomfortable and \\(\\|v\\| > 2 m/s^2\\) being extremely uncomfortable.")),
                        p("The maps visualized in the next section represent the different time periods of collection in the study. There are seven different maps of all roads* which visualize discrete measures of the magnitude of acceleration at each second interval of recording."),
                        p("*Recording for March 20th is missing the recording for Snelling Avenue North in the right lane. This is due to failure to record this segment."),
                        h3("Time Series Visualization"),
                        p("To go along with the spatial visualizations, we created several line charts to further show this idea of magnitude of acceleration. The line charts take the x-axis to be the time elapsed as driving, or, the number of seconds since the start of the recording. There are 8 main streets the study takes place in, 
                          and a number of different directions and lanes for different roads. These plots look like one line chart for each base street, then if there are multiple directions and/or lanes these are included all in the same plot in different colors. 
                          Roads that have distinct lanes with dashed or solid line separations were driven on in both directions. Streets which had no dividing lines were driven on in one direction mostly down the center due to traffic (and lack thereof) and parked cars.
                          For example, Snelling Avenue has North and South traveling directions as well as left and right lanes for both, while Lincoln Avenue had one direction for recording. The plot with the label of Snelling Ave will have four lines in different 
                          colors while the Lincoln Avenue plot will have one line representing the need for only one direction to be driven."),
                        p("The y-axis of these line charts show the same magnitude of acceleration values shown in the map above. These plots make viewing general trends for each road over time easier. Combined with the spatial visualization, a user can get a more complete view
                          and understanding of the road conditions for the roads around Macalester."),
                        h3("Additional Visualization"),
                        p("In the implications tab, you can find an additional visualization which helps to contextualize the road conditions during some week along with the temperature highs and lows. The freeze-thaw cycle....."),
                        p("This line chart shows the dialy and weekly highs and lows for the days we recorded our accelerometer and location data. "), 
                        h2("Statistical Summaries")
                      )
               )
                     )
               )
               
               ),
    
    tabPanel("Analysis and Exploration",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h3("Road Conditions"),
                 h5(em("Explore for Yourself")),
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
               ),
               withMathJax(p("These visuals show the acceleration magnitude of our data, $$\\|{v}\\|_2 = \\sqrt{X^2 + Y^2 + Z^2}.$$ The map shows discrete points of acceleration while the line charts show the rolling average of magnitude over time.")),
               p("The higher the value, the more acceleration in multiple directions there was at the given time and location. We use these values as an indication of 'road roughness' as if the value is higher, there is more likely to be some road condition such as a pothole that caused the phone to jostle at a higher rate."),
               withMathJax(tags$p("** Select a date to view from the dropdown above. The map and line charts will update dynamically with chosen input. Click on a specific point on the map to view the comfort level and associated magnitude of acceleration. Hover over the line charts to view these trends graphically.", style = "color: black;"))
               ),
               mainPanel(
                 fluidRow(
                   column(width = 11, leafletOutput("mag_map", height = "350px"))
                 ),
                 fluidRow(
                   column(width = 12, plotlyOutput("accel_plot", height = "340px"))
                 )
               )
             )),
    
    tabPanel("Implications",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
               ),
               mainPanel(
                 fluidRow(
                   column(width = 7, plotlyOutput("temp_graph", height = "250px"))
                 )
               )
             ))
  )
)

# Define server logic required for all visualizations
server <- function(input, output) {

  output$road_lines <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolylines(data = roads,
                   color = "firebrick") %>%
      addControl( 
        html = paste0(
        "<div style='
        all: unset;
        font-size:25px;
        font-family: 'Lato', sans-serif;
        color: black;
        display: block;
      '>",
        "Route Traveled Around Macalester College"),  
        position = "topleft")%>%
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
          "Cartographer: Alayna Johnson, Macalester '25<br> Road lines created in ArcGIS Pro <br> using Create Features.",
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
  })
  
  
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
  
  output$temp_graph <- renderPlotly({
    
    temp <- ggplot(env_long, aes(
      x = date, 
      y = temp, 
      color = category,
      group = category, 
      text = paste0("Date: ", format(date, "%B %d"), "<br>Temperature (°F): ", temp)
    )) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = color_map) +
      theme_minimal() +
      labs(
        color = "Temperature Type",
        x = "Date",
        y = "Temperature (°F)"
      ) +
      theme(
        legend.title = element_text(size = 10),  
        legend.text = element_text(size = 8)
      )
    
   ggplotly(temp, tooltip = "text")
  })
  
  output$accel_plot <- renderPlotly({
    req(input$date_choice)
    selected_data <- dataset_list[[input$date_choice]]
    selected_title <- date_titles[[input$date_choice]]
    
    process_and_plot(selected_data, selected_title)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
