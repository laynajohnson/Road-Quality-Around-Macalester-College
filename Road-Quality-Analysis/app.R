
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
library(gt)


# Reading data and transforming for line map of roads (the data section in app)

roads <- read_sf("data/raw-data/macalester_road_lines/macalester_road_lines.shp")
roads <- st_transform(roads, crs = 4326)


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


# Creating the Rolling Magnitude graph with two functions


plot_accel <- function(df_colored, plot_title) {
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
  
  return(ggplotly(p, tooltip = "text") %>%
           layout(
             hovermode = "x unified",
             hoverlabel = list(font = list(size = 12))
           ))
}


# Summary tables

summarize_l2 <- function(df) {
  df %>%
    group_by(base_street) %>%
    summarise(
      times_uncomfortable = sum(L2_norm > 1.6, na.rm = TRUE),
      max_L2_norm = round(max(L2_norm, na.rm = TRUE), 2),
      avg_disc = round(mean(L2_norm, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

summary_list <- list(
  feb19 = summarize_l2(feb19),
  feb27 = summarize_l2(feb27),
  mar06 = summarize_l2(mar06),
  mar13 = summarize_l2(mar13),
  mar20 = summarize_l2(mar20),
  mar27 = summarize_l2(mar27),
  apr03 = summarize_l2(apr03)
)


combine_variable_across_dates <- function(var_name) {
  map(summary_list, ~ select(.x, base_street, !!sym(var_name))) %>%
    reduce(full_join, by = "base_street") %>%
    arrange(base_street) %>%
    rename_with(~ names(summary_list), -base_street)
}

var_labels <- list(
  "times_uncomfortable" = "Number of Times Uncomfortable",
  "avg_disc" = "Average Magnitude of Acceleration",
  "max_L2_norm" = "Highest Magnitude of Acceleration"
)

select_labels <- c(
  "Discomfort" = "times_uncomfortable",
  "Average Acceleration" = "avg_disc",
  "Highest Acceleration" = "max_L2_norm"
)

render_gt_table <- function(df, var_name) {
  gt_tbl <- df %>% gt(rowname_col = "base_street")
  
  # Apply blue heatmap to all variables
  gt_tbl <- gt_tbl %>%
    data_color(
      columns = everything(),
      colors = scales::col_numeric(
        palette = c("white", "#456193"),
        domain = range(df[-1], na.rm = TRUE)
      )
    )
  
  gt_tbl %>%
    tab_header(
      title = var_labels[[var_name]]
    ) %>%
    tab_options(
      data_row.padding = px(2),
      table.font.size = "medium"
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
    
    tabPanel("The Research",
             mainPanel(width = 12,
                       tabsetPanel(
                         tabPanel("Introduction",
                                  h1("--"),
                                  h2("Analyzing Road Conditions around Macalester College"),
                                  h3("Road Quality"),
                                  p("Rough roads can cause accidents and damages to vehicles driving over them. You could pop a tire on a large pothole or side-swipe another vehicle while trying 
                                  to avoid damaging your own car on a large dip in the road. According to a study done in Italy, potholes and other types of road pavement distress are perceived as the most dangerous
                                  factors of driving to 41.6% of those riding two-wheeled vehicles and to 32.8% of those riding four-wheeled vehicles (Martinelli et al., 2022). Not only can road conditions be dangerous
                                  for your vehicle, but continuous exposure can also be harmful for your health. According to the International Organization for Standardization (1997), 
                                  exposure to whole-body vibrations can be dangerous to your bodily health and safety. These whole-body vibrations are present as you drive and can intensify over rough roads.
                                  Roads are essential to travel for many people, so they should be quality enough to feel comfortable driving on them year-round. 
"),
                                  p("Our research focuses on collecting accelerometer data to discover the comfort levels on the streets around Macalester College. With this data, we hope to gain some understanding of 
                                    road quality around Macalester over time. We focus on the transition period between winter and spring starting in February and ending in early April. Through the data collection and analysis, we seek to answer the following:"),
                                  tags$p(" How do road quality conditions vary around Macalester College as winter transitions into spring?", style = "color: black;"),
                                  h3("Freeze-Thaw Cycle"),
                                  p("Along with prior research on the usefulness of accelerometers to measure road quality, we also looked into literature surrounding the relation of the freeze-thaw cycle to potholes. There are many different research articles focusing
                                  on the freeze-thaw cycle in relation to road quality, most specifically, potholes. Djabatey (2023) researched this relationship and summarized how roads respond to environmental changes as:"),
                                  p("1. Low temperature: roads are more likely to crack rather than simply flow or rut (depressions in road due to traffic use)."),
                                  p("2. High precipitation: Roads become more saturated and are susceptible to the effects of water pressure (mainly expansion when freezing)."),
                                  p("These two ideas will guide the research to answer our primary question of effects of season change on road quality."),
                                  h3("Accelerometers"),
                                  p("Accelerometers are a relatively affordable and convenient way to measure the quality of roads while traveling on them. These meters can measure the specific acceleration changes of
                                    a vehicle they are attached to. This data in turn can be used as a proxy for measurements of  the quality of the pavement being driven on. This research started with a literature review of 
                                    studies using accelerometers similarly to measure road quality and detect or classify things like potholes and manhole covers. Through this review, we discovered that
                                    accelerometers are an established way to measure surface roughness or road quality."),
                                  p("There are many application options you can download and use if your phone has a built in accelerometer (which most smartphones do). The app we used for our data collection is called Sensor Logger. 
                                  There are a large variety of available recording options for sensors such as the accelerometer, gyroscope, camera, and GPS. The app also is capable of simply exporting data as individual or combined CSVs or other compatible forms. 
                                  Helpfully, this app has detailed documentation on its", 
                                    a("website", href = "https://www.tszheichoi.com/sensorlogger", target = "_blank"),
                                    "and corresponding",
                                    a("GitHub.", href = "https://github.com/tszheichoi/awesome-sensor-logger", target = "_blank")),
                                  h1("--")
                                  ),
                         
                         tabPanel("Data Methods",
                                  h1("--"),
                                  h4("How do road quality conditions vary around Macalester College as winter transitions into spring?"),
                                  h2("Data Collection"),
                                  p("In order to answer this question, we collect data with accelerometer and GPS sensors mounted in a vehicle using the Sensor Logger app on an iOS device (iPhone 14). The app is available for both iOS and Android devices. These sensors are built into the device itself and the app helps to record this data for export and analysis. The phone is secured to a phone-holder system that attaches to the vent of the car and holds the phone in an upright and level position. When using the accelerometer and GPS sensor options, we collect three variables of interest for this study: acceleration in +/- X, Y, and Z directions, time, and location. Additional details about these collected data is included in the Data Cleaning section. Along with these data recorded through the Sensor Logger app, general condition variables are also collected. When measuring and considering road quality, it is important to think about environmental influences that can alter recordings such as weather events and precipitation. Refer to the end of Collection Details for more information on supplementary data."),
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
                                  p("Any potential errant points occur at the start of data collection (time_elapsed = 0), so these points are removed from the data. There is some variation between points between different recording times on the same roads as speed can be different each drive. This kind of variation is not adjusted for, but should be mentioned."),
                                  h1("--")
                         ),
                         tabPanel("Behind the Analysis",
                                            h1("--"),
                                            h2("Data Visualization"),
                                            p("These visualizations will show trends and changes over time graphically. There are two main areas of focus for these visualizations: to represent road conditions over time and conditions over space and time. The variables to be used for these visualizations include acceleration, time, and location. You can view these visualizations in the next two tabs."),
                                            withMathJax(p("Throught these visualizations, we seek to answer our research question in an exploratory fashion: How do road quality conditions vary around Macalester College as winter transitions into spring? As a proxy to road conditions, we can calculate the smoothed acceleration magnitude of our data. This involves taking the magnitude of our \\(X\\), \\(Y\\), and \\(Z\\) acceleration values in \\(m/s^2\\) as, $$\\|{v}\\|_2 = \\sqrt{X^2 + Y^2 + Z^2}$$ (also called the L2 norm) and compute the rolling average of the magnitude values over time. We can learn about the magnitude of acceleration at each point independent of the direction of acceleration. This will help to understand events with significant acceleration in one or more directions and quiets some of the noise from measuring errors (Martinez-Ríos et al., 2022).")),
                                            h3("Spatial Visualization"),
                                            p("For spatial visualization, we created maps which show the magnitude of acceleration values as described before broken into categories using the international ISO 2631-1 Standard defining levels of comfort:"),
                                            withMathJax(p("\\(\\|v\\|< 0.315 m/s^2\\) as being comfortable,  \\(0.315m/s^2 < \\|v\\| < 1.6 m/s^2\\) as fairly comfortable, \\(1.6m/s^2 < \\|v\\| < 2 m/s^2\\) as uncomfortable and \\(\\|v\\| > 2 m/s^2\\) being extremely uncomfortable (International Organization for Standardization [ISO], 1997).")),
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
                                            p("In the implications tab, you can find an additional visualization which helps to contextualize the road conditions during the study period. The stacked graph contains information on weekly and daily temperatures as well as the total accumulated precipitation during the week
                                              preceding the day of collection. With this visualization we can explain some connections to the freeze-thaw cycle."),
                                            h2("Statistical Summaries"),
                                            p("Statistical summaries for our research are located in the Analysis tab. These summary tables show different summaries of road quality for 
                          the different times and base streest in the analysis. The first selection called 'Discomfort' shows the number of times each base street has a magnitude equal to
                          or greater than the value for uncomfortable acceleration in meters per second squared (1.6). The next two selections are 'Highest Acceleration'
                          and 'Average Acceleration' which show the highest magnitude of acceleration and average acceleration per date of recording time and base street.
                          Each variable selection is colored in a gradient of blues which show the darkest color as the highest value of the selected variable per time and street.
                          Using these colored tables, one can more easily see some of the trends present in the previous visuals."),
                                            h1("--")
                                  )
                         )
                       )
             ),
    tabPanel("Data Analysis",
             mainPanel(width = 12,
                       tabsetPanel(
                         tabPanel("Data Exploration",
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
                                        column(width = 11, leafletOutput("mag_map", height = "340px"))
                                      ),
                                      fluidRow(
                                        column(width = 12, plotlyOutput("accel_plot", height = "350px"))
                                      )
                                    )
                                  )),
                         tabPanel("Analysis",
                                      sidebarLayout(
                                      sidebarPanel(width = 3,
                                                   h3("Analysis"),
                                                   h5(em("Select a variable to view the associated summaries.")),
                                                   selectInput(
                                                     inputId = "metric_choice",
                                                     label = "Select Variable for Table:",
                                                     choices = select_labels
                                                   ),
                                                   p("The table to the right shows selected values for each street (combined from all directions or lanes) for each recording date."),
                                                   withMathJax(p("Discomfort: The number of times the magnitude of acceleration on a given road and date went into the 'Uncomfortable' or 
                               'Extreme Discomfort' zones. If the L2 norm was equal to or greater than \\(1.6 \\space m/s^2\\) then it was counted as one instance. ")),
                                                   p("Average Acceleration: The average magnitude of acceleration on a given road and date."),
                                                   p("Highest Acceleration: The maximum value of magnitude of acceleration for a given road and date."),
                                                   tags$p("**Select a variable from the dropdown above to update the summary table and description.", style = "color: black;")
                                                 ),
                                  mainPanel(
                                    fluidRow(
                                               column(10, offset = 1, gt_output("summary_table")),
                                               column(12, htmlOutput("summary_explanation"))
                                             
                                    )
                                  )
                         )),
                         tabPanel("Discussion",
                                    h1("--"),
                                    h2("Implications"),
                                    h3("Season Change"),
                                  p("Our primary research question focuses on the temporal aspect of season change as a factor effecting the quality of roads. We explained the freeze-thaw cycle in the introduction, and we will pull concepts from these ideas to contextualize our research and implications. To start, we have a simple line graph showing what temperature highs and lows looked
                                    like over the duration of our study period. From the graph below, we can see that every week inside the recording window experienced above and below freezing temperatures aside from the first date. Precipitation wise, we can see from the bar plot that within some weeks there was precipitation, and others there was not."),
                                  fluidRow(
                                    column(8, plotlyOutput("temp_precip", height = "400px"), offset = 2)
                                  ),
                                  p("As prior research focuses on the combination of precipitation in the cracks and voids of the asphalt paired with the freezing expansions and thawing potholes left over, we can view our data results with these ideas. In theory, we should see that after some instance of precipitation where water was able to seep into the cracks during warmer temperatures then
                                    freeze and expand, there would be more uncomfortable roads. This means on the dates of March 6th, 13th, and 20th as well as April 3rd we might expect to see higher values of our variables of interest than from the recording of the week prior. From comparing these ideas with the tables in the previous section we do not see this trend. Generally, there is not one 
                                    set trend in how any variable between discomfort instances, average L2 norm, and highest L2 norm differs from time to time."),
                                  p("Some things to consider adjusting for in the future could be things like asphalt composition, traffic flow between streets, and presence of small debris like dirt and gravel expanding in cracks with the water (Djabatey, 2023). The Saint Paul Public works site on the asphalt plant in Saint Paul that supplies the asphalt for all repair projects does not have any
                                    detailed information on the exact composition of the material used on our roads. If such data was easily available, it could be used to understand how permeable the roads driven on are and how susceptible they may be to worsening cracks and holes from the freeze-thaw cycle. "),
                                  h3("Road Maintenence"),
                                  p("Another question to ask when studying road quality is this: When was the last time each street had maintenence to address poor road conditions? On the Saint Paul Public Works website, you can find information on past and
                                      present construction projects. In Saint Paul, residential streets typically get seal coated in rotation every eight years (Seal Coating Program | Saint Paul Minnesota, n.d.). According to Public Works, the city is divided into eight areas, and the area which
                                      Macalester Groveland is in was last seal coated in 2021. The process of seal coating is a preventative measure which will result in a waterproof membrane which helps to seal the surface and small cracks. The busier arterial streets will go through a different 
                                      process called 'Mill & Overlay' which is a more extreme measure. 
                                      "),
                                  p("Each year, the city has some money allocated to fixing roads with the mill and overlay process. The selection process is based on the overall condition of a road and how busy it is (Mill & Overlay | Saint Paul Minnesota, n.d.).
                                      Within the study area, Summit Avenue was selected for this maintenence in the summer of 2023 due to deteriorating street conditions. The main two-way part of Summit went throught the entire mill and overlay process to repave the road. However, the side parts of Summit (parking streets)
                                      were only 'pothole patched where needed' (Summit Avenue Pavement Treatment | Saint Paul Minnesota, n.d.). In the exploratory visualizations, you may have noticed that these parking streets of Summit Avenue contained more readings in the uncomfortable range. From the visual appearance of
                                      the two parts of the road as well as experience driving them, the difference in quality is clear."),
                                  p("During the 2024 construction season, Snelling Avenue between St. Clair and Grand Ave. was also resurfaced in an ongoing construction project (Hwy 51/Snelling Ave. in St. Paul Project - MnDOT, n.d.). There are no additional details on what this resurfacing entailed, but the driving experience
                                    is smooth. This is also reflected in the statistical summaries with a relatively low average magnitude of acceleration. It also appears that no other street on the route was serviced outside of typical maintence which does include pothole patching when necessary."),
                                  h3("Whole-Body Vibrations"),
                                  p("In the introduction, we mentioned the idea of the harms of whole-body vibrations and how these can be caused by consistent travel over rough surfaces (ISO, 1997). By looking at the average values of magnitude of acceleration for each road in our study area, we would not necessarily be concerned
                                    over the risk of incurring these kinds of bodily harms traveling over these roads. The values in the average L2 norm table ranged from classifying as comfortable to fairly uncomfortable. Macalester Street does have the highest average magnitude of acceleration across time, but with the added 
                                    knowledge that there are speedbumps, we wouldn't consider this to be a source of freeze-thaw cycle or the city failing to patch holes. As all roads fall consistently with in the comfortable to fairly uncomfortable categories, we would not be concerned about whole-body vibration impacts."),  
                                  h2("Strengths and Limitations"),
                                    p("The choice of using the magnitude of acceleration for this reasearch has the benefit of easier interpretations and explanation of the calculation. On the other hand, the chosen measure of road quality as the magnitude 
                                    of acceleration rather than some other value can also be a limitation of this study. There are many different measurements found in literature that can be calculated from accelerometer data and used to 
                                    discover indications of poor road quality. These measure include simple vertical acceleration (Y acceleration in this case) to focus on instances of potholes or other vertical bumps, International Roughness Index 
                                    (IRI) as a road quality index (“Road Quality Assessment Using International Roughness Index Method and Accelerometer on Android,” n.d.), and Power Spectral Density (PSD) (Chen et al., 2011). These last two measures are
                                      more complicated to calculate, and therefore would be harder to explain to a general audience. Additionally, finding easy-to-navigate packages with good documentation for such specific measures was a challenge, leading
                                      us to focus on using the L2 norm. Simply using the Y (vertical) acceleration as an indicator of road quality has its drawbacks in how the mounted iPhone accelerometer jostles in the car over rough road conditions. Generally, if
                                      there is some road condition which the vehicle has to go over, you would feel it in a more up-and-down fashion. However, through observing how the mounted phone reacted to road conditions in the car in the manner in which it
                                      was mounted, the magnitude of acceleration seemed to be a better alternative to capture all movement while quieting unecessary noise."),
                                  h2("Conclusions"),
                                  p("Within the confines of our study, it does not appear that the change in seasons was directly connected to worsening road quality, as was evident from our visual and statistical results. Some factors that could be interacting with the qualty of the study conducted
                                    include the small study area and short data collection window. With additional funding and time, these aspects could have become more robust, leading to more quality data and analysis capabilities. Additionally, with more time, a more detailed or novel method of
                                    analysis could have been explored. Additional properties of the asphalt and traffic could also have made a more complete analysis."),
                                    h1("--")
                                      
                                  ))
                       )),
    
          tabPanel("Supporting Materials",
             mainPanel(
               tabsetPanel(
                 tabPanel("Bibliography",
                          h1("--"),
                          h2("Sources"),
                          p("Chen, K., Lu, M., Fan, X., Wei, M., & Wu, J. (2011). Road condition monitoring using on-board Three-axis Accelerometer and GPS Sensor. 2011 6th International ICST Conference on Communications and Networking in China (CHINACOM), 1032–1037. https://doi.org/10.1109/ChinaCom.2011.6158308
"),
                          p("Djabatey, C. (2023, July 21). Experimenting and modelling the role of road surface detritus in the formation of potholes [Thesis (University of Nottingham only)]. University of Nottingham. https://eprints.nottingham.ac.uk/73466/
"),
                          p("Hwy 51/Snelling Ave. In St. Paul Project—MnDOT. (n.d.). Retrieved May 1, 2025, from https://www.dot.state.mn.us/metro/projects/snellingave-stpaul/index.html
"),
                          p("International Organization for Standardization. (1997). ISO 2631-1:1997 – Mechanical vibration and shock — Evaluation of human exposure to whole-body vibration — Part 1: General requirements. https://cdn.standards.iteh.ai/samples/7612/0d54768e6e214481a8e814b20df83641/ISO-2631-1-1997.pdf"),
                          p("Martinelli, A., Meocci, M., Dolfi, M., Branzi, V., Morosi, S., Argenti, F., Berzi, L., & Consumi, T. (2022). Road surface anomaly assessment using low-cost accelerometers: A machine learning approach. Sensors, 22(10), 3788. https://doi.org/10.3390/s22103788"),
                          p("Martinez-Ríos, E. A., Bustamante-Bello, M. R., & Arce-Sáenz, L. A. (2022). A Review of Road Surface Anomaly Detection and Classification Systems Based on Vibration-Based Techniques. Applied Sciences, 12(19), Article 19. https://doi.org/10.3390/app12199413
"),
                          p("Mill & Overlay | Saint Paul Minnesota. (n.d.). Retrieved April 30, 2025, from https://www.stpaul.gov/departments/public-works/street-maintenance/mill-overlay
"),
                          p("Road Quality Assessment Using International Roughness Index Method and Accelerometer on Android. (n.d.). ResearchGate. https://doi.org/10.24843/LKJITI.2019.v10.i02.p01
"),
                          p("Seal Coating Program | Saint Paul Minnesota. (n.d.). Retrieved April 30, 2025, from https://www.stpaul.gov/departments/public-works/street-maintenance/seal-coating-program
"),
                          p("Summit Avenue Pavement Treatment | Saint Paul Minnesota. (n.d.). Retrieved April 30, 2025, from https://www.stpaul.gov/projects/public-works/pw2023summitaveresurfacing
")
                          
                          ),
                 tabPanel("Appendix",
                          h1("--"),
                          h2("Data Availability"),
                          p("All data and corresponding cleaning and visualization files compiled prior to the final website (and the Shiny web app file) can be found
                            in this",
                            a("GitHub repository.", href = "https://github.com/laynajohnson/Road-Quality-Around-Macalester-College", target = "_blank")),
                          p("Directions for how to navigate the repo can be foundin the README description on the home code page."),
                          h2("Package Dependencies"),
                          h3("UI & App Functionality:"),
                          p("  - shiny"),
                          p("  - shinythemes"),
                          p("  - shinyWidgets"),
                          
                          h3("Data Analysis & Visualization:"),
                          p("  - tidyverse"),
                          p("  - ggplot2"),
                          p("  - plotly"),
                          p("  - sf"),
                          p("  - leaflet"),
                          p("  - RColorBrewer"),
                          p("  - htmltools"),
                          p("  - gt")
                          )
               )
             )
      
    )
    ),
  tags$footer(
    tags$hr(),
    p("Alayna Johnson | May 1, 2025", style = "text-align: center;"),
    style = "padding: 15px; font-size: 0.9em; line-height:1; height: 10px;"
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
  
  output$temp_precip <- renderPlotly({
    # Temperature plot
    temp_plot <- ggplot(env_long, aes(
      x = date,
      y = temp,
      color = category,
      group = category,
      text = paste0("Date: ", format(date, "%B %d"), "<br>Temperature (°F): ", round(temp, 1))
    )) +
      geom_line() +
      geom_point() +
      geom_hline(yintercept = 35, color = "darkgray", alpha = 0.7, linetype = "dotted") +
      scale_color_manual(values = color_map) +
      theme_minimal() +
      labs(
        color = "Temperature Type",
        x = NULL,
        y = "Temperature (°F)"
      ) +
      theme(
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)
      )
    
    # Precipitation plot
    precip_plot <- ggplot(environment_data, aes(
      x = date, y = precip_total,
      text = paste0("Date: ", format(date, "%B %d"), "<br>Total Precipitation: ", precip_total, " in")
    )) +
      geom_col(fill = "#456193") +
      scale_x_date(date_labels = "%b %d", date_breaks = "7 days") +
      theme_minimal() +
      labs(
        x = "Date",
        y = "Precipitation (in)"
      ) 
    
    temp_interactive <- ggplotly(temp_plot, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
    precip_interactive <- ggplotly(precip_plot, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
    
    # Combining the two plots into one
    subplot(temp_interactive, precip_interactive,
            nrows = 2,
            shareX = TRUE,
            titleY = TRUE,
            heights = c(0.7, 0.3)) %>%
      layout(
        title = list(
          text = "Weather Conditions: February 19th – April 3rd",
          x = 0.5,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 18)
        ),
        margin = list(t = 50)
      )
    
  })
  
  
  output$accel_plot <- renderPlotly({
  req(input$date_choice)
  selected_data <- dataset_list[[input$date_choice]]
  selected_title <- date_titles[[input$date_choice]]

  plot_accel(selected_data, selected_title)
})
  
  output$summary_table <- render_gt({
    req(input$metric_choice)
    df <- combine_variable_across_dates(input$metric_choice)
    render_gt_table(df, input$metric_choice)
  })
  
  output$summary_explanation <- renderUI({
    req(input$metric_choice)
    
    explanation_text <- switch(input$metric_choice,
                               "times_uncomfortable" = "The discomfort measurement, as described in the side panel, can help us to understand some level of trend
                               in comfort of the drive over the roads in the study area during the specified times. The colors in the table are a gradient from light blue
                               indicating less instances of discomfort to a darker blue meaning higher number of moments of discomfort.
                               <br> <br> From this table, Macalester Street appears to have the most instances of discomfort while driving from totaling each date. However, Macalester Street
                               has five speedbumps which can cause the car to jostle more. This could be why the readings seem much higher. The road with the second
                               most instances of discomfort is Summit Avenue. This can mostly be attributed to the side streets, as the main part of Summit Ave
                               was repaved within the last two years and is still fairly smooth. Interestingly, the two most 'uncomfortable' dates are the first two
                               days of recording: February 19th and February 27th. The date with the third most still has 11 less instances of discomfort than the second (March 20th).",
                               "avg_disc" = "The averaged magnitude of acceleration measurement, as described in the side panel, shows us the overall trend of 'smoothness' for each
                               road over time in the study period. The colors in the table are a gradient from light blue indicating lower average L2 norm to a darker blue meaning
                               higher average magnitude of acceleration.
                               <br> <br> The average magnitude of acceleration is highest again for Macalester Street. This street
                               has five speedbumps which can cause the car to jostle more. This could be why the readings seem much higher. Lincoln Avenue and Vernon and Cambridge
                               streets both come in second for highest average L2 norm at 0.65. The date with the highest average is March 20th, with February 27th close behind.
                               We might expect later dates to have higher average magnitude of acceleration, yet the second and fifth recordings of the seven were. This could be 
                               because starting near the end of March, some potholes were beginning to be filled.",
                               "max_L2_norm" = "This measure does not tell us as much about the general trends of road conditions during the study time like the 
                               previous two summaries. When looking at the highest magnitude of acceleration for each road, we can get a sort of simple understanding
                               of what road might have had a particularly rough part. 
                               <br> <br> The highest L2 norm is on Lincoln Avenue on February 27th. The total discomfort for Lincoln is on the lower end out of all
                               roads in the study, so it is interesting to see that it has the highest measurement yet not the highest discomfort or average L2 norm."
    )
    
    withMathJax(HTML(paste0("<h4>Key Table Observations:</h4><p>", explanation_text, "</p>")))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
