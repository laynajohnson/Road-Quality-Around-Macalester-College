# Road Quality Around Macalester College
The purpose of this study is to assess road quality conditions around Macalester College during the transition from winter to spring. Using a vehicle-mounted accelerometer we map spatial variations in road conditions to identify areas with poor conditions such as potholes and rough surfaces. By analyzing these variations, we provide insights into how road conditions impact vehicular safety and highlight the areas in need of maintenance. This study can contribute to a better understanding of localized road quality issues and inform decision-making for improvements. We aim to answer the following question: As the winter comes to an end, how do road quality conditions vary around Macalester College?

This is an in-progress project. Click the link to my publshed app to view my current work. https://laynajohnson.shinyapps.io/road-quality-analysis/

For test data and exploratory viz, view the quarto document in the `test_runs` folder.

For "pre-website" data cleaning and visualizations, view the quarto documents in the `recording_analysis` foler. Associated data is also available within the `data` folder

For any files related to the website, view the `Road-Quality-Analysis` folder. Associated data can be found within the `data` folder

Data between the "pre-website" and website files follows the same structure. Nested folders include `cleaned-data`, `raw-data`, and `metadata`. The folder associated with raw data contains the csv files directly exported from the *Sensor Logger* application with exported naming conventions and all variables. The folder for cleaned data contains separate folders for individual datasets renamed to refelct the correct street and order of recording and combined datasets of all data named by date of recording.

### Dependencies

* RStudio
* R version 4.4.2

### Installing

* Download R Studio here: https://posit.co/download/rstudio-desktop/
* Download R here: https://www.r-project.org/
