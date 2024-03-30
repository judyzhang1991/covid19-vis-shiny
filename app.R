#---
# title: "app"
# author: "Jingyang(Judy) Zhang"
# date: "7/13/2020"
# note: this R script is the main file for running the application to create an interactive visualization of covid 19 cases and air travel over time.
#---



# LOAD PACKAGES

library(shiny)

library(maps)

library(tidyverse)

library(janitor)

library(sf)


library(ggmap)

library(ggrepel)

library(igraph)

library(sna)

library(geosphere)


library(hash)

library(GGally)

library(shinyWidgets)

library(RCurl)

library(lubridate)


# LOAD SOURCE FILES
source("map_covid.R")
source("network_flights.R")


# LOAD DATA

## MAP DATA ##
## Note: the dataset is pulled from the R package {ggplot2}.

map_dat <- map_data("world") %>%
  mutate(region = tolower(region))  %>%
  plyr::rename(replace = c(region = "country_region"))


countries <- sort(unique(c(direct_routes$source_country, direct_routes$dest_country)))


# USER INTERFACE  
ui <- fluidPage(
  
  includeCSS("data/styles.css"),
  
  titlePanel("Covid 19 Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
  
      # Dropdown widget so user can select a type of case
      selectInput("var", 
                  label = "Select display cases:",
                  choices = c("Confirmed Cases", 
                              "Recovered Cases", 
                              "Deaths Cases"),
                  selected = "Confirmed Cases"),
      
      sliderTextInput("date", 
                  label = "Date of interest:",
                  choices = format(seq(as.Date("2020/1/22"), as.Date(Sys.Date() - 1), by = "day"), "%m-%d-%Y"),
                  selected = format(as.Date(Sys.Date() - 1), "%m-%d-%Y"),
                  grid = TRUE,
                  dragRange = TRUE,
                  animate = TRUE),
      
      radioButtons("inout",
                   label = "Flight out of/Flight into",
                   choices = list("Flight out of" = "Out",
                                  "Flight into" = "In"),
                   selected = "In"),
      
      # Dropdown widget so user can select a country
      selectInput("country", 
                  label = "Select a country:",
                  choices = countries,
                  selected = "United States"),
    ),
   
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Map", plotOutput("map", width = "100%")),
                  
                  tabPanel("Network", plotOutput("network", width = "100%"))
                
                  )
              
    )
  )
)

# SERVER LOGIC
server <- function(input, output) {
  
  ## Map output
  output$map <- renderPlot({
    
    # Get data of the given date: format 
    dat <- getDat(input$date) %>% 
      clean_coviddat() 
    
    dat_geo <- joinDat(dat, map_dat)
    
    # Assign category
    dat_geo_categ <- assign_categ(dat_geo, input$var)
  
    
    color <- switch(input$var,
                    "Confirmed Cases" = c(
                      "0-199" = "#FFFFB2",
                      "200-999" = "#FED976",
                      "1,000-9,999" = "#FD8D3C",
                      "10,000-499,999" = "#F03B20",
                      "500,000+" = "#BD0026"),
                    
                    "Recovered Cases" = c(
                      "0-199" = "#D9F0A3",
                      "200-999" = "#ADDD8E",
                      "1,000-9,999" = "#78C679",
                      "10,000-499,999" = "#238443",
                      "500,000+" = "#005A32"),
                    
                    "Deaths Cases" = c(
                      "0-199" = "#D0D1E6",
                      "200-999" = "#A6BDDB",
                      "1,000-9,999" = "#67A9CF",
                      "10,000-499,999" = "#1C9099",
                      "500,000+" = "#016C59"))

   
    covid_map(dat_geo_categ, input$var, color)
    
    
    
  },
  height = 600, width = 800
  )
  
  ## Network output
  output$network <- renderPlot({
    
    # Get data of the given country of the given date "01-22-2020"
    dat <- getDat(input$date) %>% 
      clean_coviddat() 
    
    dat_geo <- joinDat(dat, map_dat)
    
    # Assign category
    dat_geo_categ <- assign_categ(dat_geo, input$var)
    
    color <- switch(input$var,
                    "Confirmed Cases" = c(
                      "0-199" = "#FFFFB2",
                      "200-999" = "#FED976",
                      "1,000-9,999" = "#FD8D3C",
                      "10,000-499,999" = "#F03B20",
                      "500,000+" = "#BD0026"),
                    
                    "Recovered Cases" = c(
                      "0-199" = "#D9F0A3",
                      "200-999" = "#ADDD8E",
                      "1,000-9,999" = "#78C679",
                      "10,000-499,999" = "#238443",
                      "500,000+" = "#005A32"),
                    
                    "Deaths Cases" = c(
                      "0-199" = "#D0D1E6",
                      "200-999" = "#A6BDDB",
                      "1,000-9,999" = "#67A9CF",
                      "10,000-499,999" = "#1C9099",
                      "500,000+" = "#016C59"))

    covid_map <- covid_map(dat_geo_categ, input$var, color)

    plot_routes(direct_routes, travel_rest_dat, input$country, covid_map, input$inout, input$date)
 
  },
  height = 600, width = 800
  )
 
}

# RUN APP
shinyApp(ui, server)