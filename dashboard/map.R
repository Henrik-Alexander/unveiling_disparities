########################################
# Project: unveiling differences       # 
# Purpose: Regression modelling        #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 29.02.2024                     #
########################################

# Load packages
library(shiny)
library(sf)
library(tidyverse)

# Load the graphics
source("U:/projects/3 Regional birth squeezes/Subnational_birth_squeezes/functions/graphics.R")


### Base layout ---------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Unveiling subnational sex differences in fertility"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Select the years range on the sliders
      h1("Years"),
      p("Please use the slider to change the time-series length of the data."),
      sliderInput(inputId = "year",
                  label = "Years:",
                  min = 1969,
                  max = 2018,
                  value = 2000),
      # Select the countries you want to look at
      h2("Countries"),
      helpText("Select a few countries that you want to look at."),
      selectInput(
        inputId = "selected_countries",
        label = NULL,
        choices = list("USA" = "United States",
                       "Australia" = "Australia",
                       "Germany" = "Germany",
                       "France" = "France",
                       "Finland" = "Finland",
                       "Mexico" = "Mexico",
                       "Spain" = "Spain"),
        selected = "France"
      ),
      # Select the measure of interest
      h3("Fertility indicators"),
      helpText("Select a measure you are interested in."),
      selectInput(
        inputId = "fert_measure",
        label = "Fertility measure",
        choices = list("TFR (women)" = "tfr_female",
                       "TFR (men)" = "tfr_male",
                       "Mean age of childbearing (women)" = "mac_female",
                       "Mean age of childbearing (men)" = "mac_male",
                       "Male TFR / female TFR" = "tfr_ratio",
                       "MAC (men) - MAC (women)" = "mac_diff"), 
        selected = "tfr_female"),
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$mapPlot <- renderPlot({
    # Prepare the data
    tmp <- subset(map_data, year == input$year & country == input$selected_countries)
    tmp$outcome <- tmp[[input$fert_measure]]
    #labels <- st_centroid(tmp)
    #labels <- cbind(labels, st_coordinates(st_centroid(tmp$geometry)))
    
    # Create space in the labels
    #tmp$region <- str_replace(tmp$region, " ", "\n")
    
    # Make a basic line graph
    ggplot(data = tmp, aes(fill = outcome)) +
      geom_sf() +
      #geom_text(data = labels, aes(x = X, y = Y, label = region), colour = "white", size = 2) +
      scale_fill_viridis_c(input$fert_measure) +
      scale_x_continuous("", expand = c(0, 0)) +
      scale_y_continuous("", expand = c(0, 00)) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.key.width = unit(2, "cm")
      )
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
