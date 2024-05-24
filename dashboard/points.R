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
library(ggrepel)

# Load the graphics
source("U:/projects/3 Regional birth squeezes/Subnational_birth_squeezes/functions/graphics.R")

# Define the coordinate system
load("U:/projects/3 Regional birth squeezes/Subnational_birth_squeezes/data/fert_data_subnational.Rda")

# Select the columns
fert <- fert %>% 
  select(country, region, year, tfr_female, tfr_male, mac_female, mac_male)

# Mark the capitals
capital_regions <- c("District of Columbia", "Berlin", "Île-de-France", "Helsinki-Uusimaa", "Australian Capital Territory", "Ciudad de México", "Comunidad de Madrid", "Madrid")
fert <- fert %>% 
  mutate(capital = ifelse(region %in% capital_regions, 1, 0))

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
      sliderInput(inputId = "min_year",
                  label = "Years:",
                  min = 1969,
                  max = 2018,
                  value = c(2000, 2005)),
      # Select the countries you want to look at
      h2("Countries"),
      helpText("Select a few countries that you want to look at."),
      checkboxGroupInput(
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
        label = "Fertility indicators",
        choices = list(
          "Total fertility rate" = "tfr",
          "Mean age of childbearing" = "mac"
        ),
        selected = "TFR"),
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
    tmp <- subset(fert, year %in% input$min_year[1]:input$min_year[2] & country %in% input$selected_countries)
    names <- names(tmp)[str_detect(names(fert), input$fert_measure)]
    tmp$outcome_x <- tmp[[names[1]]]
    tmp$outcome_y <- tmp[[names[2]]]
    
    # Make a basic line graph
    ggplot(data = tmp, aes(x = outcome_x, y = outcome_y, colour = country)) +
      geom_point(alpha = .5) +
      geom_text_repel(data = subset(tmp, year == input$min_year[2] & capital == 1), aes(label = region), nudge_y = 0.5) +
      geom_abline(yintercept = 0, slope = 1) +
      scale_colour_brewer(palette = "Set1")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
