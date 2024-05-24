########################################
# Project: unveiling differences       # 
# Purpose: Regression modelling        #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 29.02.2024                     #
########################################

# Load packages
library(shiny)
library(tidyverse)

# Load the graphics
source("U:/projects/3 Regional birth squeezes/Subnational_birth_squeezes/functions/graphics.R")

# Load the data
load("U:/projects/3 Regional birth squeezes/Subnational_birth_squeezes/data/fert_data_subnational.Rda")

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
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # Prepare the data
        tmp <- subset(fert, year %in% input$min_year[1]:input$min_year[2] & country %in% input$selected_countries)
        tmp$outcome <- tmp[[input$fert_measure]]

        # Make a basic line graph
        ggplot(data = tmp,
          aes(x = year, y = outcome, colour = country, group = region)) +
          geom_line(alpha = .3) +
          scale_colour_brewer("Country", palette = "Set1") +
          scale_y_continuous(input$fert_measure) +
          scale_x_continuous("Year", n.breaks = 10, expand = c(0, 0))
    })
}




# Run the application 
shinyApp(ui = ui, server = server)
