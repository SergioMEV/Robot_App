#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Libraries
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(readxl)
library(htmltools)  
library(fmsb)
library(magick)

# Data
country_data <- read_excel("mapData.xlsx")
robot_data <- read_excel("Round2ProjectRobotData.xlsx", sheet = 2)
testImage <- image_read("TestImage.png")

ui <- navbarPage(
  ## Theme
  theme = shinytheme("flatly"),
  ## App Title
  "covidBot", 
  
  tabPanel(
    "covidBot", # Title
    ## Layout
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          pickerInput(
            inputId = "covidBotCountry",
            label = "Country",
            choices = country_data$Country
          ),
          verbatimTextOutput("value")
        ),
        mainPanel(
          imageOutput("img")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  countryData <- reactive({
    country = input$covidBotCountry
    
    single_data = filter(country_data, Country == country)
    
    population = single_data$Population
    GDP = single_data$'GDP Per Capita'
    deaths = single_data$'Covid Deaths Per Million'
    cases = single_data$total_cases_per_million
    category = filter(robot_data, Country == country) %>%
      subset(select=-c(Total)) %>%
      max.col()
    
  })
  
  
  
  image <- image_read("RobotImage.png")
  image <- image_trim(image)
  #output$value <- renderPrint(input$covidBotCountry)
  
  output$img <- renderImage({
    country = CountryData()
    
    if(category == "Public Safety"){
      image <- image_fill(image, "green", point = "+0+0", fuzz = 0)
    }
    if(category == "Clinical"){
      image <- image_fill(image, "blue", point = "+0+0", fuzz = 0)
    }
    if(category == "Continuity of Work/Education"){
      image <- image_fill(image, "orange", point = "+0+0", fuzz = 0)
    }
    if(category == "Quality of Life"){
      image <- image_fill(image, "pink", point = "+0+0", fuzz = 0)
    }
    if(category == "Laboratory and Supply Chain Automation"){
      image <- image_fill(image, "yellow", point = "+0+0", fuzz = 0)
    }
    if(category == "Non=Hospital Care"){
      image <- image_fill(image, "purple", point = "+0+0", fuzz = 0)
    }
    
    tmpfile <- image %>%
      image_write(tempfile(fileext='png'), format = 'png')
    list(src = tmpfile, contentType = "image/png")
  })
}

shinyApp(ui = ui, server = server)