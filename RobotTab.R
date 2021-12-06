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

# Data. Countries with no robots are filtered out.
country_data <- read_excel("mapData.xlsx")
country_data <- filter(country_data, Top_Category != "NA")

# UI section of app
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
          # Drop down menu to select country to visualize
          pickerInput(
            inputId = "covidBotCountry",
            label = "Country",
            choices = country_data$Country
          ),
          htmlOutput("label")
        ),
        mainPanel(
          imageOutput("img")
        )
      )
    )
  )
)

# Server section of app
server <- function(input, output, session) {
  
  # Base image of robot and money bag are not reactive so are read here with
  # some basic modifications
  robot <- image_read("RobotImage.png") %>%
    image_trim()
    # image_fill("none", point = "+0+0", fuzz = 0) %>%
    # image_fill("none", point = "+345+0", fuzz = 0) %>% 
    # image_fill("none", point = "+345+300", fuzz = 0) %>%
    # image_fill("none", point = "+0+300", fuzz = 0) %>%
    # image_fill("none", point = "+200+330", fuzz = 0) %>%
    # image_fill("none", point = "+450+150", fuzz = 0)
  
  money <- image_read("MoneyBag.png") %>%
    image_trim() %>%
    image_fill("none", point = "+0+0", fuzz = 0) %>%
    image_fill("none", point = "+300+0", fuzz = 0) %>%
    image_fill("none", point = "+0+300", fuzz = 0) %>%
    image_fill("none", point = "+390+300", fuzz = 0) %>%
    image_scale("50x50")
  
  # Reactive image modifications
  output$img <- renderImage({
    # Get the selected country and store key information for easier access.
    country = input$covidBotCountry
    single_data = filter(country_data, Country == country)
    population = 180 -single_data$populationRank %>% as.numeric()
    gdp = single_data$gdpRank
    deaths = single_data$Covid_Deaths_Per_Million %>% as.numeric()
    cases = 180 - single_data$caseRank
    category = single_data$Top_Category
    
    # Color of the central body determined by which category of robot is most
    # used in the country. Colors chosen by the category's highlighting in the
    # original data set.
    if(category == "Public Safety"){
      robot <- image_fill(robot, "light green", point = "+250+75", fuzz = 0)
    }
    if(category == "Continuity of Work/Education"){
      robot <- image_fill(robot, "#f0bd26", point = "+250+75", fuzz = 0)
    }
    if(category == "Quality of Life"){
      robot <- image_fill(robot, "pink", point = "+250+75", fuzz = 0)
    }
    if(category == "Laboratory and Supply Chain Automation"){
      robot <- image_fill(robot, "light yellow", point = "+250+75", fuzz = 0)
    }
    if(category == "Non-Hospital Care"){
      robot <- image_fill(robot, "#d633f2", point = "+250+75", fuzz = 0)
    }
    
    # Color of mask held by the robot determined by (rank of) total Covid cases.
    
    # Color of needle determined by (rank of) total Covid deaths
    
    # Obsolete snippet, remove when top two are changed. Maybe keep brightness
    # related to population?
    robot <- image_modulate(robot, brightness = 100 + 4*(log10(population)),
                            saturation = 100 + (cases / 3.3),
                            hue = 100 + 10*(log10(deaths)))
    
    # Size of money bag changed according to rank of country's GDP per capita.
    if(gdp <= 36){
      money <- image_scale(money, "150x150")
    } else if (gdp <= 72){
      money <- image_scale(money, "125x125")
    } else if (gdp <= 108){
      money <- image_scale(money, "100x100")
    } else if (gdp <= 144){
      money <- image_scale(money, "75x75")
    }
    
    img = image_composite(robot, money, gravity = "south")
    
    tmpfile <- img %>%
      image_write(tempfile(fileext='png'), format = 'png')
    list(src = tmpfile, contentType = "image/png")
  }, deleteFile = TRUE)
  
  # Reactive text describing how the image is altered
  output$label <- renderUI({
    country = input$covidBotCountry
    
    single_data = filter(country_data, Country == country)
    
    population = single_data$Population %>% as.numeric()
    gdp = single_data$gdpRank
    numGDP = single_data$GDP_Per_Capita %>% as.numeric()
    deaths = single_data$Covid_Deaths_Per_Million %>% as.numeric()
    cases = single_data$Covid_Cases_Per_Million %>% as.numeric()
    category = single_data$Top_Category
    
    if(category == "Public Safety"){
      str1 <- paste(country, " has mostly ", category, " robots, so the base color is green. ")
    }
    if(category == "Clinical"){
      str1 <- paste(country, " has mostly ", category, " robots, so the base color is blue. ")      
    }
    if(category == "Continuity of Work/Education"){
      str1 <- paste(country, " has mostly ", category, " robots, so the base color is orange. ")
    }
    if(category == "Quality of Life"){
      str1 <- paste(country, " has mostly ", category, " robots, so the base color is pink. ")
    }
    if(category == "Laboratory and Supply Chain Automation"){
      str1 <- paste(country, " has mostly ", category, " robots, so the base color is yellow. ")
    }
    if(category == "Non-Hospital Care"){
      str1 <- paste(country, " has mostly ", category, " robots, so the base color is purple. ")
    }
    
    str2 <- paste("A population of ", population, " million increases brightness by ",
          round(4*(log10(as.numeric(population))), 2), "%. ")
    str3 <- paste(round(cases, 2), " Covid cases per million increases saturation by ",
          round((180 - single_data$caseRank) / 3.3, 2), "%. ")
    str4 <- paste(round(deaths, 2), " Covid deaths per million increases hue by ",
          round(10*(log10(as.numeric(deaths))), 2), ". ")
    
    if(gdp <= 36){
      str5 <- paste("A GDP per capita of $", round(numGDP, 2), " makes the bag very big.")
    } else if (gdp <= 72){
      str5 <- paste("A GDP per capita of $", round(numGDP, 2), " makes the bag big.")
    } else if (gdp <= 108){
      str5 <- paste("A GDP per capita of $", round(numGDP, 2), " makes the bag medium.")
    } else if (gdp <= 144){
      str5 <- paste("A GDP per capita of $", round(numGDP, 2), " makes the bag small.")
    } else {
      str5 <- paste("A GDP per capita of $", round(numGDP, 2), " makes the bag very small.")
    }
    
    HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
  })
}

shinyApp(ui = ui, server = server)