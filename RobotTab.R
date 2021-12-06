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
country_data <- filter(country_data, Top_Category != "NA" & GDP_Per_Capita != "NA")

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
    pop = single_data$Population %>% as.numeric()
    gdp = single_data$gdpRank
    deaths = single_data$Covid_Deaths_Per_Million %>% as.numeric()
    cases = as.numeric(single_data$Covid_Cases_Per_Million)
    category = single_data$Top_Category
    casesQuantile <- unname(quantile(as.numeric(country_data$Covid_Cases_Per_Million), na.rm = TRUE))
    deathsQuantile <- unname(quantile(as.numeric(country_data$Covid_Deaths_Per_Million), na.rm = TRUE))
    popQuantile <- unname(quantile(as.numeric(country_data$Population), na.rm = TRUE))
    
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
    if(cases <= casesQuantile[2]){#first quartile
      robot <- image_fill(robot, "lightcyan", point = "+455+180", fuzz = 0)
    }else if(cases <= casesQuantile[3]){#second quartile
      robot <- image_fill(robot, "lightblue2", point = "+455+180", fuzz = 0)
    }else if(cases <= casesQuantile[4]){#third
      robot <- image_fill(robot, "lightskyblue", point = "+455+180", fuzz = 0)
    }else if(cases <= casesQuantile[5]){#fourth
      robot <- image_fill(robot, "dodgerblue4", point = "+455+180", fuzz = 0)
    }
    # Color of hat symbol determined by (rank of) total Covid deaths
    if(deaths <= deathsQuantile[2]){#first quartile
      robot <- image_fill(robot, "lightpink", point = "+250+30", fuzz = 0)
    }else if(deaths <= deathsQuantile[3]){#second quartile
      robot <- image_fill(robot, "brown1", point = "+250+30", fuzz = 0)
    }else if(deaths <= deathsQuantile[4]){#third
      robot <- image_fill(robot, "red3", point = "+250+30", fuzz = 0)
    }else if(deaths <= deathsQuantile[5]){#fourth
      robot <- image_fill(robot, "red4", point = "+250+30", fuzz = 0)
    }
    # Color of needle determined by population
    if(pop <= popQuantile[2]){#first quartile
      robot <- image_fill(robot, "moccasin", point = "+50+130", fuzz = 0)
    }else if(pop <= popQuantile[3]){#second quartile
      robot <- image_fill(robot, "navajowhite3", point = "+50+130", fuzz = 0)
    }else if(pop <= popQuantile[4]){#wheat
      robot <- image_fill(robot, "lightpink", point = "+50+130", fuzz = 0)
    }else if(pop <= popQuantile[5]){#fourth
      robot <- image_fill(robot, "wheat3", point = "+50+130", fuzz = 0)
    }

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
    
    pop = single_data$Population %>% as.numeric()
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
    
    str2 <- paste("A population of ", pop, " million changes the color of the needle. ")
    str3 <- paste(round(cases, 2), " Covid cases per million changes the color of the mask. ")
    str4 <- paste(round(deaths, 2), " Covid deaths per million changes the color of the cross on the hat. ")
    
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