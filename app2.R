#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#Libraries
library(rsconnect)
library(treemap)
library(shinythemes)
library(shiny)
library(tidyverse)
library(sp)
library(leaflet)    
library(geojsonio)  
library(htmltools)  

# Import Data
mapData <- data.frame(read.csv("mapDat.csv")) %>%
    arrange(Key)


## jSON files
shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
WorldCountry <- geojson_read(shapeurl, what = "sp")
WorldCountry@data <- mapData


# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("flatly"), 
                 "App",
                        # World Panel
                        tabPanel(title = "Map",
                                 
                                 fillPage(
                                     fluidRow(
                                         leafletOutput("myMap")
                                     )),
                                 
                                 absolutePanel( #Options panel
                                     theme = shinytheme("flatly"), 
                                     id = "controls", 
                                     #class = "panel panel-default",
                                     style="padding: 12px; border: 2px solid black; background-color:#eaf3f6; width:25%; font-size:18px",
                                     bottom = 100, 
                                     left = 25, 
                                     fixed=TRUE,
                                     draggable= TRUE, 
                                     height = 400,
                                     radioButtons("radio", label = h3("Options"), #Radio Buttons
                                                  choices = list("Quantity of Robots" = "Total",
                                                                 "GDP per Capita" = "GDP_Per_Capita", 
                                                                 "Population" = "Population",
                                                                 "Covid-19 Deaths per Million" = "Covid_Deaths_Per_Million",
                                                                 "Covid-19 Cases per Million" = "Covid_Cases_Per_Million"), selected = "Total"),
                                     checkboxInput("log", label = "Apply Logarithmic Scale", value = FALSE)),
                                 
                                 fluidRow(
                                     column(8, 
                                            offset = 4,
                                            style = "padding:7.5vh 75px 30px 30px; width:70%;",
                                            wellPanel(
                                                style = "border: 2px solid black; background-color:#eaf3f6; font-size:18px",
                                                p("Welcome to the map explorer. In this tab, you can examine the total number of Covid-19 related robots 
                                                  for each country in the world. Since data on the robots is not very extensive, this map includes other functionality, such 
                                                  as viewing country level Covid-19, demographic, and economic activity indicators; furthermore, hovering a country allows you to
                                                  see their ranking in each metric. ")
                                            ))
                                 ),
                                 fluid = TRUE
                                 
                        )
                     )


# Define server logic required to draw a histogram
server <- function(input, output) {

    
    #Label
    myLabels <- paste("<strong>", WorldCountry@data$Country, "</strong>", "<br/>",
                      "<em> Robot Quantity Ranking</em>: ", WorldCountry@data$robotRank ,"</br>",
                      "<em> Top Robot Category</em>: ", WorldCountry@data$Top_Category, "</br>",
                      "<em> Covid-19 Cases Ranking</em>: ", WorldCountry@data$caseRank, "</br>",
                      "<em> Covid-19 Deaths Ranking</em>: ", WorldCountry@data$deathRank, "</br>",
                      "<em> Population ranking</em>: ", WorldCountry@data$populationRank, "<br/>",
                      "<em> Gross Domestic Product Per Capita Ranking</em>: ", WorldCountry@data$gdpRank ,"</br>")
    
        
    output$myMap <- renderLeaflet({
        
        leaflet(WorldCountry) %>% 
            addTiles(options = providerTileOptions(minZoom = 0.75, maxZoom = 10)) %>% 
            setMaxBounds(180,180,-180,-180)
        
    })
    
    #Reactive elements
    observe({
 
        ## Color and title Management
        if(input$log == FALSE) {
        if(input$radio == "GDP_Per_Capita"){
            pal <- colorNumeric(
                palette = "GnBu",
                domain = WorldCountry@data$GDP_Per_Capita,
                na.color = NA)
            
            palVal <- WorldCountry@data$GDP_Per_Capita
            title_ <- "Gross Domestic Product </br>Per Capita"
            
        } else if(input$radio == "Total"){
            pal <- colorNumeric(
                palette = "GnBu",
                domain = WorldCountry@data$Total,
                na.color = NA)
            
            palVal <- WorldCountry@data$Total
            title_ <- "Quantity of Robots"
            
        } 
        
        else if(input$radio == "Population"){
            pal <- colorNumeric(
                palette = "GnBu",
                domain = WorldCountry@data$Population,
                na.color = NA)
            
            palVal <- WorldCountry@data$Population
            title_ <- "Population (millions)"
            
        } 
        
        else if(input$radio == "Covid_Deaths_Per_Million"){
            pal <- colorNumeric(
                palette = "GnBu",
                domain = WorldCountry@data$Covid_Deaths_Per_Million,
                na.color = NA)
            
            palVal <- WorldCountry@data$Covid_Deaths_Per_Million
            title_ <- "Covid-19 Deaths </br>Per Million"
            
        } 
        else if(input$radio == "Covid_Cases_Per_Million"){
            pal <- colorNumeric(
                palette = "GnBu",
                domain = WorldCountry@data$Covid_Cases_Per_Million,
                na.color = NA)
            
            palVal <- WorldCountry@data$Covid_Cases_Per_Million
            title_ <- "Covid-19 Cases </br>Per Million"
            
        } 
        }
        else {
            
            if(input$radio == "GDP_Per_Capita"){
                pal <- colorNumeric(
                    palette = "GnBu",
                    domain = log(WorldCountry@data$GDP_Per_Capita),
                    na.color = NA)
                
                palVal <- log(WorldCountry@data$GDP_Per_Capita)
                title_ <- "Log Gross Domestic Product </br>Per Capita"
                
            } else if(input$radio == "Total"){
                pal <- colorNumeric(
                    palette = "GnBu",
                    domain = WorldCountry@data$Total,
                    na.color = NA)
                
                palVal <- WorldCountry@data$Total
                title_ <- "Quantity of Robots"
                
            } 
            
            else if(input$radio == "Population"){
                pal <- colorNumeric(
                    palette = "GnBu",
                    domain = log(WorldCountry@data$Population),
                    na.color = NA)
                
                palVal <- log(WorldCountry@data$Population)
                title_ <- "Log Population (millions)"
                
            } 
            
            else if(input$radio == "Covid_Deaths_Per_Million"){
                pal <- colorNumeric(
                    palette = "GnBu",
                    domain = log(WorldCountry@data$Covid_Deaths_Per_Million),
                    na.color = NA)
                
                palVal <- log(WorldCountry@data$Covid_Deaths_Per_Million)
                title_ <- "Log Covid-19 Deaths </br>Per Million"
                
            } 
            else if(input$radio == "Covid_Cases_Per_Million"){
                pal <- colorNumeric(
                    palette = "GnBu",
                    domain = log(WorldCountry@data$Covid_Cases_Per_Million),
                    na.color = NA)
                
                palVal <- log(WorldCountry@data$Covid_Cases_Per_Million)
                title_ <- "Log Covid-19 Cases </br>Per Million"
                
            } 
        }
        
        
        ## Draws polygons, labels, and colors the map
        leafletProxy("myMap", data = WorldCountry) %>% addPolygons(
            data = WorldCountry,
            fillColor = ~pal(palVal),
            weight = 2,
            opacity = 1,
            color = "white",
            fillOpacity = 0.7,
            highlight = highlightOptions(
                weight = 3,
                color = "grey",
                fillOpacity = 0.7,
                bringToFront = TRUE),
            label = lapply(myLabels, HTML)) %>%
            clearControls() %>% 
            addLegend("bottomright", pal = pal,
                      values = ~palVal,
                      title = title_,
                      labFormat = labelFormat(prefix = " "),
                      opacity = 1)
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
