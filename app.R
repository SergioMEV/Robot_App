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
library(eply)
library(readxl)
library(htmltools)  
library(wordcloud2)
library(waffle)
library(fmsb)
library(slickR)
library(rsconnect)
library(sp)
library(leaflet)    
library(geojsonio) 
library(magick)

# Data
country_data <- read_excel("data/mapData.xlsx")
country_data <- filter(country_data, Top_Category != "NA" & GDP_Per_Capita != "NA")

robot_data <- read_excel("data/Round2ProjectRobotData.xlsx", sheet = 3) %>% 
    filter(!is.na(CATEGORY))

mapData <- data.frame(read.csv("data/mapDat.csv")) %>%
    arrange(Key)

image_data <- read_excel("data/Round2ProjectRobotData.xlsx", sheet = 4)


## jSON files
shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
WorldCountry <- geojson_read(shapeurl, what = "sp")
WorldCountry@data <- mapData


# Define UI for application that draws a histogram
ui <- navbarPage(
    ## Theme
    theme = shinytheme("flatly"),
    ## App Title
    "CBE",
    ## Intro tab
    tabPanel(
        "Intro",
    ),
    ## Map Panel
    tabPanel(
        "Map",# Title
        style = "overflow-y: auto",
        # Map output
        fluidRow(
            leafletOutput("myMap")
        ),
        
        absolutePanel( #Options panel
            theme = shinytheme("flatly"), 
            id = "controls", 
            #class = "panel panel-default",
            style="padding: 12px; border: 2px solid black; background-color:#eaf3f6; width:25%; font-size:18px",
            bottom = 100, 
            left = 25, 
            fixed= TRUE,
            draggable= TRUE, 
            height = 360,
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
        )
        
        
        
    ),
    ## Robot panel
    tabPanel(
        "Robots", # Title
        ## Layout
        fluidPage(
            fluidRow(
                wellPanel(
                    style = "width: auto",
                    ## Select box
                    selectInput("category1", label = h3("Select Robot Category"), 
                                choices = robot_data$CATEGORY,
                                selected = 1,
                                width = '100%')
                )
            ),
            fluidRow(
                ## Chart Column
                column(6, ## Column Size
                       ## Radio buttons
                       radioGroupButtons(
                           inputId = "chartType1", label = "Choose a graph:", 
                           choices = c("Subcategories" = "Subcategory", 
                                       "Locations" = "Location", 
                                       "Name" = "Name"),
                           justified = TRUE),
                       ## Image 
                       conditionalPanel(
                           condition = "input.chartType1 == 'Subcategory'",
                           plotOutput("waffle1")
                       ),
                       conditionalPanel(
                           condition = "input.chartType1 == 'Location'",
                           plotOutput("compBarChart1")
                       ),
                       conditionalPanel(
                           condition = "input.chartType1 == 'Name'",
                           wordcloud2Output("wordCloud1")
                       )
                       
                ),
                column(6,
                       wellPanel(
                           style = "height: 440px;",
                           wellPanel(
                               style = "height: 400px; background-color: #fff",
                               h2("What are they?", style = "padding: 10px; text-align: center"),
                               slickROutput("carousel", height = "120px", width = "90%"),
                               
                           )
                       )
                )
            )
        )
    ), 
    
    
    ## Countries Panel
    tabPanel(
        "Regions", # Title
        fluidPage(
            column(
                    8,
                    wellPanel(
                        style = "background-color: #fff; 
                                border-color: #2c3e50; 
                                height: 450px;
                                border-width: 2px",
                        # conditional panels for rendering plots
                        conditionalPanel(
                            condition = "input.regionChart == 'Total'",
                            plotOutput("totalbar")
                        ),
                        conditionalPanel(
                            condition = "input.regionChart == 'Categories'",
                            plotOutput("spider")
                        ),
                        conditionalPanel(
                            condition = "input.regionChart == 'Types'",
                            plotOutput("typebar")
                        )          
                    )
                ),
            column(
                    4,
                    wellPanel(
                        style = "background-color: #fff; 
                                border-color: #2c3e50; 
                                height: 300px;
                                border-width: 2px",
                        # Region Select
                        pickerInput(
                            "regions",
                            label = "Region(s)", 
                            choices = distinct(robot_data, Region),
                            multiple = TRUE,
                            options = list(
                                "max-options" = 2,
                                "max-options-text" = "Select Only 2 Regions"
                            ),
                            selected = c("North America", "Asia")
                            ),
                        # Type of graph
                        radioGroupButtons(
                            "regionChart",
                            label = "Label",
                            choices = c("Total",
                                        "Categories", 
                                        "Types"),
                            selected = "Total",
                            direction = "vertical",
                            justified = TRUE
                        )
                    ),
                    
                    conditionalPanel(
                        condition = "input.regionChart == 'Categories'",
                        wellPanel(
                            style = "background-color: #fff; 
                                     border-color: #2c3e50; 
                                     height: 120px;
                                     border-width: 2px",
                        # Category
                            pickerInput(
                                inputId = "category",
                                label = "Category", 
                                choices = distinct(robot_data, CATEGORY),
                                selected = 1)
                        )
                    ),
                    conditionalPanel(
                        condition = "input.regionChart == 'Types'",
                        wellPanel(
                            style = "background-color: #fff; 
                                     border-color: #2c3e50; 
                                     height: auto;
                                     border-width: 2px;
                                     padding: 10px",
                            p(strong('USVs'), "are boats that operate without a crew."),
                            p(strong('UGVs'), "are ground vehicles that operate without a crew."),
                            p(strong('UAS'), "are air vehicles that operate without a crew.")
                        )
                    )
                )
            )
        ), # End of compare countries tab
    
    ## Robot Panel
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
    ),
    
    ## About Section
    navbarMenu(
        "About", #Title
        
        ## Purpose Panel
        tabPanel(
            "Purpose", # Title
            mainPanel(
                h1("Robots in response to COVID-19"),
                p(" "),
            ),
            
        ),
        
        ## Takeaways Panel
        tabPanel(
          "Takeaways", # Title  
        ),
        
        ## Design Process Panel
        tabPanel(
          "Design Process", # Title  
        ),
        
        ## Reflection Panel
        tabPanel(
            "Reflections", # Title
        ),
        
        ## Acknowledgements
        tabPanel(
            "Acknowledgements", # Title
            mainPanel(
            p("Professor Fernanda Eliott"),
            p("Class Mentor: Alex Leach"),
            )
            
        )
        
    )
)

# Define server logic required to draw a histogram  
server <- function(input, output) {

    # Graphs for Category Comparisons
    
    ## Bar Chart Category 1
    output$compBarChart1 <- renderPlot({
        
        ### Filtering dataset by given category
        cat1 <- input$category1 
        
        temp_data <- robot_data %>% 
            filter(CATEGORY == cat1) %>% 
            count(LOCATION) %>% 
            arrange(desc(n))
        
        ### Choosing variable to graph depending on input
        type1 <- temp_data$LOCATION
        
        ### Plotting Graph
        ggplot(temp_data, 
               aes(x = reorder(type1,n), y = n)) +
            geom_col(fill = "darkblue") +
            coord_flip() + 
            labs(title = paste(input$category1, "Robots by Country"),
                 y = "Number of Robots",
                 x = "Country") +
            theme_classic()
        
    })
    
    
    ## Word Clouds
    
    output$wordCloud1 <- renderWordcloud2({
        
        ## Filtering dataset
        cat1 <- input$category1
        
        temp_data <- robot_data %>% 
            filter(CATEGORY == cat1,
                   !(NAME %in% c("not sure", "unspecified", "Unspecified", "Not sure"))) %>% 
            select(NAME) %>% 
            count(NAME)
        
        ## Creating word cloud
        
        wordcloud2(temp_data, 
                  size = 1, 
                  color = 'random-dark')

    })
    
    
    ## Waffle graphs
    
    output$waffle1 <- renderPlot({
        ## Color Palette
        
        pal <- c("#bec2cb", "#ffd700", "#e8d8cd", "#3c3b6e", "#ef4135",
                 "#ff7912", "#00923f", "#0055a4", "#ef4135", "#fbbf16")
        
        ### Filtering dataset and creating a percent variable for chart
        cat1 <- input$category1
        
        temp_data <- robot_data %>%  
            filter(CATEGORY == cat1, !is.na(SUBCATEGORY)) %>%
            group_by(SUBCATEGORY) %>% 
            summarise(n = n()) %>% 
            mutate(percent = ceiling(n/sum(n)*100)) %>% 
            arrange(desc(percent))
        
        ### Creating vector variable for plotting
        subcat_count <- temp_data$percent
        names(subcat_count) <-  paste(temp_data$SUBCATEGORY, "(", temp_data$n, "robots)")
        
        if(sum(temp_data$percent) > 100){
            subcat_count[1] <- (subcat_count[1] - (sum(temp_data$percent)%%100 ))
        }
        
        ### Plotting waffle chart
        waffle(subcat_count, 
               reverse = TRUE,
               color = pal[1:nrow(temp_data)],
               xlab = "1 square = 1% of total Robots",
               title = paste("Division of", cat1, "Robots"),
               flip = TRUE) 
    })
    
    
    
    # Comparing Countries charts
    
    ## Spider Chart
    
    output$spider <- renderPlot({
        
        # Colors
        
        pal  <- c(rgb(0.2,0.7,0.2,0.2), rgb(0.2,0.2,0.7,0.2))
        
        ## Filtering dataset
        temp_data <- robot_data %>%
            group_by(Region) %>%
            filter(CATEGORY == input$category,
                   Region %in% c(input$regions[1], input$regions[2])) %>%
            count(SUBCATEGORY) %>%
            arrange(desc(n))%>%
            pivot_wider(names_from = SUBCATEGORY, values_from = n)
        
        
        ## Making new dataset for plotting
        
        temp_data2 <- temp_data[2:ncol(temp_data)] 
        
        temp_data2[is.na(temp_data2)] <- 0
        
        rownames(temp_data2) <- temp_data$Region
        
        temp_data2 <- rbind(rep(20,5), rep(0,5), temp_data2)
        
        ## Plotting
        
        radarchart(temp_data2,
                   axistype=1 , 
                   #custom polygon
                   pcol= pal , pfcol= pal , plwd=4 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,50,5), cglwd=0.8,
                   #custom labels
                   vlcex=0.6
        )
        
        # Add a legend
        legend(x=1, y=0.5, 
               legend = rownames(temp_data2)[3:4],
               bty = "n", pch=20 , col= pal , 
               text.col = "grey", cex=1.2, pt.cex=3)
    })
    
    ## type bar chart 
    
    output$typebar <- renderPlot({
        
        ## Filtering dataset
        temp_data <- robot_data %>%
            group_by(Region) %>%
            filter(Region %in% input$regions) %>% 
            count(TYPE)
        
        ## Plotting barchart
        ggplot(temp_data, aes(x = TYPE,
                              y = n,
                              fill = Region)) +
            geom_bar(position="dodge", stat="identity") +
            labs(x = "Robot Type",
                 y = "Number of Robots",
                 title = paste("Robot Type Distribution in",
                               input$regions[1], "and", 
                               input$regions[2])) + 
            theme_classic()
        
    })
    
    
    ## total bar chart 
    
    output$totalbar <- renderPlot({
        
        ## Filtering dataset
        temp_data <- robot_data %>%
            group_by(Region) %>%
            filter(Region %in% input$regions) %>% 
            count(Region)
        
        ## Plotting barchart
        ggplot(temp_data, aes(x = Region,
                              y = n,
                              fill = Region)) +
            geom_col() +
            labs(x = "Region",
                 y = "Number of Robots",
                 title = paste("Number of Robots in",
                               input$regions[1], "and", 
                               input$regions[2])) + 
            coord_flip() +
            theme_classic()
        
    })
    
    # UI
    
    ## Carousel
    output$carousel <- renderSlickR({
        ## Filtering Data
        
        temp_data <- image_data %>% 
            filter(CATEGORY == input$category1)
        
        
        ## Carousel UI
        
        carousel1 <- (slickR(temp_data$SUBCATEGORY, 
                             slideType = 'p') + settings(autoplay = TRUE,
                                                         fade = TRUE, 
                                                         pauseOnHover = TRUE, 
                                                         autoplaySpeed = 6000) ) 
        
        carousel2 <- slickR(obj = temp_data$Path,
                            objLinks = temp_data$Link,
                            height = 100, 
                            width = "95%") + settings(arrow = FALSE, 
                                                      fade = TRUE, 
                                                      adaptiveHeight = TRUE,
                                                      pauseOnHover = TRUE)
        
        carousel3 <- (slickR(temp_data$DESCRIPTION, 
                             slideType = 'p') + settings(arrows = FALSE, 
                                                         fade = TRUE,
                                                         pauseOnHover = TRUE) )
        
        carousel1 %synch% ((carousel2) %synch% (carousel3))
        
    })
    
    
    # Map Functions
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
    
    # CovidBot
    
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

# Run the application 
shinyApp(ui = ui, server = server)
