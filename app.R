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

# Data
robot_data <- read_excel("data/Round2ProjectRobotData.xlsx", sheet = 3) %>% 
    filter(!is.na(CATEGORY))

image_data <- read_excel("data/Round2ProjectRobotData.xlsx", sheet = 4)


# Define UI for application that draws a histogram
ui <- navbarPage(
    ## Theme
    theme = shinytheme("flatly"),
    ## App Title
    "App",
    
    ## Map Panel
    tabPanel(
        "Map", # Title
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
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
