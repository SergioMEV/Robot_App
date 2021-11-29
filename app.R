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
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(eply)
library(readxl)
library(htmltools)  
library(wordcloud2)
library(waffle)
library(fmsb)

# Data
robot_data <- read_excel("data/Round2ProjectRobotData.xlsx") %>% 
    filter(!is.na(CATEGORY))

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
    
    ## Compare Panel
    navbarMenu(
        "Compare",
        
        ## Robot panel
        tabPanel(
            "Robots", # Title
            ## Layout
            fluidRow(
                ## Country 1 Column
                column(6, ## Column Size 
                       ## Select box
                       selectInput("category1", label = h3("Select Category 1"), 
                                   choices = robot_data$CATEGORY,
                                   selected = 1,
                                   width = '100%'),
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
                ## Country 2 Column
                column(6, ## Column Size
                       
                       ## Select box
                       selectInput("category2", label = h3("Select Category 2"), 
                                   choices = robot_data$CATEGORY, 
                                   selected = 1,
                                   width = '100%'),
                       
                       ## Radio buttons
                       radioGroupButtons(
                           inputId = "chartType2", label = "Choose a graph:", 
                           choices = c("Subcategories" = "Subcategory", 
                                       "Locations" = "Location", 
                                       "Name" = "Name"),
                           justified = TRUE),
                       ## Plot rendering
                       
                       ### Waffle Chart
                       conditionalPanel(
                           condition = "input.chartType2 == 'Subcategory'",
                           plotOutput("waffle2")
                       ),
                       
                       ### Bar Chart
                       conditionalPanel(
                           condition = "input.chartType2 == 'Location'",
                           plotOutput("compBarChart2")
                       ),
                       
                       ### Word Cloud 
                       conditionalPanel(
                           condition = "input.chartType2 == 'Name'",
                           wordcloud2Output("wordCloud2")
                       )
                       
                )
            )
        ), ## End of robot panel
        
        ## Countries Panel
        tabPanel(
            "Countries", # Title
            
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
    
    ## Bar Chart Category 2
    output$compBarChart2 <- renderPlot({
        
        ### Filtering dataset by given category
        cat2 <- input$category2
        
        temp_data <- robot_data %>% 
            filter(CATEGORY == cat2) %>% 
            count(LOCATION) %>% 
            arrange(desc(n))
        
        ### Choosing variable to graph depending on input
        type2 <- temp_data$LOCATION
        
        ### Plotting Graph
        ggplot(temp_data, 
               aes(x = reorder(type2, n), y = n)) +
            geom_col(fill = "darkblue") +
            coord_flip() + 
            labs(title = paste(input$category2, "Robots by Country"),
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
    
    output$wordCloud2 <- renderWordcloud2({
        
        ## Filtering dataset
        cat2 <- input$category2
        
        temp_data <- robot_data %>% 
            filter(CATEGORY == cat2,
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
        
        #pal <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
        #         "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
        
        pal <- c("#bec2cb", "#ffd700", "#e8d8cd", "#3c3b6e", "#ef4135",
                 "#ff7912", "#00923f", "#0055a4", "#ef4135", "#fbbf16")
        
        ### Filtering dataset and creating a percent variable for chart
        cat1 <- input$category1
        
        temp_data <- robot_data %>%  
            filter(CATEGORY == cat1) %>%
            group_by(SUBCATEGORY) %>% 
            summarise(n = n()) %>% 
            mutate(percent = round(n/sum(n)*100)) %>% 
            arrange(percent, desc())
        
        ### Creating vector variable for plotting
        subcat_count <- temp_data$percent
        names(subcat_count) <-  paste(temp_data$SUBCATEGORY, "(", temp_data$n, "robots)")
        
        ### Plotting waffle chart
        waffle(subcat_count, 
               reverse = TRUE,
               color = pal,
               xlab = "1 square = 1% of total Robots",
               title = paste("Division of", cat1, "Robots"))
    })
    
    
    output$waffle2 <- renderPlot({
        
        ## Color Palette
        
        pal <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
                 "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
        
        ## Filtering dataset by category
        cat2 <- input$category2
        
        temp_data <- robot_data %>%  
            filter(CATEGORY == cat2) %>%
            group_by(SUBCATEGORY) %>% 
            summarise(n = n()) %>% 
            mutate(percent = round(n/sum(n)*100)) %>% 
            arrange(percent, desc())
        
        ## Creating vector variable for plotting
        subcat_count <- temp_data$percent
        names(subcat_count) <- paste(temp_data$SUBCATEGORY, "(", temp_data$n, "robots)")
        
        
        ## Plotting waffle chart
        waffle(subcat_count,
               reverse = TRUE,
               color = pal,
               xlab = "1 square = 1% of total Robots",
               title = paste("Division of", cat2, "Robots"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
