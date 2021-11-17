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
library(shinythemes)
library(readxl)

# Data
robot_data <- read_excel("data/Round2ProjectRobotData.xlsx")

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
    tabPanel(
        "Compare", # Title
        ## Layout
        fluidRow(
            ## Country 1 Column
            column(6, ## Column Size 
                   selectInput("category1", label = h3("Select Category 1"), 
                               choices = robot_data$CATEGORY, 
                               selected = 1),
            ),
            ## Country 2 Column
            column(6, ## Column Size
                   selectInput("category2", label = h3("Select Category 2"), 
                               choices = robot_data$CATEGORY, 
                               selected = 1),
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

    
}

# Run the application 
shinyApp(ui = ui, server = server)
