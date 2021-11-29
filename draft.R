library(fmsb)
library(readxl)
library(tidyverse)

robot_data <- read_excel("data/Round2ProjectRobotData.xlsx", sheet = 3) %>%
  filter(!is.na(CATEGORY))

temp_data <- robot_data %>%
  group_by(Region) %>%
  filter(Region %in% c("North America", "Asia" )) %>% 
  count(TYPE)

ggplot(temp_data, aes(x = TYPE,
                      y = n,
                      fill = Region)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Robot Type",
       y = "Number of Robots",
       title = paste("Robot Type Distribution in",
                     input$regions[1], "and", 
                     input$regions[2]))
