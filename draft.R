library(fmsb)
library(tidyverse)

robot_data <- read_excel("data/Round2ProjectRobotData.xlsx") %>%
  filter(!is.na(CATEGORY))

head(robot_data)

temp_data <- robot_data %>%
  group_by(LOCATION) %>% 
  filter(CATEGORY == "Public Safety") %>%
  count(SUBCATEGORY) %>% 
  arrange(desc(n))%>%
  pivot_wider(names_from = SUBCATEGORY, values_from = n) 

rownames(temp_data) <- ()

temp_data <- rbind(rep(temp_data[[1]],5) , rep(0,5), temp_data)


radarchart(temp_data,
           axistype=1 , 
            #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)

