library(readxl)
library(dplyr)
library(waffle)


robot_data <- read_excel("data/Round2ProjectRobotData.xlsx")


temp_data <- robot_data %>%  
  filter(CATEGORY == "Public Safety, Public Works, Non-Clinical Public Health") %>%
  group_by(SUBCATEGORY) %>% 
  summarise(n = n()) %>% 
  mutate(percent = round(n/sum(n)*100)) %>% 
  arrange(percent, desc())

subcat_count <- temp_data$percent
names(subcat_count) <- temp_data$SUBCATEGORY


waffle(subcat_count, reverse = TRUE)

