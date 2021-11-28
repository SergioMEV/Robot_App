library(readxl)
library(dplyr)
library(waffle)
library(RColorBrewer)

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


temp_data1 <- robot_data %>% 
  count(LOCATION) %>% 
  arrange(desc(n))


brewer.pal(n = 12, name = "Paired")

pal <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
         "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")