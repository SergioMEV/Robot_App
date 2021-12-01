library(fmsb)
library(readxl)
library(tidyverse)

robot_data <- read_excel("data/Round2ProjectRobotData.xlsx", sheet = 4) %>%
  filter(!is.na(CATEGORY))

temp_data <- robot_data %>%  
  filter(CATEGORY == "Clinical Health", !is.na(SUBCATEGORY)) %>%
  group_by(SUBCATEGORY) %>% 
  summarise(n = n()) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  arrange(percent, desc())

temp_data2 <- robot_data %>% 
  select(DESCRIPTION)


library(htmlwidgets)
library(slickR)

if(interactive()){
  slickR(obj=nba_team_logo$uri)
  # synching 3 groups
  # creating groups
  sx1 <- as.numeric(grepl('C',nba_team_logo$uri,ignore.case = FALSE))
  sx2 <- as.numeric(grepl('D',nba_team_logo$uri,ignore.case = FALSE))*2
  sx3 <- sx1 + sx2
  # split into list of size 3
  sIdx <- lapply(split(nba_team_logo$uri,sx3),function(x) match(x,nba_team_logo$uri))
  # synching logic (a,b) and (a,c)
  groups <- expand.grid(list('a',c('b','c')),stringsAsFactors = FALSE)
  slickR(obj = nba_team_logo$uri,
         slideId = c('a','b','c'),
         slideType = rep('img',3),
         synchSlides = groups,
         height = 100)
}






