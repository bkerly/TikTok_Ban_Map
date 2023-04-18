library(tidyverse)
library(readxl)
library(ggthemes)
library(maps)
library(usmap)


world_data <- read_excel("data.xlsx",sheet = 1)

world_map <- map_data("world") %>%
  left_join(world_data,by=c("region"="Country"))

wm <- ggplot(data=world_map,aes(x=long,y=lat))+
  geom_polygon(aes(group = group,fill=`Gov Only`,
                   color = "black"))+
  theme_void()+
  scale_color_manual(values = "black")+
  theme(legend.position = "none")

ggsave(wm,
       file = "world ban map.png",
       width = 8,
       height = 4,
       units = "in")

state_data <- read_excel("data.xlsx",sheet = 2) %>%
  mutate(State = toupper(State)) %>%
  transmute(state = State,
            values = `Gov Only`)

US_map <- usmap::us_map(regions = "states") %>%
  mutate(State = toupper(full)) %>%
  left_join(state_data,by=c("state"))


USm <- plot_usmap(data = state_data) +
  theme(legend.position = "none")

ggsave(USm,
       file = "US state ban map.png",
       width = 8,
       height = 4,
       units = "in")
