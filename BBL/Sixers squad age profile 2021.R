#packages
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggforce)
library(Cairo)
library(magick)
library(scales)

#mutate data
data <- Sixers2021%>%
  mutate(dob = dmy(dob),
    reference_date = dmy(reference_date),
    age = (reference_date - dob) / 365)

#plotting
ggplot(data, aes(x = age, y = Games, color = "white"))+
  geom_point(color = "hot pink", size = 6, alpha = 0.9)+
  geom_text_repel(aes(label = Player), 
                  size = 4.5, 
                  colour = "white", family = "Fira Sans Light")+
  labs(x = "Age",y = "Games Played", title = "<span style ='color:hotpink'>Sydney Sixers</span> | Age Profile",
       subtitle = "2020/21 BBL", caption = "Michael Najdan")+
  scale_y_continuous(breaks = c(2,4,6,8,10,12,14,16))+
  scale_x_continuous(breaks = c(18,22,26,30,34,38))+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#141622"),
        panel.background = element_rect(fill = "#141622", 
                                        colour = "#141622",
                                        size = 2, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, 
                                        linetype = 'dashed',
                                        colour = "gray20"),
        axis.title.x = element_text(size = 13, 
                                    colour = "white", 
                                    family = "Fira Sans",
                                    face = "bold"),
        axis.title.y = element_text(size = 13, 
                                    colour = "white", 
                                    family = "Fira Sans",
                                    face = "bold"),
        axis.text.x = element_text(colour = "white", family = "Fira Sans"),
        axis.text.y = element_text(colour = "white", family = "Fira Sans"),
        plot.title = element_markdown(face = "bold", 
                                  colour = "white", 
                                  size = 20, 
                                  family = "Fira Sans"),
        plot.subtitle = element_text(colour = "white", 
                                     family = "Fira Sans" , 
                                     size = 10),
        plot.caption = element_text(colour = "gray70", 
                                    family = "Fira Sans Light" , 
                                    size = 8))+
  annotate("rect", 
           xmin = 26,
           xmax = 30,
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.25,
           fill = "mediumseagreen")
