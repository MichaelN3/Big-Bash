# Create data
Player <- c(rep("Marcus Stoinis" , 2) , rep("Glen Maxwell" , 2),
            rep("Ben Dunk", 2), rep("Peter Handscomb",2), rep("Nick Larkin", 2), 
            rep("Nic Maddinson", 2), rep("Seb Gotch", 2), rep("Hilton Cartwright", 2))
Average <- rep(c("Average vs Pace" , "Average vs Spin") , 8)
StrikeRate <- rep(c("Strike Rate vs Pace", "Strike Rate vs Spin"), 8)
AverageValues<- c(55.4,48.3,
                  33.4,46.6,
                  18,15.2,
                  20.9,30.5,
                  38.9,29,
                  12.2,8.7,
                  11,28.5,
                  30.6, 77)
StrikeRateValues <- c(131.8,141.2,
                      161.4,125.4,
                      112.9,121.9,
                      121,122,
                      138.8,111.5,
                      90.5,97.8,
                      108.2,97.4,
                      125.4,128.3)
data <- data.frame(Player,Average,StrikeRate, StrikeRateValues, AverageValues)
View(data)

# Plot
p <- ggplot(data, aes(x=Player, y=StrikeRateValues)) +
  geom_segment(aes(x=Player, xend=Player, y=0, yend=StrikeRateValues),
               color = ifelse(data$AverageValues>=40, "springgreen4", "gray20"),
               size =1) +
  geom_point(color = ifelse(data$AverageValues>=40, "springgreen4", "gray20"),
             size = 15.2) +
  coord_flip() +
  geom_text(aes(label = AverageValues),
            size = 4.2, hjust = 0.5, family = "Fira Sans", color = "white")+
  labs(title = "Average and Strike Rate vs Pace and Spin | Melbourne Stars", x = "Player", y = "Average",
       subtitle = "<span style = 'color:springgreen4'>Average Over 40", 
       caption = "BBL 2017-2020\nMichael Najdan")+
  theme(legend.position="none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
    panel.background=element_rect(fill = "#141622"),
    plot.background=element_rect(fill = "#141622"),
    axis.text.x=element_text(color = "white", family = "Fira Sans"),
    axis.text.y=element_text(color = "white",
                             size = 11, family = "Fira Sans"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title= element_text(color = "white",
                             family = "Fira Sans",
                             size = 18,
                             face = "bold"),
    plot.subtitle=element_markdown(color = "white", size = 11,
                               family = "Fira Sans"),
    plot.caption = element_text(color = "gray78", family = "Fira Sans"))
    
p

p + facet_wrap(.~StrikeRate)+
  theme(strip.text.x=element_text(size = 13, family = "Fira Sans",
        color = "white", face = "bold"),
        strip.background=element_rect(fill = "#141622"))

#add image
img<- png::readPNG("Desktop/Stars.png")
g_pic<- rasterGrob(img, interpolate=TRUE)
