Data1<- read.csv('BBL Scores since 14:15.csv')

Data1%>%filter(BallFace>=100)%>%ggplot(aes(PwrPlyRuns,RunScr))+
  geom_point(alpha= 0.4, size = 3)+
  stat_smooth(method = "lm",color = "red", fullrange = TRUE, se = FALSE)+
  scale_y_continuous(breaks = c(40,60,80,100,120,140,160,180,200,220,240))+
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90,100))+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(family = "Fira Sans", face = "bold"),
        axis.title.y = element_text(family = "Fira Sans", face = "bold"),
        axis.text.x = element_text(family = "Fira Sans"),
        axis.text.y = element_text(family = "Fira Sans"),
        panel.grid.major = element_line(color = "gray20",
                                        size = 0.1,
                                        linetype = "dashed"),
        plot.title = element_text(family = "Fira Sans",
                                  face = "bold",
                                  size = 14),
        plot.subtitle = element_text(family = "Fira Sans",
                                     size = 10))+
  labs(x = "Powerplay Runs", y = "Final Total",
       title = "Powerplay and Final Scores with line of regression",
       subtitle = "BBL since 2014/15")+
  annotate("label", x = 70, y = 100,
           label = "- Pearson correlation coefficient = 0.545\n- Gradient of regression of Powerplay Runs on Final Total = 1.405",
           family = "Fira Sans")
