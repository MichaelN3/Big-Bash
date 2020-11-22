#add data
Data<- read.csv('BBL PP Bowlers.csv')

#mutate and filter
Data<-Data%>%
  mutate(BallsperWicket = BallBowled/Wickets)%>%
  mutate(ER = RunConc/BallBowled*6)%>%
  filter(BallBowled>=60)%>%
  filter(Wickets>=3)

#plot a
a<- ggplot(Data, aes(ER, BallsperWicket))+
  geom_point(data = Data%>%filter(team=="Melbourne Stars"), color = "springgreen4", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Sydney Sixers"), color = "hot pink", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Brisbane Heat"), color = "deepskyblue", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Melbourne Renegades"), color = "firebrick", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Perth Scorchers"), color = "orange", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Adelaide Strikers"), color = "navyblue", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Hobart Hurricanes"), color = "darkorchid4", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Sydney Thunder"), color = "greenyellow", size = 3.2)+
  geom_text_repel(data = Data%>%filter(BallsperWicket<= 30 & ER <= 7.914523), aes(label = player), color = "white", family = "Fira Sans", size = 2.8)+
  geom_text_repel(data = Data%>%filter(ER >= 9 | BallsperWicket>40), aes(label = player), color = "white", family = "Fira Sans", size = 2.8)+
  labs(x = "Economy Rate", y = "Balls per Wicket", title = "BBL Powerplay Bowling - 17/18 and 18/19",
       caption = "Minimum 60 deliveries and 3 wickets")+
  theme(legend.position  = "none", 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#141622"),
        panel.background = element_rect(fill = "#141622", 
                                        colour = "#141622",
                                        size = 2, 
                                        linetype = "solid"),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 10, 
                                    face = "bold", 
                                    colour = "white", 
                                    family = "Fira Sans"),
        axis.title.y = element_text(size = 10, 
                                    face = "bold", 
                                    colour = "white", 
                                    family = "Fira Sans"),
        axis.text.x = element_text(colour = "white",
                                   family = "Fira Sans"),
        axis.text.y = element_text(colour = "white",
                                   family = "Fira Sans"),
        plot.title = element_text(face = "bold", 
                                  colour = "white", 
                                  size = 17, 
                                  family = "Fira Sans"),
        plot.caption = element_text(colour = "gray78",
                                    family = "Fira Sans",
                                    size = 7))+
  geom_vline(xintercept = mean(Data$ER),
             color = "gray78", linetype = "dashed", size = 0.2)+
  geom_hline(yintercept = mean(Data$BallsperWicket),
             color = "gray78", linetype = "dashed", size = 0.2)+
  annotate("label", x = 6.2, y = 10, label = "Low Economy Rate,\nLow Strike Rate",
           family = "Fira Sans", size = 3, fill = "gray78")+
  annotate("label", x = 9.8, y = 10, label = "High Economy Rate,\nLow Strike Rate",
           family = "Fira Sans", size = 3, fill = "gray78")+
  annotate("label", x = 7.914523, y = 50, label = "Average\nEconomy Rate",
           family = "Fira Sans", size = 3, fill = "#141622", color = "white")+
  annotate("label", x = 9.2, y = 21.27752, label = "Average Balls\nper Wicket",
           family = "Fira Sans", size = 3, fill = "#141622", color = "white")

#add data 2
Data2<- read.csv('BBL Team PP Bowling.csv')

b<-ggplot(Data2, aes(x =reorder (team,Econ),y = Econ, fill = team))+
  geom_bar(stat = "identity",
           position = position_dodge())+
  scale_fill_manual(values = c("navyblue",
                               "deepskyblue", 
                               "darkorchid4",
                               "firebrick",
                               "springgreen4",
                               "orange",
                               "hot pink",
                               "greenyellow"))+
  coord_flip()+
  geom_text(aes(label = AVG), color = "white", hjust = 1.4,
            family = "Fira Sans", size = 3.5)+ 
  labs(y = "Economy Rate")+
  theme(panel.background = element_rect(fill = "#141622",
                                        color = "#141622"),
        plot.background = element_rect(fill = "#141622"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x =element_text(color = "white", family = "Fira Sans", size = 8,
                                   face = "bold"),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        axis.text.y = element_text(color = "white",
                                 family = "Fira Sans",
                                 size = 7.5),
        axis.text.x = element_text(color = "white",
                                   family = "Fira Sans",
                                   size = 7.5),
        legend.position="none")

#inserting bar chart into scatter for powerplay bowling
a + inset_element(b, left = 0.05, right = 0.44, 
                  bottom = 0.54, top = 0.94,
                  align_to = "full") 
