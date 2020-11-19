#add data
Data<- read.csv('IPL PP Bowlers 2020.csv')

#mutate and filter
Data<-Data%>%
  mutate(BallsperWicket = BallBowled/Wickets)%>%
  mutate(ER = RunConc/BallBowled*6)%>%
  filter(BallBowled>=24)%>%
  filter(Wickets>=1)

#plot a
a<- ggplot(Data, aes(ER, BallsperWicket))+
  geom_point(data = Data%>%filter(team=="Mumbai Indians"), color = "royalblue4", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Rajasthan Royals"), color = "hot pink", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Kolkata Knight Riders"), color = "darkorchid4", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Chennai Super Kings"), color = "yellow1", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Kings XI Punjab"), color = "red", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Sunrisers Hyderabad"), color = "dark orange", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Delhi Capitals"), color = "navyblue", size = 3.2)+
  geom_point(data = Data%>%filter(team=="Royal Challengers Bangalore"), color = "gold4", size = 3.2)+
  geom_text_repel(data = Data, aes(label = player), color = "white", family = "Fira Sans", size = 2.8)+
  labs(x = "Economy Rate", y = "Balls per Wicket", title = "2020 IPL Powerplay Bowling",
       caption = "Minimum 24 deliveries and 1 wicket")+
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
  annotate("label", x = 4.9, y = 10, label = "Low Economy Rate, Low Strike Rate",
           family = "Fira Sans", size = 3, fill = "gray78")+
  annotate("label", x = 11, y = 10, label = "High Economy Rate, Low Strike Rate",
           family = "Fira Sans", size = 3, fill = "gray78")+
  annotate("label",x = 4.9, y = 66, label = "Low Economy Rate, High Strike Rate",
           family = "Fira Sans", size = 3, fill = "gray78")+
  annotate("label", x = 7.663021, y = 55, label = "Average\nEconomy Rate",
           family = "Fira Sans", size = 3, fill = "#141622", color = "white")+
  annotate("label", x = 4.7, y = 29.27044, label = "Average Balls per Wicket",
           family = "Fira Sans", size = 3, fill = "#141622", color = "white")


#add data 2
Data2<- read.csv('IPL Team PP Bowling 2020.csv')

b<-ggplot(Data2, aes(x =reorder (team,Econ),y = Econ, fill = team))+
  geom_bar(stat = "identity",
           position = position_dodge())+
  scale_fill_manual(values = c("yellow1",
                               "navyblue", 
                               "red",
                               "darkorchid4",
                               "royal blue4",
                               "hot pink",
                               "gold4",
                               "dark orange"))+
  coord_flip()+
  geom_text(aes(label = AVG), color = "white", hjust = 1.4,
            family = "Fira Sans", size = 3.5)+ 
  labs(y = "Economy Rate")+
  theme(panel.background = element_rect(fill = "#141622",
                                        color = "#141622"),
        plot.background = element_rect(fill = "#141622"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x =element_text(color = "white", family = "Fira Sans", size = 8),
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
a + inset_element(b, left = 0.62, right = 0.98, 
                  bottom = 0.57, top = 0.98,
                  align_to = "full") 
