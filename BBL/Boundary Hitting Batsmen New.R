#mutating
BBL_Batting_since_2017<-BBL_Batting_since_2017%>%
  filter(BallFace>=100)%>%
  mutate(RunsfromSixes=`6's`*6)%>%
  mutate(SixPC=RunsfromSixes/RunScr*100)%>%
  mutate(RunsfromFours = `4's`*4)%>%
  mutate(FourPC = RunsfromFours/RunScr*100)%>%
  mutate(BoundaryPC=FourPC+SixPC)

#plot
ggplot(BBL_Batting_since_2017, aes(FourPC, SixPC))+geom_point(data = BBL_Batting_since_2017%>%filter(FourPC>=40), color ="#25ABF8", size = 4)+
  geom_point(data = BBL_Batting_since_2017%>%filter(SixPC>=30), color = "red", size = 4)+
  geom_point(data = BBL_Batting_since_2017%>%filter(SixPC<30&FourPC<40), color = "white", size = 3, alpha = 0.4)+
  geom_text_repel(data = BBL_Batting_since_2017 %>%
                    filter(FourPC>= 40|SixPC>=30, 
                           !player %in% c("Tom Banton", "Max Bryant", "Philip Salt")),
                  aes(label = player, family = "Lato",fontface = "bold"), 
                  seed = 15, size = 3, segment.color = "#25ABF8",
                  point.padding = 0.1,color = "white",
                  nudge_x = 0.4, nudge_y = 0.4) +
  labs(x = "% of Runs from 4's", y = "% of Runs from 6's", title = "Big Bash Boundary-hitting Batsmen",
       subtitle = "Since 2017",
       caption = "Minimum 100 Balls Faced\nMichael Najdan")+
  theme(legend.position  = "none", 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#141622"),
        panel.background = element_rect(fill = "#141622", 
                                        colour = "#141622",
                                        size = 2, 
                                        linetype = "solid"),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 11, 
                                    face = "bold", 
                                    colour = "white", 
                                    family = "Lato"),
        axis.title.y = element_text(size = 11, 
                                    face = "bold", 
                                    colour = "white", 
                                    family = "Lato"),
        axis.text.x = element_text(colour = "white",
                                   family = "Lato"),
        axis.text.y = element_text(colour = "white",
                                   family = "Lato"),
        plot.title = element_text(face = "bold", 
                                  colour = "white", 
                                  size = 14, 
                                  family = "Lato"),
        plot.caption=element_text(color = "gray78",
                                  family = "Lato",
                                  size = 7),
        plot.subtitle=element_text(family = "Lato",
                                   size = 10,
                                   color = "white"))+
  geom_hline(yintercept = 30,
             linetype = "dotted",
             color = "gray80")+
  geom_vline(xintercept = 40,
             linetype = "dotted",
             color = "gray80")+
  geom_mark_circle(aes(filter=player=="Max Bryant",
                       label = "Max Bryant",
                       description = "2nd Highest Boundary %\n with over 50% of runs\ncoming from 4's"),
                   label.fontsize = c(12,9),
                   color = "white",
                   label.family = "Lato",
                   label.colour = "white",
                   con.colour = "white",
                   label.fill = "#141622",
                   label.buffer = unit(5, "mm"),
                   label.fontface = c("bold", "plain"),
                   con.type = "straight")+
  geom_mark_circle(aes(filter=player =="Tom Banton",
                       label = "Tom Banton",
                       description = "Highest overall\nBoundary % with\n43% of runs\ncoming from 6's"),
  label.fontsize = c(12,9),
  color = "white",
  label.family = "Lato",
  label.colour = "white",
  con.colour = "white",
  label.buffer = unit(15,"mm"),
  label.fill = "#141622",
  label.fontface = c("bold","plain"),
  con.type = "straight")+
  geom_mark_circle(aes(filter=player =="Philip Salt",
                       label = "Phil Salt",
                       description = "50% of runs have\ncome from 4's\nand 20% of runs\nfrom 6's"),
                   label.fontsize = c(12,9),
                   color = "white",
                   label.family = "Lato",
                   label.colour = "white",
                   con.colour = "white",
                   label.buffer = unit(1,"mm"),
                   label.fill = "#141622",
                   label.fontface = c("bold","plain"),
                   con.type = "straight")
