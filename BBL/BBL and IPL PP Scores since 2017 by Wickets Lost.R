#importing data for BBL
Data <-read.csv('BBL PP Scores since 2017.csv')

#plot BBL
a<- ggplot(data = Data, aes(Outs, RunScr))+
  geom_jitter(data = Data%>%filter(W== 1 & BallFace>=36), color = "#25ABF8", size = 3, width = 0.2, alpha = 0.5)+
  geom_jitter(data = Data%>%filter(W== 0 & BallFace>=36), color = "gray70", size = 2, alpha = 0.3, width = 0.2)+
  geom_smooth(data = Data%>%filter(W==1 & BallFace>=36), alpha = 0.1, aes(color = "A"), method = "lm", linetype = "dotted", size = 0.7, se = FALSE)+
  geom_smooth(data = Data%>%filter(W==0 & BallFace>=36), alpha = 0.1, aes(color = "B"), method = "lm", linetype = "dotted", size = 0.7, se = FALSE)+
  scale_color_manual(values = c("blue", "yellow"))+
  labs(title = "Powerplay Scores for <span style='color:#25ABF8'>Winning</span> and <span style ='color:gray70'>Losing</span> Teams by Wickets Lost with trend lines for <span style='color:blue'>Winning</span> and <span style='color:yellow'>Losing</span> Teams", x = "Season", y = "Score")+
  theme(panel.background=element_rect("#141622"),
        plot.background=element_rect("#141622"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        plot.title=element_markdown(family = "Fira Sans", face = "bold",
                                size = 14, color = "white", hjust = 0.45),
        plot.subtitle= element_markdown(family = "Fira Sans",
                                        size = 12,
                                        color="white"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(family = "Fira Sans",
                                 size = 9, color = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key=element_rect(color = "#141622", fill = "#141622"),
        legend.title=element_text(family = "Roboto Condensed",
                                  face = "bold", color = "white", size = 10),
        legend.text=element_text(family = "Roboto Condensed", size = 9, color = "white"),
        legend.background=element_rect(fill = "#141622"),
        legend.position = "FALSE")+
  scale_x_continuous(breaks = c(0, 1,2,3,4,5))

#importing data for IPL
Data2 <-read.csv('IPL PP Scores since 2017.csv')

#plot BBL
b<- ggplot(data = Data2, aes(Outs, RunScr))+
  geom_jitter(data = Data2%>%filter(W== 1 & BallFace>=36), color = "#25ABF8", size = 3, width = 0.2, alpha = 0.5)+
  geom_jitter(data = Data2%>%filter(W== 0 & BallFace>=36), color = "gray70", size = 2, alpha = 0.3, width = 0.2)+
  geom_smooth(data = Data2%>%filter(W==1 & BallFace>=36), alpha = 0.1, aes(color = "A"), method = "lm", linetype = "dotted", size = 0.7, se = FALSE)+
  geom_smooth(data = Data2%>%filter(W==0 & BallFace>=36), alpha = 0.1, aes(color = "B"), method = "lm", linetype = "dotted", size = 0.7, se = FALSE)+
  scale_color_manual(values = c("blue", "yellow"))+
  labs(x = "Season", y = "Score")+
  theme(panel.background=element_rect("#141622"),
        plot.background=element_rect("#141622"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        plot.title=element_markdown(family = "Fira Sans", face = "bold",
                                    size = 16, color = "white"),
        plot.subtitle= element_markdown(family = "Fira Sans",
                                        size = 12,
                                        color="white"),
        axis.text.x=element_text(family = "Fira Sans",
                                 size = 9, color = "white"),
        axis.text.y=element_text(family = "Fira Sans",
                                 size = 9, color = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key=element_rect(color = "#141622", fill = "#141622"),
        legend.title=element_text(family = "Roboto Condensed",
                                  face = "bold", color = "white", size = 10),
        legend.text=element_text(family = "Roboto Condensed", size = 9, color = "white"),
        legend.background=element_rect(fill = "#141622"),
        legend.position = "FALSE")+
  scale_x_continuous(breaks = c(0,1,2,3,4,5))

#combining plots
a/b
