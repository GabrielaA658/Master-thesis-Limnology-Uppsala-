#bacterial Abundance M1&2
library(vegan)
library(ggpubr)
library(tidyverse)
#m1####
data <- read.csv("D:/UPPSALA/Master_thesis/Results/PC_countings/Data_MI/meta_data_flow_cy.csv")
bact_1 <- data[,1:8]
bact_2 <- data[,9:16]
bact_1 <- bact_1[1:27,]
bact_2 <- bact_2[1:27,]

bact_1_nD <- bact_1 %>% 
  filter(Treatment!="Di")
bact_2_nD <- bact_2 %>% 
  filter(Treatment.1!="Di")


#combined start, end - second disturbance
M1<-gather(bact_1_nD,
           time,
           value,
           MI1_s0:MI1_s4)
M1$Origin<-factor(M1$Origin,levels = c("C","D","E"))
series1<-ggline(M1,
       x="time",
       y="value",
       color="Origin",
       add=c("mean_sd","jitter"),
       legend.title="Origin",
       title="Time series")+
  scale_color_manual(values=c("#333333","#99CC99","#FF6666"))+
  border()+
  ylab("")+
  xlab("Experimental day")+
  scale_x_discrete(labels=c(0:4))

ggplot(M1,
       aes(x=time,
       y=value,fill=Origin,shape=Treatment)
       )+
  scale_fill_manual(values = c("#333333","#66CC99","#FF6666"))+
  geom_point(aes(shape=Treatment),size=4)+
  scale_shape_manual(values = c(21,24))+
  scale_x_discrete(labels=c(0:4))+
  guides(fill = guide_legend(override.aes = list(shape=21)))+ 
  xlab("Experimental day")+
  ylab("Cell / ml")

new.plot <- ggscatter(M1,
                      x = "time",
                      y = "value",
                      fill = "Origin", 
                      color = "black",
                      shape = "Treatment", 
                      size = 4,
                      position = position_dodge(0.3))+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
  scale_x_discrete(labels=c(0,2,4,6,8))+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  border()+ 
  xlab("Experimental day")+
  ylab("Cells/ml")

annotate_figure(new.plot,
                top=text_grob("Microcosm 1 - Bacterial abundance",face = "bold"))

bact_1_nD$Origin<-factor(bact_1_nD$Origin,levels = c("C","D","E"))

bacteria_m1<-ggarrange(new.plot,
                       ggarrange(
                         ggscatter(bact_1_nD,
                                   x="Origin",
                                   y="MI1_s0",
                                   shape="Treatment",
                                   color ="black", 
                                   fill="Origin",
                                   palette = c("#666666","#99FF99","#FF6666"),
                                   main="Day 0",
                                   ylab = "",
                                   size=4,
                                   ylim=c(100000,1600000),
                                   position=position_dodge(0.5))+
  border()+
    scale_shape_manual(values = c(21, 24))+
  theme(legend.position = "none")+
    guides(fill = guide_legend(override.aes = list(shape=21))),
                        ggscatter(bact_1_nD,
          x="Origin",
          y="MI1_s4",
          shape = "Treatment",
          color ="black",
          fill="Origin" ,
          palette = c("#666666","#99FF99","#FF6666"),
          main="Day 8",
          ylab = "",
          ylim=c(100000,1600000),
          size = 4,
          position=position_dodge(0.5))+
  border()+
    scale_shape_manual(values = c(21, 24))+
  theme(legend.position = "none")+  
    guides(fill = guide_legend(override.aes = list(shape=21))),
legend = "none",labels = c("B","C")),
nrow = 2,labels = "A",common.legend = T)

annotate_figure(bacteria_m1,
                top = text_grob("Microcosm 1 - Bacterial abundance",face = "bold"),
                left = "Cells / ml")

bacteria_m1_boxplot<-ggarrange(new.plot,
ggarrange(
  ggboxplot(bact_1_nD,
            x="Origin",
            y="MI1_s0",
            shape="Treatment",
            add = c("mean","jitter"),
            color ="Treatment" ,
            palette = c("#FF6666","#666666"),
            main="Day 0",
            ylab = "",
            ylim=c(100000,1600000),
            fill = c("#666666","#99FF99","#99FF99",
                     "#666666","#FF6666","#FF6666"))+
    border()+
    theme(legend.position = "none"),
  ggboxplot(bact_1_nD,
            x="Origin",
            y="MI1_s4",
            add = c("mean","jitter"),
            color ="Treatment" ,
            palette = c("#FF6666","#666666"),
            main="Day 8",
            ylab = "",
            ylim=c(100000,1600000),
            fill = c("#99FF99","#FF6666","#666666",
                     "#99FF99","#FF6666","#666666"))+
    border()+
    theme(legend.position = "none"),
  common.legend = T,labels = c("B","C")),
nrow = 2,labels = "A")

#m2####
M2<-gather(bact_2_nD,
           time,
           value,
           MI2_s0:MI2_s4)
bact_2_nD$Origin.1<-factor(bact_2_nD$Origin.1,levels = c("C","D","E"))
names(M2)<-c("Treatment","Replicate","Origin","time","value")
series2<-ggline(M2,
                x="time",
                y="value",
                color="Origin",
                add=c("mean_sd","jitter"),
                legend.title="Origin",
                title="Time series")+
  scale_color_manual(values=c("#333333","#99CC99","#FF6666"))+
  border()+
  ylab("")+
  xlab("Experimental day")+
  scale_x_discrete(labels=c(0:4))

new.plot2 <- ggscatter(M2,
                      x = "time",
                      y = "value",
                      fill = "Origin", 
                      color = "black",
                      shape = "Treatment", 
                      size = 4,
                      position = position_dodge(0.3))+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
  scale_x_discrete(labels=c(0,2,4,6,8))+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  border()+ 
  xlab("Experimental day")+
  ylab("Cells/ml")

annotate_figure(new.plot2,
                top = text_grob("Microcosm 2 - Bacterial abundance",face = "bold"))

bacteria_m2_boxplot<-ggarrange(
  new.plot2,
  ggarrange(ggboxplot(bact_2_nD,
                      x="Origin.1",
                      y="MI2_s0",
                      add = c("mean","jitter"),
                      color ="Treatment.1" ,
                      palette = c("#FF6666","#666666"),
                      main="Day 0",
                      ylab = "",
                      ylim=c(250000,1200000),
                      xlab = "Origin",
                      fill = c("#666666","#99FF99","#FF6666",
                               "#666666","#99FF99","#FF6666")
  )+
    border()+
    theme(legend.position = "top")+
    labs(color="Treatment"),ggboxplot(bact_2_nD,
                                      x="Origin.1",
                                      y="MI2_s4",
                                      add = c("mean","jitter"),
                                      color ="Treatment.1" ,
                                      palette = c("#FF6666","#666666"),
                                      main="Day 8",
                                      ylab = "",
                                      ylim=c(250000,1200000),
                                      xlab = "Origin",
                                      fill = c("#666666","#99FF99","#FF6666",
                                               "#666666","#99FF99","#FF6666")
    )+
    border()+
    theme(legend.position = "top")+
    labs(color="Treatment"),common.legend = T,labels = c("B","C")),
  nrow = 2,labels = "A")
names(bact_2_nD)<-c("Treatment","Replicate","Origin","MI2_s0","MI2_s1","MI2_s2",
                    "MI2_s3","MI2_s4")
bacteria_m2<-ggarrange(
  new.plot2,
  ggarrange(ggscatter(bact_2_nD,
                    x="Origin",
                    y="MI2_s0",
                    shape = "Treatment",
                    color ="black",
                    fill="Origin" ,
                    palette = c("#666666","#99FF99","#FF6666"),
                    main="Day 0",
                    ylab = "",
                    ylim=c(250000,1200000),
                    xlab = "Origin",
                    size = 4,
                    position=position_dodge(0.5))+
  border()+
  scale_shape_manual(values = c(21, 24))+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  theme(legend.position = "top"),
  ggscatter(bact_2_nD,
          x="Origin",
          y="MI2_s4",
          color ="black",
          shape = "Treatment",
            fill="Origin" ,
          palette = c("#666666","#99FF99","#FF6666"),
          main="Day 8",
          ylab = "",
          ylim=c(250000,1200000),
          xlab = "Origin",
          size = 4,
          position=position_dodge(0.5)
)+
  border()+
  scale_shape_manual(values = c(21, 24))+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  theme(legend.position = "top"),legend = "none",labels = c("B","C")),
nrow = 2,labels = "A",common.legend = T)
annotate_figure(bacteria_m2,
                top = text_grob("Microcosm 2 - Bacterial abundance",face = "bold"),
                left = "Cell / ml")

#model####
qqnorm(bact_1_nD$MI1_s4)
qqline(bact_1_nD$MI1_s4)
m1 <- lm(bact_1_nD$MI1_s4~bact_1_nD$Origin*bact_1_nD$Treatment)
summary(m1)
plot(m1$residuals)
summary(aov(m1))
TukeyHSD(aov(m1))

qqnorm(bact_2_nD$MI2_s4)
qqline(bact_2_nD$MI2_s4) #not normally!
m2 <- lm(bact_2_nD$MI2_s4~bact_2_nD$Origin*bact_2_nD$Treatment)
summary(aov(m2))
plot(m2$residuals)
TukeyHSD(aov(m2))

kruskal.test(bact_2_nD$MI2_s4~bact_2_nD$Treatment)

#levene test####
library(car)
leveneTest(bact_1_nD$MI1_s4~bact_1_nD$Origin*bact_1_nD$Treatment)
leveneTest(MI1_s4~interaction(Origin,Treatment),
           data = bact_1_nD)
leveneTest(bact_1_nD$MI1_s4~bact_1_nD$Origin)
leveneTest(bact_1_nD$MI1_s4~bact_1_nD$Treatment)           
#m2
leveneTest(bact_2_nD$MI2_s4~bact_2_nD$Origin*bact_2_nD$Treatment)
leveneTest(bact_2_nD$MI2_s4~bact_2_nD$Origin)
leveneTest(bact_2_nD$MI2_s4~bact_2_nD$Treatment)

#correlation with CDOM####
#M1####
shapiro.test(bact_1_nD$MI1_s4)
shapiro.test(CDOM1_nD$S4)
cor.test(CDOM1_nD$S4,bact_1_nD$MI1_s4,method = "kendall")
#M2####
shapiro.test(bact_2_nD$MI2_s4)
shapiro.test(CDOM2_nD$S4)
cor.test(CDOM2_nD$S4,bact_2_nD$MI2_s4,method = "kendall")
