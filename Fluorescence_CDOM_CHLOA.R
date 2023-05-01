#Fluorescence ####
library(tidyverse)
library(ggpubr)
library(nlme)
# M1####
#CDOM####
data_PB_MI1_DOM<-read.csv("PB_MI1_CDOM.csv")
CDOM1<-na.omit(data_PB_MI1_DOM)

CDOM1_nD <- CDOM1 %>% 
  filter(treat!="Di")
CDOM1_nDp <- gather(CDOM1_nD,
                    timepoint,value,S0:S4)
ggline(CDOM1_nDp,
       x="timepoint",
       y="value",
       color = "Origin",
       add = c("mean_sd","jitter"),
       title = "MI1 - CDOM - Fluorescence",
       legend.title="Treatment")+
  border()+
  scale_color_manual(values = c("#333333","#66CC99","#FF6666"))+
  ylab("RFU")+
  xlab("Experimental day")
ggboxplot(CDOM1_nDp,
          x="timepoint",
          y="value",
          color = "Origin",
          add = c("mean_sd","jitter"),
          title = "Second disturbance - Fluorescence - CDOM",
          legend.title="Origin")+
  border()+
  scale_color_manual(values = c("#333333","#66CC99","#FF6666"))+
  ylab("RFU")+
  xlab("Experimental day")
#details about interaction changing through time
ggboxplot(CDOM1_nDp,
          x="Origin",
          y="value",
          color = "treat",
          add = c("mean_sd","jitter"),
          title = "Second disturbance - Fluorescence - CDOM",
          legend.title="Treatment")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")+
  xlab("Experimental day")
#s0
ggboxplot(CDOM1_nDp[1:27,],
          x="Origin",
          y="value",
          color = "Treatment",
          add = c("mean_sd","jitter"),
          title = "CDOM s0",
          fill = c("#FF6666","#666666","#99FF99","#FF6666","#666666","#99FF99"),
          legend.title="Treatment")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")+
  xlab("")
#s2
ggboxplot(CDOM1_nDp[37:54,],
          x="Origin",
          y="value",
          color = "Treatment",
          add = c("mean_sd","jitter"),
          title = "CDOM s2",
          fill = c("#FF6666","#666666","#99FF99","#FF6666","#666666","#99FF99"),
          legend.title="Treatment")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")+
  xlab("")
#s4
ggboxplot(CDOM1_nDp[73:90,],
          x="Origin",
          y="value",
          color = "Treatment",
          add = c("mean_sd","jitter"),
          title = "CDOM s4",
          fill = c("#FF6666","#666666","#99FF99","#FF6666","#666666","#99FF99"),
          legend.title="Treatment")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")+
  xlab("")
#combined s0,s2,s4####
names(CDOM1_nDp)<-c("cuvette","Treatment","Replicate","Origin","bottle","timepoint","value")
CDOM1_nDp$Origin <- factor(CDOM1_nDp$Origin,levels = c("C","D","E"))
ggscatter(CDOM1_nDp[1:27,],
x="Origin",
y="value",
color = "black",
fill="Origin",
size = 4,
ylim=c(5,45),
position=position_dodge(0.5),
title = "Day 0",
shape = "Treatment")+
  border()+ylab("")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
  scale_shape_manual(values = c(21, 24))+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  xlab("")

combi_CDOM<- ggarrange(
  ggscatter(CDOM1_nDp[37:54,],
            x="Origin",
            y="value",
            color = "black",
            fill="Origin",
            size = 4,
            ylim=c(5,45),
            position=position_dodge(0.5),
            title = "Day 4",
            shape = "Treatment")+
    border()+ylab("")+
    theme(legend.position = "none")+
    scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
    scale_shape_manual(values = c(21, 24))+
    guides(fill = guide_legend(override.aes = list(shape=21)))+
    xlab(""),
  ggscatter(CDOM1_nDp[73:90,],
            x="Origin",
            y="value",
            color = "black",
            fill="Origin",
            ylim=c(5,45),
            size = 4,
            shape = "Treatment",
            position=position_dodge(0.5),
            title = "Day 8")+
    border()+ylab("")+
    theme(legend.position = "none")+
    scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
    scale_shape_manual(values = c(21, 24))+
    guides(fill = guide_legend(override.aes = list(shape=21)))+
    xlab(""),common.legend = T,nrow = 1,labels = c("A","B")
)
annotate_figure(
  combi_CDOM,top = text_grob("Microcosm 1 - CDOM Fluorescence",
                             face ="bold" ),
  bottom = "Origin",left = "RFU")
#timeseries with scatter####
time_cdom_1<-ggscatter(CDOM1_nDp[38:90,],
                       x = "timepoint",
                       y = "value",
                       fill = "Origin", 
                       color = "black",
                       shape = "Treatment", 
                       size = 4,
                       position = position_dodge(0.5))+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
  scale_x_discrete(labels=c(4,6,8))+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  border()+ 
  xlab("Experimental day")+
  ylab("RFU")
c1 <- annotate_figure(time_cdom_1,
                text_grob("CDOM Fluorescence",face = "bold"))

#CHLO A####
data_PB_MI1_CHLOR<-read.csv("PB_MI1_CHLORA.csv")
pb1<-na.omit(data_PB_MI1_CHLOR)
pb1_nD <- pb1 %>% 
  filter(treat!="Di")
pb1_nD_plot <- gather(pb1_nD,
                      timepoint,value,
                      S0:S4)
ggline(pb1_nD_plot,
       x="timepoint",
       y="value",
       add = "mean_sd",
       title.legend="",
       color = "treat",
       title = "Mi1 - Chlo A - Fluorescence")+
  border()+
  scale_color_manual(values = c("brown3","darkslategray"))+
  ylab("RFU")
#timeseries m1 by origin####
Ori_C1 <- ggline(pb1_nD_plot[38:90,],
          x="timepoint",
          y="value",
          add = c("mean_sd","jitter"),
          title.legend="",
          color = "Origin",
          legend.title="Origin",
          xlab="",
          title = "Second Disturbance - Fluorescence - Chlo A")+
  border()+
  scale_color_manual(values = c("#333333","#66CC99","#FF6666"))+
  ylab("RFU")
Ori_E1 <- ggline(pb1_nD_plot[38:90,],
                 x="timepoint",
                 y="value",
                 add = c("mean_sd","jitter"),
                 title.legend="",
                 color = "Origin",
                 legend.title="Origin",
                 xlab="",
                 title = "Second Disturbance - Fluorescence - Chlo A")+
  border()+
  scale_color_manual(values = c("#333333","#66CC99","#FF6666"))+
  ylab("RFU")
Ori_D1 <- ggline(pb1_nD_plot[38:90,],
                 x="timepoint",
                 y="value",
                 add = c("mean_sd","jitter"),
                 title.legend="",
                 color = "Origin",
                 legend.title="Origin",
                 xlab="",
                 title = "Second Disturbance - Fluorescence - Chlo A")+
  border()+
  scale_color_manual(values = c("#333333","#66CC99","#FF6666"))+
  ylab("RFU")

#by treatment grouped####
ggline(pb1_nD_plot[38:90,],
          x="timepoint",
          y="value",
          add = c("mean_sd","jitter"),
          title.legend="",
          color = "treat",
          legend.title="Treatment",
          title = "Mi1 - Chlo A - Fluorescence-treatment")+
  border()+
  scale_color_manual(values = c("brown3","darkslategray"))+
  ylab("RFU")
#timepoint in x axis, split by origin and treatment. Details of interaction in each
#sampling point
#s2
ggboxplot(pb1_nD_plot[37:54,],
          x="Origin",
          y="value",
          add = c("mean_sd","jitter"),
          title.legend="",
          color = "treat",
          legend.title="Treatment",
          xlab="",
          fill = c("#FF6666","#666666","#99FF99","#FF6666","#666666","#99FF99"),
          title = "Second Disturbance - Fluorescence - Chlo A - s2")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")
#s3
ggboxplot(pb1_nD_plot[55:72,],
          x="Origin",
          y="value",
          add = c("mean_sd","jitter"),
          title.legend="",
          color = "treat",
          legend.title="Treatment",
          xlab="",
          fill = c("#FF6666","#666666","#99FF99","#FF6666","#666666","#99FF99"),
          title = "Second Disturbance - Fluorescence - Chlo A - s3")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")
#s4
ggboxplot(pb1_nD_plot[73:90,],
          x="Origin",
          y="value",
          add = c("mean_sd","jitter"),
          title.legend="",
          color = "treat",
          legend.title="Treatment",
          xlab="",
          fill = c("#FF6666","#666666","#99FF99","#FF6666","#666666","#99FF99"),
          title = "Second Disturbance - Fluorescence - Chlo A - s4")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")

#by origin & treat####
ggboxplot(pb1_nD_plot[38:90,],
          x="Origin",
          y="value",
          add = c("mean_sd","jitter"),
          title.legend="",
          color = "treat",
          legend.title="Treatment",
          title = "Mi1 - Chlo A - Fluorescence s2,3,4")+
  border()+
  scale_color_manual(values = c("brown3","darkslategray"))+
  ylab("RFU")
#timeseries with scatter####
time_chlo_1<-ggscatter(pb1_nD_plot[38:90,],
          x = "timepoint",
          y = "value",
          fill = "Origin", 
          color = "black",
          shape = "Treatment", 
          size = 4,
          position = position_dodge(0.5))+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
  scale_x_discrete(labels=c(4,6,8))+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  border()+ 
  xlab("Experimental day")+
  ylab("RFU")
chlo1 <- annotate_figure(time_chlo_1,
                text_grob("In vivo chlorophyll a Fluorescence",face = "bold"))
ggarrange(chlo1,c1,
                labels = c("A","B"),
          common.legend = T)

#combined fluorescence M1####
pb1_nD_plot$Origin <- factor(pb1_nD_plot$Origin,levels = c("C","D","E"))
names(pb1_nD_plot)<-c("cuvette","Treatment","Replicate","Origin","bottle","timepoint","value")
combi_chlora1 <- ggarrange(
  ggscatter(pb1_nD_plot[37:54,],
            x="Origin",
            y="value",
            size = 4,
            color ="black", 
              fill="Origin",
            shape = "Treatment",
            xlab="",
            ylim=c(125,220),
            position=position_dodge(0.5),
            title = "Day 4")+
    border()+
    ylab("")+
    scale_shape_manual(values = c(21, 24))+
    theme(legend.position = "none")+
    scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
    guides(fill = guide_legend(override.aes = list(shape=21))),
  ggscatter(pb1_nD_plot[73:90,],
            x="Origin",
            y="value",
            size = 4,
            color = "black",
            fill = "Origin",
            shape = "Treatment",
            xlab="",
            title = "Day 8",
            ylim=c(125,220),
            position=position_dodge(0.5))+
    border()+
    ylab("")+
    theme(legend.position = "none")+
    scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
    scale_shape_manual(values = c(21, 24))+
    guides(fill = guide_legend(override.aes = list(shape=21))),
  common.legend = T,labels = c("A","B"),nrow = 1
)
annotate_figure(
  combi_chlora1,top = text_grob("Microcosm 1 - in vivo Chlo A Fluorescence",
                             face ="bold" ),
  bottom = "Origin",
  left = "RFU")

#M2####
#CHLO A####
pb_Mi2 <- read.csv("Results/PC_countings/Data_MI/PB_MI2_CHLORA.csv")
pb_Mi2 <- pb_Mi2 %>% 
  filter(cuvette!="i2") %>% 
  filter(cuvette!="i5") %>% 
  filter(cuvette!="I9")

pb_Mi2 <- na.omit(pb_Mi2)
pb_Mi2_nD <- pb_Mi2 %>% 
  filter(Treatment!="Di")
pb_Mi2_nDplot <- gather(pb_Mi2_nD,
                        timepoint,value,
                        S0:S4)
#timeseries m2####
ggboxplot(pb_Mi2_nDplot,
          x="timepoint",
          y="value",
          add = c("mean_sd","jitter"),
          title.legend="",
          color = "Origin",
          legend.title="Origin",
          title = "Mi2 - Chlo A - Fluorescence")+
  border()+
  scale_color_manual(values = c("#333333","#66CC99","#FF6666"))+
  ylab("RFU")
ggline(pb_Mi2_nDplot,
       x="timepoint",
       y="value",
       add = c("mean_sd","jitter"),
       title.legend="",
       color = "Treatment",
       legend.title="Treatment",
       title = "Mi2 - Chlo A - Fluorescence")+
  border()+
  scale_color_manual(values = c("brown3","darkslategray"))+
  ylab("RFU")
#by ori&treat####
#s0
ggboxplot(pb_Mi2_nDplot[1:18,],
          x="Origin",
          y="value",
          add = c("mean_sd","jitter"),
          title.legend="",
          color = "Treatment",
          legend.title="Treatment",
          fill = c("#666666","#99FF99","#FF6666","#666666","#99FF99","#FF6666"),
          title = "Day 0 - Chlo A - Fluorescence")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")
#s2
ggboxplot(pb_Mi2_nDplot[37:54,],
          x="Origin",
          y="value",
          add = c("mean_sd","jitter"),
          title.legend="",
          color = "Treatment",
          legend.title="Treatment",
          fill = c("#666666","#99FF99","#FF6666","#666666","#99FF99","#FF6666"),
          title = "Day 4 - Chlo A - Fluorescence")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")
#s4
ggboxplot(pb_Mi2_nDplot[73:90,],
          x="Origin",
          y="value",
          add = c("mean_sd","jitter"),
          title.legend="",
          color = "Treatment",
          legend.title="Treatment",
          fill = c("#666666","#99FF99","#FF6666","#666666","#99FF99","#FF6666"),
          title = "Day 8 - Chlo A - Fluorescence")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")
#combined####
combi_chlor2 <- ggarrange(
  ggscatter(pb_Mi2_nDplot[1:18,],
            x="Origin",
            y="value",
            size = 4,
            fill="Origin",
            color = "black",
            shape = "Treatment",
            title = "Day 0",
            ylim=c(135,235),
            position=position_dodge(0.5))+
    border()+ylab("")+
    theme(legend.position = "none")+
    scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
    scale_shape_manual(values = c(21, 24))+
    guides(fill = guide_legend(override.aes = list(shape=21)))+xlab(""),
  ggscatter(pb_Mi2_nDplot[37:54,],
            x="Origin",
            y="value",
         fill="Origin",
         shape="Treatment",
         ylim=c(135,235),
         color="black",
         size = 4,
            position=position_dodge(0.5),
            title = "Day 4")+
    border()+ylab("")+
    theme(legend.position = "none")+
    scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
    scale_shape_manual(values = c(21, 24))+
    guides(fill = guide_legend(override.aes = list(shape=21)))+xlab(""),
  ggscatter(pb_Mi2_nDplot[73:90,],
            x="Origin",
            y="value",
            shape = "Treatment",
            fill = "Origin",
            size=4,
            ylim=c(135,235),
            color = "black",
            position = position_dodge(0.5),
            title = "Day 8")+
    border()+ylab("")+
    theme(legend.position = "none")+
    scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
    scale_shape_manual(values = c(21, 24))+
    guides(fill = guide_legend(override.aes = list(shape=21)))+xlab(""),
  common.legend = T,labels = c("A","B","C"),nrow = 1
)
annotate_figure(combi_chlor2,
                top = text_grob("Microcosm 2  - in vivo Chlo A Fluorescence",
                                face = "bold"),
                bottom = "Origin",
                left = "RFU")
time_chlo_2<-ggscatter(pb_Mi2_nDplot,
                       x = "timepoint",
                       y = "value",
                       fill = "Origin", 
                       color = "black",
                       shape = "Treatment", 
                       size = 4,
                       position = position_dodge(0.5))+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
  scale_x_discrete(labels=c(0,2,4,6,8))+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  border()+ 
  xlab("Experimental day")+
  ylab("RFU")
chlo2 <- annotate_figure(time_chlo_2,
                         text_grob("In vivo chlorophyll a Fluorescence",face = "bold"))


#CDOM####
CDOM2<-read.csv("Results/PC_countings/Data_MI/PB_MI2_CDOM.csv")
CDOM2<-na.omit(CDOM2)

CDOM2 <- CDOM2 %>% 
  filter(cuvette!="i2") %>% 
  filter(cuvette!="i5") %>% 
  filter(cuvette!="I9")

CDOM2_nD <- CDOM2 %>% 
  filter(treat!="Di")
CDOM2_nDp <- gather(CDOM2_nD,
                    timepoint,value,
                    S0:S4)
#timeseries
ggboxplot(CDOM2_nDp,
          x="timepoint",
          y="value",
          color = "Origin",
          add = c("mean_sd","jitter"),
          title = "MI2 - CDOM - Fluorescence",
          legend.title="Origin")+
  border()+
  scale_color_manual(values = c("#333333","#66CC99","#FF6666"))+
  ylab("RFU")+
  xlab("Experimental day")

#by treat####
ggboxplot(CDOM2_nDp,
          x="timepoint",
          y="value",
          color = "treat",
          add = c("mean_sd","jitter"),
          title = "MI2 - CDOM - Fluorescence",
          legend.title="Treatment")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")+
  xlab("Experimental day")

#by ori&treat####
#s0
ggboxplot(CDOM2_nDp[1:18,],
          x="Origin",
          y="value",
          color = "treat",
          add = c("mean_sd","jitter"),
          title = "Day 0",
          fill = c("#666666","#99FF99","#FF6666","#666666","#99FF99","#FF6666"),
          legend.title="Treatment")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")+
  xlab("")
#s2
ggboxplot(CDOM2_nDp[37:54,],
          x="Origin",
          y="value",
          color = "treat",
          add = c("mean_sd","jitter"),
          title = "Day 4",
          fill = c("#666666","#99FF99","#FF6666","#666666","#99FF99","#FF6666"),
          legend.title="Treatment")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")+
  xlab("")
#s4
ggboxplot(CDOM2_nDp[73:90,],
          x="Origin",
          y="value",
          color = "treat",
          add = c("mean_sd","jitter"),
          title = "Day 8",
          fill = c("#666666","#99FF99","#FF6666","#666666","#99FF99","#FF6666"),
          legend.title="Treatment")+
  border()+
  scale_color_manual(values = c("#FF6666","#333333"))+
  ylab("RFU")+
  xlab("")

#combined####
names(CDOM2_nDp)<-c("cuvette","Treatment","Replicate","Origin","bottle","timepoint","value")
combi_CDOM2 <- ggarrange(
  ggscatter(CDOM2_nDp[1:18,],
            x="Origin",
            y="value",
            color = "black",
            title = "Day 0",
         size = 4,
         fill="Origin",
         shape = "Treatment",
         ylim=c(3,50),
         position = position_dodge(0.5))+
    border()+ylab("")+
    theme(legend.position = "none")+
    scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
    scale_shape_manual(values = c(21, 24))+
    guides(fill = guide_legend(override.aes = list(shape=21)))+
    xlab(""),
  ggscatter(CDOM2_nDp[37:54,],
            x="Origin",
            y="value",
            color = "black",
            fill="Origin",
            ylim=c(3,50),
         size = 4,
         position = position_dodge(0.5),
            title = "Day 4",
           shape = "Treatment" )+
    border()+ylab("")+
    theme(legend.position = "none")+
    scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
    scale_shape_manual(values = c(21, 24))+
    guides(fill = guide_legend(override.aes = list(shape=21)))+
    xlab(""),
  ggscatter(CDOM2_nDp[73:90,],
            x="Origin",
            y="value",
            color = "black",
            ylim=c(3,50),
         size = 4,
         fill="Origin",
         shape = "Treatment",
            title = "Day 8",
            position = position_dodge(0.5))+
    border()+ylab("")+
    theme(legend.position = "none")+
    scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
    scale_shape_manual(values = c(21, 24))+
    guides(fill = guide_legend(override.aes = list(shape=21)))+
    xlab(""),common.legend = T,labels = c("A","B","C"),nrow = 1
)
annotate_figure(
  combi_CDOM2,
  top = text_grob("Microcosm 2 - CDOM Fluorescence",
                  face = "bold"),
  bottom = "Origin",left = "RFU")
time_cdom_2<-ggscatter(CDOM2_nDp,
                       x = "timepoint",
                       y = "value",
                       fill = "Origin", 
                       color = "black",
                       shape = "Treatment", 
                       size = 4,
                       position = position_dodge(0.5))+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
  scale_x_discrete(labels=c(0,2,4,6,8))+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  border()+ 
  xlab("Experimental day")+
  ylab("RFU")
c2 <- annotate_figure(time_cdom_2,
                      text_grob("CDOM Fluorescence",face = "bold"))
ggarrange(chlo2,c2,
          labels = c("A","B"))
#TESTS####
#M1####
#chlor_a####
#s4 by treat
qqnorm(pb1_nD$S4)
qqline(pb1_nD$S4)
boxplot(S4~treat,data = pb1_nD)
boxplot(S4~Origin,data = pb1_nD)
aov(S4~treat,data = pb1_nD)
plot(aov(S4~treat,data = pb1_nD)$residuals)
summary(aov(S4~treat,data = pb1_nD))
#by ori
aov(pb1_nD$S4~pb1_nD$Origin)
plot(aov(pb1_nD$S4~pb1_nD$Origin)$residuals)
summary(aov(pb1_nD$S4~pb1_nD$Origin))
TukeyHSD(aov(pb1_nD$S4~pb1_nD$Origin))
plot(TukeyHSD(aov(pb1_nD$S4~pb1_nD$Origin)),las=1)

#interaction
aov(pb1_nD$S4~pb1_nD$Origin*pb1_nD$treat)
summary(aov(pb1_nD$S4~pb1_nD$Origin*pb1_nD$treat))
TukeyHSD(aov(pb1_nD$S4~pb1_nD$Origin*pb1_nD$treat))

#CDOM####
hist(CDOM1_nD$S4)
qqnorm(CDOM1_nD$S4)
qqline(CDOM1_nD$S4)
aov(CDOM1_nD$S4~CDOM1_nD$treat*CDOM1_nD$Origin)
summary(aov(CDOM1_nD$S4~CDOM1_nD$treat*CDOM1_nD$Origin))
TukeyHSD(aov(CDOM1_nD$S4~CDOM1_nD$treat*CDOM1_nD$Origin))
#correlation####
plot(pb1_nD$S4~CDOM1_nD$S4)
abline(lm(pb1_nD$S4~CDOM1_nD$S4))
cor.test(pb1_nD$S4,CDOM1_nD$S4,method="kendall")
cor.test(pb1_nD$S4,CDOM1_nD$S4,method="spearman")

#M2####
#chlo_a####
qqnorm(pb_Mi2_nD$S4)
qqline(pb_Mi2_nD$S4)
hist(pb_Mi2_nD$S4)#not normally
boxplot(pb_Mi2_nD$S4~pb_Mi2_nD$Treatment)
boxplot(pb_Mi2_nD$S4~pb_Mi2_nD$Origin)
pb_Mi2_nD$cuvette <- paste(pb_Mi2_nD$Treatment,pb_Mi2_nD$Origin)
kruskal.test(pb_Mi2_nD$S4~pb_Mi2_nD$Treatment)
kruskal.test(pb_Mi2_nD$S4~pb_Mi2_nD$Origin)

kruskal.test(pb_Mi2_nD$S4,pb_Mi2_nD$cuvette,
                     p.adjust.method = "BH")

#CDOM####
hist(CDOM2_nD$S4)
qqnorm(CDOM2_nD$S4)
qqline(CDOM2_nD$S4)

summary(aov(CDOM2_nD$S4~CDOM2_nD$treat*CDOM2_nD$Origin))
TukeyHSD(aov(CDOM2_nD$S4~CDOM2_nD$treat*CDOM2_nD$Origin))
#correlation####
plot(pb_Mi2_nD$S4~CDOM2_nD$S4)
abline(lm(pb_Mi2_nD$S4~CDOM2_nD$S4))
cor.test(pb_Mi2_nD$S4,CDOM2_nD$S4,method = "spearman")
cor.test(pb_Mi2_nD$S4,CDOM2_nD$S4,method = "kendall")
shapiro.test(pb_Mi2_nD$S4)
shapiro.test(CDOM2_nD$S4)
