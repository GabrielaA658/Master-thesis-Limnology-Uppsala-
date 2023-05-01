#phytoplankton community composition
library(ggpubr)
library(tidyverse)
#ABSOLUTE ABUNDANCE####
m1 <- read.csv("vis_Mi1.csv")
#divide by start/end comm####
m1_e <- as.data.frame(split(m1,m1$Timepoint)[1])
m1_s <- as.data.frame(split(m1,m1$Timepoint)[2])
names(m1_e) <- names(m1)
names(m1_s) <- names(m1)

m2 <- read.csv("vis_Mi2.csv")
m2_e <- as.data.frame(split(m2,m2$Timepoint)[1])
m2_s <- as.data.frame(split(m2,m2$Timepoint)[2])
names(m2_e) <- names(m2)
names(m2_s) <- names(m2)

#remove Di treatment
m1_s_nod <- m1_s %>% 
  filter(Treatment!="Di")
m1_e_nod <- m1_e %>% 
  filter(Treatment!="Di")

m2_e_nod <- m2_e %>% 
  filter(Treatment!="Di")
m2_s_nod <- m2_s %>% 
  filter(Treatment!="Di")

m1_s_plot <- gather(m1_s_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)
m1_e_plot <- gather(m1_e_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)
m2_s_plot <- gather(m2_s_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)
m2_e_plot <- gather(m2_e_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)
#M1 origin####
m1_before <- ggbarplot(m1_s_plot,
                       x="Origin",
                       y="Abun",
                       color = "species",
                       title = "Start community Mi1",
                       combine = TRUE,
                       add = "mean",
                       fill = "species",
                       legend.title="Group",
                       palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                   "#FFCC33","#336600"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
m1_after <- ggbarplot(m1_e_plot,
                      x="Origin",
                      y="Abun",
                      color = "species",
                      title = "End community Mi1",
                      combine = TRUE,
                      add = "mean",
                      fill = "species",
                      legend.title="Group",
                      palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                  "#FFCC33","#336600"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(m1_before,m1_after,          common.legend = T,labels = c("A","B"))
#by treatment####
treat_m1s_abs <- ggbarplot(m1_s_plot,
                           x="Treatment",
                           y="Abun",
                           color = "species",
                           title = "before treatments Mi1",
                           combine = TRUE,
                           add = "mean",
                           fill = "species",
                           legend.title="Group",
                           palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                       "#FFCC33","#336600"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
treat_m1e_abs <- ggbarplot(m1_e_plot,
                           x="Treatment",
                           y="Abun",
                           color = "species",
                           title = "after treatments Mi1",
                           combine = TRUE,
                           add = "mean",
                           fill = "species",
                           legend.title="Group",
                           palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                       "#FFCC33","#336600"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

#combined####
m1_s_nod[,15] <- paste(m1_s_nod$Treatment,m1_s_nod$Origin) 
m1_e_nod[,15] <- paste(m1_e_nod$Treatment,m1_e_nod$Origin)

#re run gather
m1_s_plot <- gather(m1_s_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)
m1_e_plot <- gather(m1_e_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)
par(mfrow=c(2,1))
ggbarplot(m1_s_plot,
          x="V15",
          y="Abun",
          color = "species",
          title = "before treatments Mi1",
          combine = TRUE,
          add = "mean",
          fill = "species",
          legend.title="Group",
          palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                      "#FFCC33","#336600"),
          xlab = " ")+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
m1_abs <- ggbarplot(m1_e_plot,
          x="V15",
          y="Abun",
          color = "species",
          title = "Phytoplankton Abundance",
          combine = TRUE,
          add = "mean",
          ylim=c(0,500),
          fill = "species",
          legend.title="Group",
          palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                      "#FFCC33","#336600"),
          xlab = " ")+
  ylab("Cells/ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5,face = "bold"))

m1_s_plot$V15 <- factor(m1_s_plot$V15,
                        levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
m1_e_plot$V15 <- factor(m1_e_plot$V15,
                        levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
abs_m1_start_end<- (ggarrange(ggbarplot(m1_s_plot,
                    x="V15",
                    y="Abun",
                    color = "species",
                    title = "Day 0",
                    combine = TRUE,
                    add = "mean",
                    fill = "species",
                    legend.title="Group",
                    palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                "#FFCC33","#336600"),
                    xlab = " ")+
            ylab("Cells/ml")+
            theme(legend.title = element_text(size = 8),
                  legend.text = element_text(size = 8)),
          ggbarplot(m1_e_plot,
                    x="V15",
                    y="Abun",
                    color = "species",
                    title = "Day 8",
                    combine = TRUE,
                    add = "mean",
                    fill = "species",
                    legend.title="Group",
                    palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                "#FFCC33","#336600"),
                    xlab = " ")+
            ylab("Cells/ml")+
            theme(legend.title = element_text(size = 8),
                  legend.text = element_text(size = 8)),
          ncol = 1,
          nrow = 2,
          common.legend = T,
          labels = c("A","B"),
          label.y = 1))
annotate_figure(abs_m1_start_end,
                top = text_grob("Microcosm 1 - Phytoplankton abundance",face = "bold"),
                bottom = "Treatment")
#M2####
#ori####
m2_before <- ggbarplot(m2_s_plot,
                       x="Origin",
                       y="Abun",
                       color = "species",
                       title = "Start community Mi2",
                       combine = TRUE,
                       add = "mean",
                       fill = "species",
                       legend.title="Group",
                       palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                   "#330033","#FFCC33","#336600"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
m2_end <- ggbarplot(m2_e_plot,
                    x="Origin",
                    y="Abun",
                    color = "species",
                    title = "End community Mi2",
                    combine = TRUE,
                    add = "mean",
                    fill = "species",
                    legend.title="Group",
                    palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                "#330033","#FFCC33","#336600"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

ggarrange(m2_before,m2_end,
          common.legend = T,labels = c("A","B"))
#by treatment####
treat_m2s_abs <- ggbarplot(m2_s_plot,
                           x="Treatment",
                           y="Abun",
                           color = "species",
                           title = "before treatments Mi2",
                           combine = TRUE,
                           add = "mean",
                           fill = "species",
                           legend.title="Group",
                           palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                       "#330033","#FFCC33","#336600"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
treat_m2e_abs <- ggbarplot(m2_e_plot,
                           x="Treatment",
                           y="Abun",
                           color = "species",
                           title = "after treatments Mi2",
                           combine = TRUE,
                           add = "mean",
                           fill = "species",
                           legend.title="Group",
                           palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                       "#330033","#FFCC33","#336600"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

#combined####

m2_s_nod[,15] <- paste(m2_s_nod$Treatment,m2_s_nod$Origin) 
m2_e_nod[,15] <- paste(m2_e_nod$Treatment,m2_e_nod$Origin)          

m2_s_plot <- gather(m2_s_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)
m2_e_plot <- gather(m2_e_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)
m2_s_plot$V15 <- factor(m2_s_plot$V15,
                        levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
m2_e_plot$V15 <- factor(m2_e_plot$V15,
                        levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
m2_abs <- ggbarplot(m2_e_plot,
          x="V15",
          y="Abun",
          color = "species",
          title = "Phytoplankton Abundance",
          combine = TRUE,
          add = "mean",
          ylim=c(0,600),
          fill = "species",
          legend.title="Group",
          palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                      "#330033","#FFCC33","#336600"),
          xlab = " ")+
  ylab("Cells/ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold",hjust = 0.5))

abs_m2_start_end<- (ggarrange(ggbarplot(m2_s_plot,
                    x="V15",
                    y="Abun",
                    color = "species",
                    title = "Day 0",
                    combine = TRUE,
                    add = "mean",
                    fill = "species",
                    legend.title="Group",
                    palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                "#330033","#FFCC33","#336600"),
                    xlab = " ")+
            ylab("Cells/ml")+
            theme(legend.title = element_text(size = 8),
                  legend.text = element_text(size = 8)),
          ggbarplot(m2_e_plot,
                    x="V15",
                    y="Abun",
                    color = "species",
                    title = "Day 8",
                    combine = TRUE,
                    add = "mean",
                    fill = "species",
                    legend.title="Group",
                    palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                "#330033","#FFCC33","#336600"),
                    xlab = " ")+
            ylab("Cells/ml")+
            theme(legend.title = element_text(size = 8),
                  legend.text = element_text(size = 8)),
          ncol = 1,
          nrow = 2,
          common.legend = T,
          labels = c("A","B"),
          label.y = 1))
annotate_figure(abs_m2_start_end,
                top = text_grob("Microcosm 2 - Phytoplankton abundance",
                                face = "bold"))
#RELATIVE ABUNDANCE####
#M1####
r_m1 <- read.csv("g_vis_Mi1.csv")
r_m1 <- subset(r_m1,select = c(Timepoint,Treatment,Replicate,Origin,R.Streptophyta,R.Chlorophyta,
                               R.Cyanobacteria,R.Bacillariophyta,R.Cryptophyta,R.Heterokontophyta,
                               R.Ciliophora))
names(r_m1)<-c("Timepoint","Treatment","Replicate","Origin","Streptophyta","Chlorophyta",
               "Cyanobacteria","Bacillariophyta","Cryptophyta","Heterokontophyta",
               "Ciliophora")
r_m1<-na.omit(r_m1)
r_e1 <- as.data.frame(split(r_m1,r_m1$Timepoint)[1])
r_s1 <- as.data.frame(split(r_m1,r_m1$Timepoint)[2])
names(r_e1) <- names(r_m1)
names(r_s1) <- names(r_m1)

r_s1_nod <- r_s1 %>% 
  filter(Treatment!="Di")
r_e1_nod <- r_e1 %>% 
  filter(Treatment!="Di")

r_s1_plot <- gather(r_s1_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)

r_e1_plot <-gather(r_e1_nod,
                   species,
                   Abun,
                   Streptophyta:Ciliophora)
#Origin####
rm1_start <- ggbarplot(r_s1_plot,
                       x="Origin",
                       y="Abun",
                       color = "species",
                       title = "before treatments Mi1",
                       combine = TRUE,
                       add = "mean",
                       fill = "species",
                       legend.title="Group",
                       palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                   "#FFCC33","#336600"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
rm1_end <- ggbarplot(r_e1_plot,
                     x="Origin",
                     y="Abun",
                     color = "species",
                     title = "after treatments Mi1",
                     combine = TRUE,
                     add = "mean",
                     fill = "species",
                     legend.title="Group",
                     palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                 "#FFCC33","#336600"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

ggarrange(rm1_start,rm1_end,common.legend = T,labels = c("A","B"))
#Treatment####
rtreat_1s <- ggbarplot(r_s1_plot,
                       x="Treatment",
                       y="Abun",
                       color = "species",
                       title = "start community Mi1",
                       combine = TRUE,
                       add = "mean",
                       fill = "species",
                       legend.title="Group",
                       palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                   "#FFCC33","#336600"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

rtreat_1e <- ggbarplot(r_e1_plot,
                       x="Treatment",
                       y="Abun",
                       color = "species",
                       title = "end community Mi1",
                       combine = TRUE,
                       add = "mean",
                       fill = "species",
                       legend.title="Group",
                       palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                   "#FFCC33","#336600"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

ggarrange(treat_m1s_abs,treat_m1e_abs,
          rtreat_1s,rtreat_1e,
          common.legend = T,labels = c("A","B","C","D"))
#combined####
r_s1_nod[,12] <- paste(r_s1_nod$Treatment,r_s1_nod$Origin)
r_e1_nod[,12] <- paste(r_e1_nod$Treatment,r_e1_nod$Origin)
r_s1_plot <- gather(r_s1_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)
r_e1_plot <- gather(r_e1_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)
r_s1_plot$V12 <- factor(r_s1_plot$V12,
                        levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
r_e1_plot$V12 <- factor(r_e1_plot$V12,
                        levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
m1_rel <- ggbarplot(r_e1_plot,
          x="V12",
          y="Abun",
          color = "species",
          title = "Community composition",
          combine = TRUE,
          add = "mean",
          fill = "species",
          legend.title="Group",
          palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                      "#FFCC33","#336600"),
          xlab = " ")+
  ylab("Proportion")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold",hjust = 0.5))
ggarrange(m1_abs,
          m1_rel,
          common.legend = T,
          labels = c("A","B"))
rel_m1_start_end <- (ggarrange(ggbarplot(r_s1_plot,
                    x="V12",
                    y="Abun",
                    color = "species",
                    title = "Day 0",
                    combine = TRUE,
                    add = "mean",
                    fill = "species",
                    legend.title="Group",
                    palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                "#FFCC33","#336600"),
                    xlab = " ")+
            ylab("Proportion")+
            theme(legend.title = element_text(size = 8),
                  legend.text = element_text(size = 8)),
          ggbarplot(r_e1_plot,
                    x="V12",
                    y="Abun",
                    color = "species",
                    title = "Day 8",
                    combine = TRUE,
                    add = "mean",
                    fill = "species",
                    legend.title="Group",
                    palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                "#FFCC33","#336600"),
                    xlab = " ")+
            ylab("Proportion")+
            theme(legend.title = element_text(size = 8),
                  legend.text = element_text(size = 8)),
          ncol = 1,
          nrow = 2,
          common.legend = T,
          labels = c("A","B"),
          label.y = 1))
annotate_figure(rel_m1_start_end,
                top = text_grob("Microcosm 1 - Community composition",
                                face = "bold"))
#M2####

r_2 <- read.csv("g_vis_Mi2.csv")
r_2 <- na.omit(r_2)
r_s2 <- as.data.frame(split(r_2,r_2$Timepoint)[2])
names(r_s2) <- names(r_2)
r_e2 <- as.data.frame(split(r_2,r_2$Timepoint)[1])
names(r_e2) <- names(r_2)
r_s2 <- subset(r_s2,select = c(Timepoint,Treatment,Replicate,Origin,R.Streptophyta,R.Chlorophyta,
                               R.Cyanobacteria,R.Bacillariophyta,R.Cryptophyta,R.Heterokontophyta,
                               R.Ciliophora,R.Heliozoa))
names(r_s2)<-c("Timepoint","Treatment","Replicate","Origin","Streptophyta","Chlorophyta",
               "Cyanobacteria","Bacillariophyta","Cryptophyta","Heterokontophyta",
               "Ciliophora","Heliozoa")
r_e2 <- subset(r_e2,select = c(Timepoint,Treatment,Replicate,Origin,R.Streptophyta,R.Chlorophyta,
                               R.Cyanobacteria,R.Bacillariophyta,R.Cryptophyta,R.Heterokontophyta,
                               R.Ciliophora,R.Heliozoa))
names(r_e2)<-c("Timepoint","Treatment","Replicate","Origin","Streptophyta","Chlorophyta",
               "Cyanobacteria","Bacillariophyta","Cryptophyta","Heterokontophyta",
               "Ciliophora","Heliozoa")
r_s2_nod <- filter(r_s2,
                   Treatment!="Di")
r_e2_nod <- filter(r_e2,
                   Treatment!="Di")
r_s2_plot <- gather(r_s2_nod,
                    species,
                    Abun,
                    Streptophyta:Heliozoa)
r_e2_plot <- gather(r_e2_nod,
                    species,
                    Abun,
                    Streptophyta:Heliozoa)


#Origin####
rm2_s <- ggbarplot(r_s2_plot,
                   x="Origin",
                   y="Abun",
                   color = "species",
                   title = "before treatments Mi2",
                   combine = TRUE,
                   add = "mean",
                   fill = "species",
                   legend.title="Group",
                   palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                               "#330033","#FFCC33","#336600"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
rm2_e <- ggbarplot(r_e2_plot,
                   x="Origin",
                   y="Abun",
                   color = "species",
                   title = "after treatments Mi2",
                   combine = TRUE,
                   add = "mean",
                   fill = "species",
                   legend.title="Group",
                   palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                               "#330033","#FFCC33","#336600"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

ggarrange(rm2_s,rm2_e,
          common.legend = T,labels = c("A","B"))
#Treatment####
rtreat_2s <- ggbarplot(r_s2_plot,
                       x="Treatment",
                       y="Abun",
                       color = "species",
                       title = "before treatments Mi2",
                       combine = TRUE,
                       add = "mean",
                       fill = "species",
                       legend.title="Group",
                       palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                   "#330033","#FFCC33","#336600"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
rtreat_2e <- ggbarplot(r_e2_plot,
                       x="Treatment",
                       y="Abun",
                       color = "species",
                       title = "after treatments Mi2",
                       combine = TRUE,
                       add = "mean",
                       fill = "species",
                       legend.title="Group",
                       palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                   "#330033","#FFCC33","#336600"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

ggarrange(treat_m2s_abs,treat_m2e_abs,
          rtreat_2s,rtreat_2e,
          common.legend = T,labels = c("A","B","C","D"))
#combined####
r_s2_nod[,13] <- paste(r_s2_nod$Treatment,r_s2_nod$Origin)
r_e2_nod[,13] <- paste(r_e2_nod$Treatment,r_e2_nod$Origin)
r_s2_plot <- gather(r_s2_nod,
                    species,
                    Abun,
                    Streptophyta:Heliozoa)
r_e2_plot <- gather(r_e2_nod,
                    species,
                    Abun,
                    Streptophyta:Heliozoa)
#change column order

r_s2_plot$V13 <- factor(r_s2_plot$V13,
                        levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
r_e2_plot$V13 <- factor(r_e2_plot$V13,
                        levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
m2_rel <- ggbarplot(r_e2_plot,
          x="V13",
          y="Abun",
          color = "species",
          title = "Community composition",
          combine = TRUE,
          add = "mean",
          fill = "species",
          legend.title="Group",
          palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                      "#330033","#FFCC33","#336600"),
          xlab = " ")+
  ylab("Proportion")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold",hjust = 0.5))
ggarrange(m2_abs,
          m2_rel,
          labels = c("A","B"),common.legend = T)
rel_m2_start_end <- ggarrange(ggbarplot(r_s2_plot,
                    x="V13",
                    y="Abun",
                    color = "species",
                    title = "Day 0",
                    combine = TRUE,
                    add = "mean",
                    fill = "species",
                    legend.title="Group",
                    palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                "#330033","#FFCC33","#336600"),
                    xlab = " ")+
            ylab("Proportion")+
            theme(legend.title = element_text(size = 8),
                  legend.text = element_text(size = 8)),
          ggbarplot(r_e2_plot,
                    x="V13",
                    y="Abun",
                    color = "species",
                    title = "Day 8",
                    combine = TRUE,
                    add = "mean",
                    fill = "species",
                    legend.title="Group",
                    palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                                "#330033","#FFCC33","#336600"),
                    xlab = " ")+
            ylab("Proportion")+
            theme(legend.title = element_text(size = 8),
                  legend.text = element_text(size = 8)),
          ncol = 1,
          nrow = 2,
          common.legend = T,
          labels = c("A","B"),
          label.y = 1)
annotate_figure(rel_m2_start_end,
                top = text_grob("Microcosm 2 - Community composition",
                                face = "bold"))
#By GENUS####
#by treatment####
genus_m1 <- read.csv("D:/UPPSALA/Master_thesis/Results/PC_countings/Data_MI/raw-vis_Mi1.csv",
                     sep = ";")
genus_m2 <- read.csv("D:/UPPSALA/Master_thesis/Results/PC_countings/Data_MI/raw-vis_Mi2.csv",
                     sep = ";")
genus_m1_abs <- genus_m1[,1:37]
genus_m1_rel <- cbind(genus_m1[,1:3],genus_m1[,40:67])
genus_m1_abs[,38] <- paste(genus_m1_abs$Treatment,genus_m1_abs$Origin)
genus_m1_rel[,32] <- paste(genus_m1_rel$Treatment,genus_m1_rel$Origin)

#abs####
genus_em1 <- as.data.frame(split(genus_m1_abs,genus_m1_abs$Timepoint)[1])
genus_sm1 <- as.data.frame(split(genus_m1_abs,genus_m1_abs$Timepoint)[2])
names(genus_em1) <- names(genus_m1_abs)
names(genus_sm1) <- names(genus_m1_abs)

genus_sm1 <- genus_sm1 %>% 
  filter(Treatment!="Di")
genus_em1 <- genus_em1 %>% 
  filter(Treatment!="Di")

genus_sm1_plot <- gather(genus_sm1,
                         species,
                         Abun,
                         Fragilaria:Aphanizomenon)
genus_em1_plot <- gather(genus_em1,
                         species,
                         Abun,
                         Fragilaria:Aphanizomenon)
genus_sm1_plot$V38 <- factor(genus_sm1_plot$V38,
                        levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
genus_em1_plot$V38<-factor(genus_em1_plot$V38,
                           levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
s1_genus <- ggbarplot(genus_sm1_plot,
                      x="Treatment",
                      y="Abun",
                      color = "species",
                      title = "Start community Mi1",
                      combine = TRUE,
                      add = "mean",
                      fill = "species",
                      legend.title="Genus",
                      palette = c("#CCCCCC","#FFFF00","#00CCFF","#9900FF",
                                  "#006600","#999966","#0033FF","#FF6600",
                                  "#99FFCC","#FF0033","#FF33FF","#99FF33",
                                  "#00CC99","#663399","#339933","#3300CC",
                                  "#FF9900","#6699CC","#CC6633","#33FFFF",
                                  "#00FF00","#CCFF00","#CC0099","#FF00CC",
                                  "#FF99CC","#993300","#999900","#FFCC99"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
e1_genus <- ggbarplot(genus_em1_plot,
                      x="Treatment",
                      y="Abun",
                      color = "species", 
                      title = "End community Mi1",
                      combine = TRUE,
                      add = "mean",
                      fill = "species",
                      legend.title="Genus",
                      palette = c("#CCCCCC","#FFFF00","#00CCFF","#9900FF",
                                  "#006600","#999966","#0033FF","#FF6600",
                                  "#99FFCC","#FF0033","#FF33FF","#99FF33",
                                  "#00CC99","#663399","#339933","#3300CC",
                                  "#FF9900","#6699CC","#CC6633","#33FFFF",
                                  "#00FF00","#CCFF00","#CC0099","#FF00CC",
                                  "#FF99CC","#993300","#999900","#FFCC99"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(s1_genus,e1_genus,
          common.legend = T)
#relative####
rgenus_em1 <- as.data.frame(split(genus_m1_rel,genus_m1_rel$Timepoint)[1])
rgenus_sm1 <- as.data.frame(split(genus_m1_rel,genus_m1_rel$Timepoint)[2])

names(rgenus_em1) <- c("Timepoint","Treatment","Origin","Fragilaria","Pediastrum",
                       "Staurastrum","Asterionella","Dictyosphaerium",
                       "Microcystis","Desmodesmus","Eudorina","Chlorophyceae","Oocystis",  
                       "Merismopedia","Closterium","Mallomonas","Rhodomonas",
                       "Dolichospermum2","Rimostrombidium","Coelastrum","Ochromonas","Woroninchinia",
                       "Mesodinium","Cyclotella","Acanthoceras","Chlorella","Stephanodiscus","Chroococcus",    
                       "Dinobryon","Trachelomonas","Aphanizomenon","V32")

names(rgenus_sm1) <- c("Timepoint","Treatment","Origin","Fragilaria","Pediastrum",
                       "Staurastrum","Asterionella","Dictyosphaerium",
                       "Microcystis","Desmodesmus","Eudorina","Chlorophyceae","Oocystis",  
                       "Merismopedia","Closterium","Mallomonas","Rhodomonas",
                       "Dolichospermum2","Rimostrombidium","Coelastrum","Ochromonas","Woroninchinia",
                       "Mesodinium","Cyclotella","Acanthoceras","Chlorella","Stephanodiscus","Chroococcus",    
                       "Dinobryon","Trachelomonas","Aphanizomenon","V32")

rgenus_sm1 <- rgenus_sm1 %>% 
  filter(Treatment!="Di")
rgenus_em1 <- rgenus_em1 %>% 
  filter(Treatment!="Di")

rgenus_sm1_plot <- gather(rgenus_sm1,
                          species,
                          Abun,
                          Fragilaria:Aphanizomenon)

rgenus_em1_plot <- gather(rgenus_em1,
                          species,
                          Abun,
                          Fragilaria:Aphanizomenon)

rgenus_sm1_plot$V32 <- factor(rgenus_sm1_plot$V32,
                             levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))


rs1_genus <- ggbarplot(rgenus_sm1_plot,
                       x="Treatment",
                       y="Abun",
                       color = "species",
                       title = "Start community rMi1",
                       combine = TRUE,
                       add = "mean",
                       fill = "species",
                       legend.title="Genus",
                       palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                   "#006600","#999966","#0033FF","#990000",
                                   "#0000CC","#FF9933","#FF00FF","#99FF33",
                                   "#00CC99","#993333","#339933","#3300CC",
                                   "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                   "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                   "#9933FF","#CCFF66","#999900","#FFCC99"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
re1_genus <- ggbarplot(rgenus_em1_plot,
                       x="Treatment",
                       y="Abun",
                       color = "species", 
                       title = "End community rMi1",
                       combine = TRUE,
                       add = "mean",
                       fill = "species",
                       legend.title="Genus",
                       palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                   "#006600","#999966","#0033FF","#990000",
                                   "#0000CC","#FF9933","#FF00FF","#99FF33",
                                   "#00CC99","#993333","#339933","#3300CC",
                                   "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                   "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                   "#9933FF","#CCFF66","#999900","#FFCC99"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(rs1_genus,re1_genus,
          common.legend = T)
#M2####
genus_m2_abs <- genus_m2[,1:48]
genus_m2_rel <- cbind(genus_m2[,1:9],genus_m2[,51:89])

genus_m2_abs[,49] <- paste(genus_m2_abs$Treatment,genus_m2_abs$Origin)
genus_m2_rel[,49] <- paste(genus_m2_rel$Treatment,genus_m2_rel$Origin)
#abs####
genus_em2 <- as.data.frame(split(genus_m2_abs,genus_m2_abs$Timepoint)[1])
genus_sm2 <- as.data.frame(split(genus_m2_abs,genus_m2_abs$Timepoint)[2])
names(genus_em2) <- names(genus_m2_abs[,1:49])
names(genus_sm2) <- names(genus_m2_abs[,1:49])

genus_sm2 <- genus_sm2 %>% 
  filter(Treatment!="Di")
genus_em2 <- genus_em2 %>% 
  filter(Treatment!="Di")

genus_sm2_plot <- gather(genus_sm2,
                         species,
                         Abun,
                         Fragilaria:Chlamydomonas)
genus_em2_plot <- gather(genus_em2,
                         species,
                         Abun,
                         Fragilaria:Chlamydomonas)
s2_genus <- ggbarplot(genus_sm2_plot,
                      x="Treatment",
                      y="Abun",
                      color = "species",
                      title = "Start community Mi2",
                      combine = TRUE,
                      add = "mean",
                      fill = "species",
                      legend.title="Genus",
                      palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                  "#006600","#999966","#0033FF","#990000",
                                  "#0000CC","#FF9933","#FF00FF","#99FF33",
                                  "#00CC99","#993333","#339933","#3300CC",
                                  "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                  "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                  "#9933FF","#CCFF66","#999900","#FFCC99",
                                  "#333300","#FFFF99","#99FFCC","#003300",
                                  "#6666CC","#000000","#993300","#CC99CC",
                                  "#FF0033","#00CCCC","#CC0033","#00FF00"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
e2_genus <- ggbarplot(genus_em2_plot,
                      x="Treatment",
                      y="Abun",
                      color = "species", 
                      title = "End community Mi2",
                      combine = TRUE,
                      add = "mean",
                      fill = "species",
                      legend.title="Genus",
                      palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                  "#006600","#999966","#0033FF","#990000",
                                  "#0000CC","#FF9933","#FF00FF","#99FF33",
                                  "#00CC99","#993333","#339933","#3300CC",
                                  "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                  "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                  "#9933FF","#CCFF66","#999900","#FFCC99",
                                  "#333300","#FFFF99","#99FFCC","#003300",
                                  "#6666CC","#000000","#993300","#CC99CC",
                                  "#FF0033","#00CCCC","#CC0033","#00FF00"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(s2_genus,e2_genus,
          common.legend = T)
#relative####
rgenus_em2 <- as.data.frame(split(genus_m2_rel,genus_m2_rel$Timepoint)[1])
rgenus_sm2 <- as.data.frame(split(genus_m2_rel,genus_m2_rel$Timepoint)[2])

names(rgenus_em2) <- names(genus_m2_abs[,1:49])
names(rgenus_sm2) <- names(genus_m2_abs[,1:49])

rgenus_sm2 <- rgenus_sm2 %>% 
  filter(Treatment!="Di")
rgenus_em2 <- rgenus_em2 %>% 
  filter(Treatment!="Di")

rgenus_sm2_plot <- gather(rgenus_sm2,
                          species,
                          Abun,
                          Fragilaria:Chlamydomonas)
rgenus_em2_plot <- gather(rgenus_em2,
                          species,
                          Abun,
                          Fragilaria:Chlamydomonas)

rs2_genus <- ggbarplot(rgenus_sm2_plot,
                       x="Treatment",
                       y="Abun",
                       color = "species",
                       title = "Start community rMi2",
                       combine = TRUE,
                       add = "mean",
                       fill = "species",
                       legend.title="Genus",
                       palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                   "#006600","#999966","#0033FF","#990000",
                                   "#0000CC","#FF9933","#FF00FF","#99FF33",
                                   "#00CC99","#993333","#339933","#3300CC",
                                   "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                   "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                   "#9933FF","#CCFF66","#999900","#FFCC99",
                                   "#333300","#FFFF99","#99FFCC","#003300",
                                   "#6666CC","#000000","#993300","#CC99CC",
                                   "#FF0033","#00CCCC","#CC0033","#00FF00"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
re2_genus <- ggbarplot(rgenus_em2_plot,
                       x="Treatment",
                       y="Abun",
                       color = "species", 
                       title = "End community rMi2",
                       combine = TRUE,
                       add = "mean",
                       fill = "species",
                       legend.title="Genus",
                       palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                   "#006600","#999966","#0033FF","#990000",
                                   "#0000CC","#FF9933","#FF00FF","#99FF33",
                                   "#00CC99","#993333","#339933","#3300CC",
                                   "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                   "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                   "#9933FF","#CCFF66","#999900","#FFCC99",
                                   "#333300","#FFFF99","#99FFCC","#003300",
                                   "#6666CC","#000000","#993300","#CC99CC",
                                   "#FF0033","#00CCCC","#CC0033","#00FF00"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(rs2_genus,re2_genus,
          common.legend = T)
#by Origin####
#abs####
s1_genus_o <- ggbarplot(genus_sm1_plot,
                        x="Origin",
                        y="Abun",
                        color = "species",
                        title = "Start community Mi1",
                        combine = TRUE,
                        add = "mean",
                        fill = "species",
                        legend.title="Genus",
                        palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                    "#006600","#999966","#0033FF","#990000",
                                    "#0000CC","#FF9933","#FF00FF","#99FF33",
                                    "#00CC99","#993333","#339933","#3300CC",
                                    "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                    "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                    "#9933FF","#CCFF66","#999900","#FFCC99"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
e1_genus_o <- ggbarplot(genus_em1_plot,
                        x="Origin",
                        y="Abun",
                        color = "species", 
                        title = "End community Mi1",
                        combine = TRUE,
                        add = "mean",
                        fill = "species",
                        legend.title="Genus",
                        palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                    "#006600","#999966","#0033FF","#990000",
                                    "#0000CC","#FF9933","#FF00FF","#99FF33",
                                    "#00CC99","#993333","#339933","#3300CC",
                                    "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                    "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                    "#9933FF","#CCFF66","#999900","#FFCC99"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(s1_genus_o,e1_genus_o,
          common.legend = T)
#relative####
rs1_genus_o <- ggbarplot(rgenus_sm1_plot,
                         x="Origin",
                         y="Abun",
                         color = "species",
                         title = "Start community rMi1",
                         combine = TRUE,
                         add = "mean",
                         fill = "species",
                         legend.title="Genus",
                         palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                     "#006600","#999966","#0033FF","#990000",
                                     "#0000CC","#FF9933","#FF00FF","#99FF33",
                                     "#00CC99","#993333","#339933","#3300CC",
                                     "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                     "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                     "#9933FF","#CCFF66","#999900","#FFCC99"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
re1_genus_o <- ggbarplot(rgenus_em1_plot,
                         x="Origin",
                         y="Abun",
                         color = "species", 
                         title = "End community rMi1",
                         combine = TRUE,
                         add = "mean",
                         fill = "species",
                         legend.title="Genus",
                         palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                     "#006600","#999966","#0033FF","#990000",
                                     "#0000CC","#FF9933","#FF00FF","#99FF33",
                                     "#00CC99","#993333","#339933","#3300CC",
                                     "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                     "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                     "#9933FF","#CCFF66","#999900","#FFCC99"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(rs1_genus_o,re1_genus_o,
          common.legend = T)
#M2####
#abs####
s2_genus_o <- ggbarplot(genus_sm2_plot,
                        x="Origin",
                        y="Abun",
                        color = "species",
                        title = "Start community Mi2",
                        combine = TRUE,
                        add = "mean",
                        fill = "species",
                        legend.title="Genus",
                        palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                    "#006600","#999966","#0033FF","#990000",
                                    "#0000CC","#FF9933","#FF00FF","#99FF33",
                                    "#00CC99","#993333","#339933","#3300CC",
                                    "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                    "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                    "#9933FF","#CCFF66","#999900","#FFCC99",
                                    "#333300","#FFFF99","#99FFCC","#003300",
                                    "#6666CC","#000000","#993300","#CC99CC",
                                    "#FF0033","#00CCCC","#CC0033"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
e2_genus_o <- ggbarplot(genus_em2_plot,
                        x="Origin",
                        y="Abun",
                        color = "species", 
                        title = "End community Mi2",
                        combine = TRUE,
                        add = "mean",
                        fill = "species",
                        legend.title="Genus",
                        palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                    "#006600","#999966","#0033FF","#990000",
                                    "#0000CC","#FF9933","#FF00FF","#99FF33",
                                    "#00CC99","#993333","#339933","#3300CC",
                                    "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                    "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                    "#9933FF","#CCFF66","#999900","#FFCC99",
                                    "#333300","#FFFF99","#99FFCC","#003300",
                                    "#6666CC","#000000","#993300","#CC99CC",
                                    "#FF0033","#00CCCC","#CC0033"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(s2_genus_o,e2_genus_o,
          common.legend = T)

#relative####
rs2_genus_o <- ggbarplot(rgenus_sm2_plot,
                         x="Origin",
                         y="Abun",
                         color = "species",
                         title = "Start community rMi2",
                         combine = TRUE,
                         add = "mean",
                         fill = "species",
                         legend.title="Genus",
                         palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                     "#006600","#999966","#0033FF","#990000",
                                     "#0000CC","#FF9933","#FF00FF","#99FF33",
                                     "#00CC99","#993333","#339933","#3300CC",
                                     "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                     "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                     "#9933FF","#CCFF66","#999900","#FFCC99",
                                     "#333300","#FFFF99","#99FFCC","#003300",
                                     "#6666CC","#000000","#993300","#CC99CC",
                                     "#FF0033","#00CCCC","#CC0033"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
re2_genus_o <- ggbarplot(rgenus_em2_plot,
                         x="Origin",
                         y="Abun",
                         color = "species", 
                         title = "End community rMi2",
                         combine = TRUE,
                         add = "mean",
                         fill = "species",
                         legend.title="Genus",
                         palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                     "#006600","#999966","#0033FF","#990000",
                                     "#0000CC","#FF9933","#FF00FF","#99FF33",
                                     "#00CC99","#993333","#339933","#3300CC",
                                     "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                     "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                     "#9933FF","#CCFF66","#999900","#FFCC99",
                                     "#333300","#FFFF99","#99FFCC","#003300",
                                     "#6666CC","#000000","#993300","#CC99CC",
                                     "#FF0033","#00CCCC","#CC0033"))+
  ylab("Relative cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(rs2_genus_o,re2_genus_o,
          common.legend = T)
s1_genus_o <- ggbarplot(genus_sm1_plot,
                        x="Origin",
                        y="Abun",
                        color = "species",
                        title = "Start community Mi1",
                        combine = TRUE,
                        add = "mean",
                        fill = "species",
                        legend.title="Genus",
                        palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                    "#006600","#999966","#0033FF","#990000",
                                    "#0000CC","#FF9933","#FF00FF","#99FF33",
                                    "#00CC99","#993333","#339933","#3300CC",
                                    "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                    "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                    "#9933FF","#CCFF66","#999900","#FFCC99"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
e1_genus_o <- ggbarplot(genus_em1_plot,
                        x="Origin",
                        y="Abun",
                        color = "species", 
                        title = "End community Mi1",
                        combine = TRUE,
                        add = "mean",
                        fill = "species",
                        legend.title="Genus",
                        palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                    "#006600","#999966","#0033FF","#990000",
                                    "#0000CC","#FF9933","#FF00FF","#99FF33",
                                    "#00CC99","#993333","#339933","#3300CC",
                                    "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                    "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                    "#9933FF","#CCFF66","#999900","#FFCC99"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(s1_genus_o,e1_genus_o,
          common.legend = T)
#combined####
#abs####
genus_sm1_plot$V38 <- factor(genus_sm1_plot$V38,
                        levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
genus_em1_plot$V38 <- factor(genus_em1_plot$V38,
                             levels = c("Add C","Con C","Add D","Con D","Add E","Con E"))
s1_genus_i <- ggbarplot(genus_sm1_plot,
                        x="V38",
                        y="Abun",
                        color = "species",
                        title = "Day 0",
                        combine = TRUE,
                        add = "mean",
                        fill = "species",
                        legend.title="Genus",
                        palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                    "#006600","#999966","#0033FF","#990000",
                                    "#0000CC","#FF9933","#FF00FF","#99FF33",
                                    "#00CC99","#993333","#339933","#3300CC",
                                    "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                    "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                    "#9933FF","#CCFF66","#999900","#FFCC99",
                                    "#333300","#FFFF99","#99FFCC","#003300",
                                    "#6666CC","#000000","#993300","#CC99CC",
                                    "#FF0033","#00CCCC","#CC0033","#00FF00"))+
  ylab("Cells/ml")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
genus_em1_plot$V38<-as.factor(genus_em1_plot$V38,
                              levels=c("Add C","Con C","Add D","Con D","Add E","Con E"))
e1_genus_i <- ggbarplot(genus_em1_plot,
                        x="V38",
                        y="Abun",
                        color = "species", 
                        title = "Day 8",
                        combine = TRUE,
                        add = "mean",
                        fill = "species",
                        legend.title="Genus",
                        palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                    "#006600","#999966","#0033FF","#990000",
                                    "#0000CC","#FF9933","#FF00FF","#99FF33",
                                    "#00CC99","#993333","#339933","#3300CC",
                                    "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                    "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                    "#9933FF","#CCFF66","#999900","#FFCC99",
                                    "#333300","#FFFF99","#99FFCC","#003300",
                                    "#6666CC","#000000","#993300","#CC99CC",
                                    "#FF0033","#00CCCC","#CC0033","#00FF00"))+
  ylab("Cells/ml")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(s1_genus_i,e1_genus_i,
          common.legend = T)
annotate_figure(ggarrange(s1_genus_i,e1_genus_i,
                          common.legend = T),
                top = text_grob("Microcosm 1 - Phytoplankton abundance",face = "bold"))
#relative####
rgenus_sm1_plot$V32<-as.factor(rgenus_sm1_plot$V32,
                               levels=c("Add C", "Con C","Add D","Con D","Add E","Con E"))
rgenus_em1_plot$V32 <- factor(rgenus_em1_plot$V32,
                                 levels=c("Add C","Con C", "Add D","Con D", "Add E", "Con E"))
rs1_genus_i <- ggbarplot(rgenus_sm1_plot,
                         x="V32",
                         y="Abun",
                         color = "species",
                         title = "Day 0",
                         combine = TRUE,
                         add = "mean",
                         fill = "species",
                         legend.title="Genus",
                         palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                     "#006600","#999966","#0033FF","#990000",
                                     "#0000CC","#FF9933","#FF00FF","#99FF33",
                                     "#00CC99","#993333","#339933","#3300CC",
                                     "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                     "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                     "#9933FF","#CCFF66","#999900","#FFCC99"))+
  ylab("Proportion")+
  xlab(" ")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
re1_genus_i <- ggbarplot(rgenus_em1_plot,
                         x="V32",
                         y="Abun",
                         color = "species", 
                         title = "Day 8",
                         combine = TRUE,
                         add = "mean",
                         fill = "species",
                         legend.title="Genus",
                         palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                     "#006600","#999966","#0033FF","#990000",
                                     "#0000CC","#FF9933","#FF00FF","#99FF33",
                                     "#00CC99","#993333","#339933","#3300CC",
                                     "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                     "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                     "#9933FF","#CCFF66","#999900","#FFCC99"))+
  ylab("Proportion")+
  xlab(" ")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
annotate_figure(ggarrange(rs1_genus_i,re1_genus_i,
                   common.legend = T),
         top=text_grob("Microcosm 1 - Community composition by Genus", face = "bold"))
#M2####
s2_genus_i <- ggbarplot(genus_sm2_plot,
                        x="V49",
                        y="Abun",
                        color = "species",
                        title = "Start community Mi2",
                        combine = TRUE,
                        add = "mean",
                        fill = "species",
                        legend.title="Genus",
                        palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                    "#006600","#999966","#0033FF","#990000",
                                    "#0000CC","#FF9933","#FF00FF","#99FF33",
                                    "#00CC99","#993333","#339933","#3300CC",
                                    "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                    "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                    "#9933FF","#CCFF66","#999900","#FFCC99",
                                    "#333300","#FFFF99","#99FFCC","#003300",
                                    "#6666CC","#000000","#993300","#CC99CC",
                                    "#FF0033","#00CCCC","#CC0033"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
e2_genus_i <- ggbarplot(genus_em2_plot,
                        x="V49",
                        y="Abun",
                        color = "species", 
                        title = "End community Mi2",
                        combine = TRUE,
                        add = "mean",
                        fill = "species",
                        legend.title="Genus",
                        palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                    "#006600","#999966","#0033FF","#990000",
                                    "#0000CC","#FF9933","#FF00FF","#99FF33",
                                    "#00CC99","#993333","#339933","#3300CC",
                                    "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                    "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                    "#9933FF","#CCFF66","#999900","#FFCC99",
                                    "#333300","#FFFF99","#99FFCC","#003300",
                                    "#6666CC","#000000","#993300","#CC99CC",
                                    "#FF0033","#00CCCC","#CC0033"))+
  ylab("Absolute cell abundace / ml")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ggarrange(s2_genus_i,e2_genus_i,
          common.legend = T)
#relative####
rgenus_sm2_plot$V49<-factor(rgenus_sm2_plot$V49,
                               levels=c("Add C","Con C","Add D","Con D","Add E","Con E"))
rgenus_em2_plot$V49 <- factor(rgenus_em2_plot$V49,
                                 levels=c("Add C","Con C","Add D","Con D","Add E","Con E"))
rs2_genus_i <- ggbarplot(rgenus_sm2_plot,
                         x="V49",
                         y="Abun",
                         color = "species",
                         title = "Day 0",
                         combine = TRUE,
                         add = "mean",
                         fill = "species",
                         legend.title="Genus",
                         palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                     "#006600","#999966","#0033FF","#990000",
                                     "#0000CC","#FF9933","#FF00FF","#99FF33",
                                     "#00CC99","#993333","#339933","#3300CC",
                                     "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                     "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                     "#9933FF","#CCFF66","#999900","#FFCC99",
                                     "#333300","#FFFF99","#99FFCC","#003300",
                                     "#6666CC","#000000","#993300","#CC99CC",
                                     "#FF0033","#00CCCC","#CC0033"))+
  ylab("Proportion")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
re2_genus_i <- ggbarplot(rgenus_em2_plot,
                         x="V49",
                         y="Abun",
                         color = "species", 
                         title = "Day 8",
                         combine = TRUE,
                         add = "mean",
                         fill = "species",
                         legend.title="Genus",
                         palette = c("#CCCCCC","#FF3300","#00CCFF","#9900FF",
                                     "#006600","#999966","#0033FF","#990000",
                                     "#0000CC","#FF9933","#FF00FF","#99FF33",
                                     "#00CC99","#993333","#339933","#3300CC",
                                     "#FFFF00","#6699CC","#CCFF33","#FF0000",
                                     "#00FF00","#CCFF00","#CC3300","#FF00CC",
                                     "#9933FF","#CCFF66","#999900","#FFCC99",
                                     "#333300","#FFFF99","#99FFCC","#003300",
                                     "#6666CC","#000000","#993300","#CC99CC",
                                     "#FF0033","#00CCCC","#CC0033"))+
  ylab("Proportion")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
annotate_figure(ggarrange(rs2_genus_i,re2_genus_i,
          common.legend = T),
          top = text_grob("Microcosm 2 - Community composition by Genus",face = "bold"))


