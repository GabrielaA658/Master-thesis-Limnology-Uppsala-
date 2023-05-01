library(ggpubr)
library(vegan)
library(tidyverse)
OrgC<-read.csv("D:/UPPSALA/Master_thesis/Results/PC_countings/Data_MI/Shimadzu_1.csv")
C<-data.frame(split(OrgC,OrgC$Analysis.Inj.)[2])

NPOC<-data.frame(cbind(C$NPOC.Sample.ID,C$NPOC.Inj..No.,C$NPOC.Treatment,
                       C$NPOC.Origin,C$NPOC.Conc.))
colnames(NPOC)<-c("Sample.ID","Inj","Treatment","Origin","NPOC.ppm")
NPOC$NPOC.ppm<-gsub(",",".",NPOC$NPOC.ppm)
NPOC$NPOC.ppm<-as.numeric(NPOC$NPOC.ppm)
which(NPOC$Inj==4)              

#TN
N<-data.frame(split(OrgC,OrgC$Analysis.Inj.)[3])
TN<-data.frame(cbind(N$TN.Sample.ID,N$TN.Inj..No.,N$TN.Treatment,
                     N$TN.Origin,              N$TN.Conc.))
colnames(TN)<-c("Sample.ID","Inj","Treatment","Origin","TN.ppm")
TN$TN.ppm<-gsub(",",".",TN$TN.ppm)
TN$TN.ppm<-as.numeric(TN$TN.ppm)
which(TN$Inj==4)

#MI1 <- cbind(NPOC,TN$TN.ppm) CANT BECAUSE DIFF INJ

#subset data to group replicates
#Treatmenrs MI1 W####
MI1_C<-NPOC%>% 
  filter(Sample.ID!="MQ") %>% 
  filter(Sample.ID!="aniontd2 0.905pppmN") %>% 
  filter(Sample.ID!="NH4Cl 0.967ppm N") %>% 
  #filter(Treatment!="MQ") %>% 
  filter(Sample.ID!="EDTA 8.56ppmC") %>% 
  filter(Sample.ID!="KHP9.90ppmC")
MI1_N <- TN%>% 
  filter(Sample.ID!="MQ") %>% 
  filter(Sample.ID!="aniontd2 0.905pppmN") %>% 
  filter(Sample.ID!="NH4Cl 0.967ppm N") %>% 
  #filter(Treatment!="MQ") %>% 
  filter(Sample.ID!="EDTA 8.56ppmC") %>% 
  filter(Sample.ID!="KHP9.90ppmC")

Mi_env <- MI1_C[1:61,]
Mi_treat<-MI1_C[62:146,]
Min_env <- MI1_N[1:61,]
Min_treat <-MI1_N[62:146,]
all_m1 <- cbind(Mi_treat,Min_treat$TN.ppm)
env_m1 <- cbind(Mi_env,Min_env$TN.ppm)
names(all_m1) <- c("Sample.ID","Inj","Treatment","Origin","NPOC.ppm","TN.ppm")
names(env_m1)<-c("Sample.ID","Inj","Treatment","Origin","NPOC_ppm","TN_ppm") 

all_m1_nod <- all_m1 %>% 
  filter(Treatment!="Di")

all_m1[all_m1$NPOC.ppm <1,]
all_m1 <- all_m1[all_m1$NPOC.ppm >1,]

all_m1_nod[all_m1_nod$NPOC.ppm <1,]
all_m1_nod <- all_m1_nod[all_m1_nod$NPOC.ppm >1,]

#m2####
m2 <- read.csv("Results/PC_countings/Data_MI/Shimadzu_2.csv")
m2_c <- as.data.frame(split(m2,m2$Analysis.Inj..)[2])
m2_n <- as.data.frame(split(m2,m2$Analysis.Inj..)[3])

tn_m2<-data.frame(cbind(m2_n$TN.Sample.ID,m2_n$TN.Inj..No.,
                        m2_n$TN.Treatment,m2_n$TN.Origin,
                        m2_n$TN.Conc.))

colnames(tn_m2)<-c("Sample.ID","Inj","Treatment","Origin","TN.ppm")
tn_m2$TN.ppm<-gsub(",",".",tn_m2$TN.ppm)
tn_m2$TN.ppm<-as.numeric(tn_m2$TN.ppm)

tc_m2 <- data.frame(cbind(m2_c$NPOC.Sample.ID,m2_c$NPOC.Inj..No.,
                          m2_c$NPOC.Treatment,m2_c$NPOC.Origin,
                          m2_c$NPOC.Conc.))
colnames(tc_m2)<-c("Sample.ID","Inj","Treatment","Origin","NPOC.ppm")
tc_m2$NPOC.ppm <- gsub(",",".",tc_m2$NPOC.ppm)
tc_m2$NPOC.ppm <- as.numeric(tc_m2$NPOC.ppm)

m2_treat <- tn_m2 %>% 
  filter(Treatment!="MQ") %>% 
  filter(Treatment!="Calib1_N") %>% 
  filter(Treatment!="Calib2_C") %>% 
  filter(Treatment!="Calib2_N") %>% 
  filter(Treatment!="Calib1_C") 
m2_treat_c <- tc_m2 %>% 
  filter(Treatment!="MQ") %>% 
  filter(Treatment!="Calib1_N") %>% 
  filter(Treatment!="Calib2_C") %>% 
  filter(Treatment!="Calib2_N") %>% 
  filter(Treatment!="Calib1_C") 

m2_treat[m2_treat$TN.ppm <0.3,]
m2_treat <- m2_treat[m2_treat$TN.ppm >0.3,]

m2_treat_c[m2_treat_c$NPOC.ppm <0.73,] #lots!
m2_treat_c <- m2_treat_c[m2_treat_c$NPOC.ppm >0.73,]

m2_treat_nod <- m2_treat %>% 
  filter(Treatment!="Di")

m2_treat_nod[m2_treat_nod$TN.ppm <0.4,]
m2_treat_nod <- m2_treat_nod[m2_treat_nod$TN.ppm >0.4,]

m2_treat_cnod <- m2_treat_c %>% 
  filter(Treatment!="Di")

m2_treat_cnod[m2_treat_cnod$NPOC.ppm <1,]
m2_treat_cnod <- m2_treat_cnod[m2_treat_cnod$NPOC.ppm >1,]
#P####
TP_data<-read.csv("Results/PC_countings/Data_MI/TP_abs_calculations.csv")

TP<-subset(TP_data,
           select = c("Sample_ID","Treatment","Origin","Tot.P..µg.L."))
TP_M2<-TP[13:30,]
TP_M1 <- TP[45:62,]

TP_M1$Origin<-factor(TP_M1$Origin,levels = c("C","D","E"))
TP_M2$Origin<-factor(TP_M2$Origin,levels = c("C","D","E"))

all_m1_nod$Origin<-factor(all_m1_nod$Origin,levels = c("C","D","E"))
m2_treat_cnod$Origin<-factor(m2_treat_cnod$Origin,levels = c("C","D","E"))
m2_treat_nod$Origin<-factor(m2_treat_nod$Origin,levels = c("C","D","E"))
p_m1 <- ggscatter(TP_M1,
                  x="Origin",
                  y="Tot.P..µg.L.",
                  size=2,
                  mean.point = T,
                  mean.point.size = 4,
                  position=position_dodge(0.5),
                  shape="Treatment",
                  main="  Total phosphorus (TP)",
                  color = "black",
                  fill = "Origin",
                  ylab = "µg/L",
                  xlab = "")+
  border()+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
  scale_shape_manual(values = c(21, 24))+
  guides(fill = guide_legend(override.aes = list(shape=21)))
n_m1<-ggscatter(all_m1_nod,
          x="Origin",
          y="TN.ppm",
          fill="Origin",
          size = 2,
          mean.point = T,
          mean.point.size = 4,
          shape = "Treatment",
          color = "black",
          ylab = "mg/L",
          xlab = "",
          position=position_dodge(0.5),
          title = "Total nitrogen (TN)")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
  scale_shape_manual(values = c(21, 24))+
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  border()
c_m1<-ggscatter(all_m1_nod,
               x="Origin",
               y="NPOC.ppm",
               color = "black",
               ylab = "mg/L",
               xlab = "",
               mean.point = T,
               mean.point.size = 4,
               size = 2,
               shape = "Treatment",
               fill="Origin",
               position = position_dodge(0.5),
               title = "Total organic carbon (TOC)")+
  theme(legend.position = "none")+
  border()+
  scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
  scale_shape_manual(values = c(21, 24))+
  guides(fill = guide_legend(override.aes = list(shape=21)))
#M1 general####
nutrient_m1<-ggarrange(p_m1,
  n_m1,
       c_m1     ,
            nrow =   1,labels = c("A","B","C"),common.legend = T)

annotate_figure(nutrient_m1,
                top = text_grob("Microcosm 1 - nutrient content day 8",face = "bold"),
                bottom = "Origin")
#m2 general ####
p_m2<- ggscatter(TP_M2,
                 x="Origin",
                 y="Tot.P..µg.L.",
                 shape = "Treatment",
                 main="  Total phosphorus (TP)",
                 color = "black",
                 fill = "Origin",
                 size = 2,
                 mean.point = T,
                 position=position_dodge(0.5),
                 mean.point.size = 4,
                 ylab = "µg/L",
                 xlab = "")+
  theme(legend.position = "none")+
  border()+
  scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
  scale_shape_manual(values = c(21, 24))+
  guides(fill = guide_legend(override.aes = list(shape=21)))
n_m2<-ggarrange(ggscatter(m2_treat_nod,
                    x="Origin",
                    y="TN.ppm",
                    shape="Treatment",
                    size = 2,
                    mean.point=T,
                    mean.point.size=4,
                    position=position_dodge(0.5),
                    color = "black",
                    fill = "Origin",
                    title = "Total nitrogen (TN)",
                    ylab = "mg/L",
                    xlab = "")+
            theme(legend.position = "none")+
            border()+
            scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
            scale_shape_manual(values = c(21, 24))+
            guides(fill = guide_legend(override.aes = list(shape=21))))
c_m2<-ggscatter(m2_treat_cnod,
                    x="Origin",
                    y="NPOC.ppm",
                    mean.point = T                    ,
                    mean.point.size = 4,
                    fill = "Origin",
                    color = "black",
                    shape = "Treatment",
                    position=position_dodge(0.5),
                    title = "Total organic carbon (TOC)",
                    ylab = "mg/L",xlab = "")+
            theme(legend.position = "none")+
            border()+
            scale_fill_manual(values = c("#666666","#99FF99","#FF6666"))+
            scale_shape_manual(values = c(21, 24))+
            guides(fill = guide_legend(override.aes = list(shape=21)))
nutrient_m2<-ggarrange(
  p_m2,
  n_m2,
  c_m2          ,labels = c("A","B","C"),
  nrow = 1,common.legend = T)
annotate_figure(nutrient_m2,
                top = text_grob("Microcosm 2 - nutrient content day 8",face = "bold"),
                bottom = "Origin")
#tests M1####
#TOC
hist(all_m1_nod$NPOC.ppm)
boxplot(all_m1_nod$NPOC.ppm~all_m1_nod$Origin)
boxplot(all_m1_nod$NPOC.ppm~all_m1_nod$Treatment)
boxplot(all_m1_nod$NPOC.ppm~paste(all_m1_nod$Origin,all_m1_nod$Treatment))
qqnorm(all_m1_nod$NPOC.ppm)
qqline(all_m1_nod$NPOC.ppm)

kruskal.test(all_m1_nod$NPOC.ppm~all_m1_nod$Origin)
kruskal.test(all_m1_nod$NPOC.ppm~all_m1_nod$Treatment)
kruskal.test(all_m1_nod$NPOC.ppm~paste(all_m1_nod$Origin,all_m1_nod$Treatment))
pairwise.wilcox.test(all_m1_nod$NPOC.ppm,all_m1_nod$Origin,
                     p.adjust.method = "BH")
pairwise.wilcox.test(all_m1_nod$NPOC.ppm,
                     paste(all_m1_nod$Origin,all_m1_nod$Treatment),
                     p.adjust.method="BH")
#TN
hist(all_m1_nod$TN.ppm)
qqnorm(all_m1_nod$TN.ppm)
qqline(all_m1_nod$TN.ppm)
boxplot(all_m1_nod$TN.ppm~all_m1_nod$Origin)
boxplot(all_m1_nod$TN.ppm~all_m1_nod$Treatment)
boxplot(all_m1_nod$TN.ppm~paste(all_m1_nod$Origin,all_m1_nod$Treatment))

summary(aov(all_m1_nod$TN.ppm~all_m1_nod$Origin*all_m1_nod$Treatment))
TukeyHSD(aov(all_m1_nod$TN.ppm~all_m1_nod$Origin*all_m1_nod$Treatment))

#Tot P
qqnorm(TP_M1$Tot.P..µg.L.)
qqline(TP_M1$Tot.P..µg.L.)
hist(TP_M1$Tot.P..µg.L.)
boxplot(TP_M1$Tot.P..µg.L.~paste(TP_M1$Treatment,TP_M1$Origin))
aov(TP_M1$Tot.P..µg.L.~TP_M1$Treatment*TP_M1$Origin)
summary(aov(TP_M1$Tot.P..µg.L.~TP_M1$Treatment*TP_M1$Origin))
TukeyHSD(aov(TP_M1$Tot.P..µg.L.~TP_M1$Treatment*TP_M1$Origin))

#tests MI2####
#TOC
hist(m2_treat_cnod$NPOC.ppm)
boxplot(m2_treat_cnod$NPOC.ppm~m2_treat_cnod$Origin)
boxplot(m2_treat_cnod$NPOC.ppm~m2_treat_cnod$Treatment)
boxplot(m2_treat_cnod$NPOC.ppm~paste(m2_treat_cnod$Origin,m2_treat_cnod$Treatment))
qqnorm(m2_treat_cnod$NPOC.ppm)
qqline(m2_treat_cnod$NPOC.ppm)

kruskal.test(m2_treat_cnod$NPOC.ppm~m2_treat_cnod$Origin)
kruskal.test(m2_treat_cnod$NPOC.ppm~m2_treat_cnod$Treatment)
kruskal.test(m2_treat_cnod$NPOC.ppm,paste(m2_treat_cnod$Origin,
                                          m2_treat_cnod$Treatment))
pairwise.wilcox.test(m2_treat_cnod$NPOC.ppm,paste(m2_treat_cnod$Origin,
                                                  m2_treat_cnod$Treatment),
                     p.adjust.method = "BH")
#TN
hist(m2_treat_nod$TN.ppm)
qqnorm(m2_treat_nod$TN.ppm)
qqline(m2_treat_nod$TN.ppm)
boxplot(m2_treat_nod$TN.ppm~m2_treat_nod$Origin)
boxplot(m2_treat_nod$TN.ppm~m2_treat_nod$Treatment)
boxplot(m2_treat_nod$TN.ppm~paste(m2_treat_nod$Origin,m2_treat_nod$Treatment))

summary(aov(m2_treat_nod$TN.ppm~m2_treat_nod$Origin*m2_treat_nod$Treatment))
TukeyHSD(aov(m2_treat_nod$TN.ppm~m2_treat_nod$Origin*m2_treat_nod$Treatment))
#Tot P
qqnorm(TP_M2$Tot.P..µg.L.)
qqline(TP_M2$Tot.P..µg.L.)
boxplot(TP_M2$Tot.P..µg.L.~paste(TP_M2$Treatment,TP_M2$Origin))

aov(TP_M2$Tot.P..µg.L.~TP_M2$Treatment*TP_M2$Origin)
summary(aov(TP_M2$Tot.P..µg.L.~TP_M2$Treatment*TP_M2$Origin))
TukeyHSD(aov(TP_M2$Tot.P..µg.L.~TP_M2$Treatment*TP_M2$Origin))

#media charachterisation####
TP_media1 <- TP_data[64:71,]
TP_media2 <- TP_data[72:79,]

TP_media1 <- TP_media1 %>% 
  filter(Treatment!="Di")
TP_media2 <- TP_media2 %>% 
  filter(Treatment!="Di")

boxplot(TP_media1$Tot.P..µg.L.~TP_media1$Sample_ID,main="Media day 0 Microcosm 1")
boxplot(TP_media2$Tot.P..µg.L.~TP_media2$Sample_ID,main="Media day 0 Microcosm 2")

Min_env <- separate(Min_env,col = Sample.ID,
                    into = c("Sample.ID","Treatment", "Origin"),sep = "_")
Min_env_nod <- Min_env %>% 
  filter(Treatment!="Di")


Mi_env <- MI1_C[1:61,]
Min_env <- MI1_N[1:61,]
env_m1 <- cbind(Mi_env,Min_env$TN.ppm)
names(env_m1)<-c("Sample.ID","Inj","Treatment","Origin","NPOC_ppm","TN_ppm") 

Min_env_nod1 <- as.data.frame(split(Min_env_nod,Min_env_nod$Sample.ID)[1])
Min_env_nod1 <- Min_env_nod1[1:19,]
Min_env_nod2 <- as.data.frame(split(Min_env_nod,Min_env_nod$Sample.ID)[2])
Min_env_nod2 <- Min_env_nod2[4:15,]

Mi_env <- separate(Mi_env,col = Sample.ID,
                   into = c("Sample.ID","Treatment", "Origin"),sep = "_")
Mi_env_nod <- Mi_env %>% 
  filter(Treatment!="Di")
Mi_env_nod1 <- as.data.frame(split(Mi_env_nod,Mi_env_nod$Sample.ID)[1])
Mi_env_nod1 <- Mi_env_nod1[1:18,]
Mi_env_nod2 <- as.data.frame(split(Mi_env_nod,Mi_env_nod$Sample.ID)[2])
Mi_env_nod2 <- Mi_env_nod2[4:15,]
#tN MI1 media####
boxplot(Min_env_nod1$MI1.TN.ppm~paste(Min_env_nod1$MI1.Treatment,
                                      Min_env_nod1$MI1.Origin),las=2,
        xlab = " ",
        ylab = "TN.ppm",
        main="TN M1 day 0")
#NPOC media m1
boxplot(Mi_env_nod1$MI1.NPOC.ppm~paste(Mi_env_nod1$MI1.Treatment,
                                       Mi_env_nod1$MI1.Origin),las=2,
        xlab = " ",
        ylab = "NPOC.ppm",
        main="TOC M1 day 0")
#tn m2 media####
boxplot(Min_env_nod2$MI2.TN.ppm~paste(Min_env_nod2$MI2.Treatment,
                                      Min_env_nod2$MI2.Origin),las=2,
        xlab = " ",
        ylab = "TN.ppm",
        main="TN M2 day 0")
#Npoc m2 media
boxplot(Mi_env_nod2$MI2.NPOC.ppm~paste(Mi_env_nod2$MI2.Treatment,
                                       Mi_env_nod2$MI2.Origin),las=2,
        xlab = " ",
        ylab = "NPOC.ppm",
        main="TOC M2 day 0")
