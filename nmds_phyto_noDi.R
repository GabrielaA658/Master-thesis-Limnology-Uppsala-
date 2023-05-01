#NMDS phyto Milibrary(vegan)
library(tidyverse)
library(ggpubr)
library(vegan)
library(vegan3d)
library(gplots)
library(reshape2)
library(ggplot2)

m1 <- read.csv("vis_Mi1.csv")
m1_e <- as.data.frame(split(m1,m1$Timepoint)[1])
m1_s <- as.data.frame(split(m1,m1$Timepoint)[2])
names(m1_e) <- names(m1)
names(m1_s) <- names(m1)

r_m1 <- read.csv("g_vis_Mi1.csv")
r_m1 <- subset(r_m1,select = c(Timepoint,Treatment,Replicate,Origin,R.Streptophyta,R.Chlorophyta,
                               R.Cyanobacteria,R.Bacillariophyta,R.Cryptophyta,R.Heterokontophyta,
                               R.Ciliophora))
names(r_m1)<-c("Timepoint","Treatment","Replicate","Origin","Streptophyta","Chlorophyta",
               "Cyanobacteria","Bacillariophyta","Cryptophyta","Heterokontophyta",
               "Ciliophora")
m1_s_nod <- m1_s %>% 
  filter(Treatment!="Di")
m1_e_nod <- m1_e %>% 
  filter(Treatment!="Di")
m1_s_nod[,15] <- paste(m1_s_nod$Treatment,m1_s_nod$Origin) 
m1_e_nod[,15] <- paste(m1_e_nod$Treatment,m1_e_nod$Origin)

m1_s_plot <- gather(m1_s_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)
m1_e_plot <- gather(m1_e_nod,
                    species,
                    Abun,
                    Streptophyta:Ciliophora)

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

m2 <- read.csv("vis_Mi2.csv")
m2_e <- as.data.frame(split(m2,m2$Timepoint)[1])
m2_s <- as.data.frame(split(m2,m2$Timepoint)[2])
names(m2_e) <- names(m2)
names(m2_s) <- names(m2)

m2_e_nod <- m2_e %>% 
  filter(Treatment!="Di")
m2_s_nod <- m2_s %>% 
  filter(Treatment!="Di")
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

r_e2_nod <- r_e2 %>% 
  filter(Treatment!="Di")
r_s2_nod <- r_s2 %>% 
  filter(Treatment!="Di")
r_s2_plot <- gather(r_s2_nod,
                    species,
                    Abun,
                    Streptophyta:Heliozoa)
r_e2_plot <- gather(r_e2_nod,
                    species,
                    Abun,
                    Streptophyta:Heliozoa)

#NMDS####
#Abs####
m1_s_nod
m1_e_nod
m2_s_nod
m2_e_nod

species_e1 <- m1_e_nod[,8:14]
species_e2 <- m2_e_nod[,7:14]


set.seed(45)
ord <- metaMDS(species_e1,
               k=5)
ordiplot(ord,
         k=5,
         choices = c(1,2),
         type = "n",
         main="M1 - Phytoplankton community d8")
text(ord,
     choices = c(1,2),
     display = "species",
     cex=0.75)
points(ord,
       choices = c(1,2),
       pch=19,
       col=c(rep("brown3",9),rep("darkslategray",9)))
ordihull(ord,
         as.factor(m1_e_nod$Origin),
         label = F,
         choices=c(1,2),
         col = c("#333333","#66CC99","#FF6666"))
legend("bottomleft",
       legend = round(ord$stress,5),cex = 0.5,
       bg="#FFFFCC")
legend("bottomright",
       legend = c("Treatment","Add","Con"),
       col = c("white","brown3","darkslategray"),
       pch = 19,
       cex = 0.7)
legend("topleft",
       legend = c("Origin","C","D","E"),
       col = c("white","#333333","#66CC99","#FF6666"),
       pch = 12,
       cex = 0.7)
ord_2 <- metaMDS(species_e2,
               k=5)
ordiplot(ord_2,
         k=5,
         choices = c(1,2),
         type = "n",
         main="M2 - Phytoplankton community d8")
text(ord_2,
     choices = c(1,2),
     display = "species",
     cex=0.75)
points(ord_2,
       choices = c(1,2),
       pch=19,
       col=c(rep("brown3",9),rep("darkslategray",9)))
ordihull(ord_2,
         as.factor(m2_e_nod$Origin),
         label = F,
         choices=c(1,2),
         col = c("#333333","#66CC99","#FF6666"))
legend("bottomleft",
       legend = round(ord$stress,5),cex = 0.5,
       bg="#FFFFCC")
legend("bottomright",
       legend = c("Treatment","Add","Con"),
       col = c("white","brown3","darkslategray"),
       pch = 19,
       cex = 0.7)
legend("topleft",
       legend = c("Origin","C","D","E"),
       col = c("white","#333333","#66CC99","#FF6666"),
       pch = 12,
       cex = 0.7)
#rel####
rspecies_e1 <- r_e1_nod[,5:11]
rspecies_e2 <- r_e2_nod[,5:12]
ord_1rel <- metaMDS(rspecies_e1,
                 k=5)
ordiplot(ord_1rel,
         k=5,
         choices = c(1,2),
         type = "n",
         main="rM1 - Phytoplankton community d8")
text(ord_1rel,
     choices = c(1,2),
     display = "species",
     cex=0.75)
points(ord_1rel,
       choices = c(1,2),
       pch=19,
       col=c(rep("brown3",9),rep("darkslategray",9)))
ordihull(ord_1rel,
         as.factor(r_e1_nod$Origin),
         label = F,
         choices=c(1,2),
         col = c("#333333","#66CC99","#FF6666"))
legend("bottomleft",
       legend = round(ord$stress,5),cex = 0.5,
       bg="#FFFFCC")
legend("bottomright",
       legend = c("Treatment","Add","Con"),
       col = c("white","brown3","darkslategray"),
       pch = 19,
       cex = 0.7)
legend("topleft",
       legend = c("Origin","C","D","E"),
       col = c("white","#333333","#66CC99","#FF6666"),
       pch = 12,
       cex = 0.6)
ord_r2 <- metaMDS(rspecies_e2,
                 k=5)
ordiplot(ord_r2,
         k=5,
         choices = c(1,2),
         type = "n",
         main="rM2 - Phytoplankton community d8")
text(ord_r2,
     choices = c(1,2),
     display = "species",
     cex=0.75)
points(ord_r2,
       choices = c(1,2),
       pch=19,
       col=c(rep("brown3",9),rep("darkslategray",9)))
ordihull(ord_r2,
         as.factor(r_e2_nod$Origin),
         label = F,
         choices=c(1,2),
         col = c("#333333","#66CC99","#FF6666"))
legend("bottomleft",
       legend = round(ord$stress,5),cex = 0.5,
       bg="#FFFFCC")
legend("bottomright",
       legend = c("Treatment","Add","Con"),
       col = c("white","brown3","darkslategray"),
       pch = 19,
       cex = 0.7)
legend("topleft",
       legend = c("Origin","C","D","E"),
       col = c("white","#333333","#66CC99","#FF6666"),
       pch = 12,
       cex = 0.6)

#Violin####
ggviolin(m1_e_plot,
         x="V15",
         y="Abun",      
         color = "V15",
         title = "M1 Phytoplankton abundance d8",
         ylab = "Cell abundance / ml",
         xlab=" ")+
  border()+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#FF6666","#333333","#66CC99",
                                "#66CC99","#FF6666","#333333"))
ggviolin(m2_e_plot,
         x="V15",
         y="Abun",      
         color = "V15",
         title = "M2 Phytoplankton abundance d8",
         ylab = "Cell abundance / ml",
         xlab=" ")+
  border()+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#333333","#66CC99","#FF6666",
                                "#333333","#66CC99","#FF6666"))
ggviolin(r_e1_plot,
         x="V12",
         y="Abun",      
         color = "V12",
         title = "rM1 Phytoplankton abundance d8",
         ylab = "Cell abundance / ml",
         xlab=" ")+
  border()+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#333333","#66CC99","#FF6666",
                                "#333333","#66CC99","#FF6666"))   
ggviolin(r_e2_plot,
         x="V13",
         y="Abun",      
         color = "V13",
         title = "rM2 Phytoplankton abundance d8",
         ylab = "Cell abundance / ml",
         xlab=" ")+
  border()+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#333333","#66CC99","#FF6666",
                                "#333333","#66CC99","#FF6666"))   
#ABSOLUTE ABUNDANCE####
#permanova m1####
set.seed(4)
adonis2(species_e1~m1_e_nod$Origin,method = "bray",permutations = 999)
adonis2(species_e1~m1_e_nod$Treatment,method = "bray",permutations = 999)
adonis2(species_e1~m1_e_nod$Origin*m1_e_nod$Treatment,method = "bray",permutations = 999)
permanova_m1 <- adonis(species_e1~m1_e_nod$Treatment,method = "bray",permutations = 999)
coef_m1 <- coefficients(permanova_m1)["m1_e_nod$Treatment1",]
par(mar = c(4, 9, 2, 1))
barplot(sort(coef_m1),horiz = T,las=1,main = "Taxa contributting to Treatment ordination",
        xlab = "Eigenvalues")
permanova_m15 <- adonis(species_e1~m1_e_nod$V15,method = "bray",permutations = 999)
coef_m15 <- coefficients(permanova_m15)["m1_e_nod$V151",]
barplot(sort(coef_m15),horiz = T,las=1,main = "Taxa contributting to PCoA",
        xlab = "Eigenvalues")

#homogeneity of variance####
dist_e1 <- vegdist(species_e1,method = "bray")
hist(dist_e1)
qqnorm(dist_e1)
qqline(dist_e1)
set.seed(4)
variance_e1 <- betadisper(dist_e1,m1_e_nod$Origin)
plot(betadisper(dist_e1,m1_e_nod$Origin),main = "Beta diversity M1 Origin")
plot(betadisper(dist_e1,m1_e_nod$Origin,sqrt.dist = T),main = "Beta diversity M1 Origin")
anova(betadisper(dist_e1,m1_e_nod$Origin))
variance_e1t <- betadisper(dist_e1,m1_e_nod$Treatment)
plot(betadisper(dist_e1,m1_e_nod$Treatment),main = "Beta diversity M1 Treatment",
     col = c("brown3","darkslategray"))
plot(betadisper(dist_e1,m1_e_nod$Treatment,sqrt.dist = T),main = "Beta diversity M1 Treatment",
     col = c("brown3","darkslategray"))
variance_e1i <- betadisper(dist_e1,m1_e_nod$V15)
plot(betadisper(dist_e1,m1_e_nod$V15),main="Beta diversity M1 Interaction")
anova(betadisper(dist_e1,m1_e_nod$Treatment))
anova(betadisper(dist_e1,m1_e_nod$V15))

labs_e1 <- paste("Dimension",1:4,"(",
                 round(100*variance_e1$eig/sum(variance_e1$eig),2),"%)")
plot(variance_e1, cex = 0.7,pch = 15:17,
     main = "", cex.lab=1,
     xlab = labs_e1[1],ylab = labs_e1[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#66CC99","#FF6666"))
title("PCoA M1 Grouped by Origin",adj=0,cex=0.2)
labs_e1t <- paste("Dimension",1:4,"(",
                  round(100*variance_e1t$eig/sum(variance_e1t$eig),2),"%)")
plot(variance_e1t, cex = 0.5,pch = 15:17,main = "",
     cex.lab=1,
     xlab = labs_e1t[1],ylab = labs_e1t[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#FF6666"))
title("by Treatment",adj=0,cex=0.5)
title("PCoA Second disturbance Day8", line=3,adj=0,cex=0.7)
par(mfrow=c(1,1))
labs_e1i <- paste("Dimension",1:2,"(",
                  round(100*variance_e1i$eig/sum(variance_e1i$eig),2),"%)")
plot(variance_e1i, cex = 0.7,pch = 15:17,
     main = "", cex.lab=1,
     xlab = labs_e1i[1],ylab = labs_e1i[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#66CC99","#FF6666"))
title("PCoA Absolute abundance",cex=0.5,line = 2,adj=0.5)

set.seed(4)
permutest(betadisper(dist_e1,m1_e_nod$Origin),pairwise=T)
permutest(betadisper(dist_e1,m1_e_nod$Treatment),pairwise=T)
permutest(betadisper(dist_e1,m1_e_nod$V15),pairwise=T,permutations = 999)
permutest(betadisper(dist_e1,m1_e_nod$V15,sqrt.dist = T),pairwise=T,permutations = 999)

TukeyHSD(betadisper(dist_e1,m1_e_nod$Origin))
TukeyHSD(betadisper(dist_e1,m1_e_nod$Treatment))
TukeyHSD(betadisper(dist_e1,m1_e_nod$V15,sqrt.dist = T))

#PCoA####
pcoa_ord_m1 <- capscale(species_e1~1)
screeplot(pcoa_ord_m1,type="l")
eigenvals(pcoa_ord_m1) %>% summary()->evm1
plot(pcoa_ord_m1,display="sites",main="Multivariate dispersion/M1/Origin")
ordispider(pcoa_ord_m1,m1_e_nod$Origin,display = "sites",
           col = c("#333333","#66CC99","#FF6666"))
legend("topleft",legend = c("Origin","E","C","D"),pch = 19,
       col = c("white","#FF6666","#333333","#66CC99"),cex = 0.7)
plot(pcoa_ord_m1,display="sites",main="Multivariate dispersion/M1/Treatment")
ordispider(pcoa_ord_m1,m1_e_nod$Treatment,display = "sites",
           col = c("brown3","darkslategray"))
legend("topleft",legend = c("Treatment","Add","Con"),pch = 19,
       col = c("white","brown3","darkslategray"),cex = 0.7)
#
plot(capscale(m1_s_nod[,8:14]~1),display="sites")
points(pcoa_ord_m1,display="sites",col="red")
#permanova m2####
set.seed(4)
adonis2(species_e2~m2_e_nod$Origin,method = "bray",permutations = 999)
adonis2(species_e2~m2_e_nod$Treatment,method = "bray",permutations = 999)
adonis2(species_e2~m2_e_nod$Origin*m2_e_nod$Treatment,method = "bray",permutations = 999)
permanova_m2 <- adonis(species_e2~m2_e_nod$V15,method = "bray",permutations = 999)
coef_m2 <- coefficients(permanova_m2)["m2_e_nod$V151",]
par(mar = c(4, 9, 2, 1))
barplot(sort(coef_m2),horiz = T,las=1,main = "Taxa contributting to PCoA",
        xlab="Eigenvalues")

#homogeneity of variance####
dist_e2 <- vegdist(species_e2,method = "bray")
hist(dist_e2)
qqnorm(dist_e2)
qqline(dist_e2)

variance_e2 <- betadisper(dist_e2,m2_e_nod$Origin)
variance_e2t <- betadisper(dist_e2,m2_e_nod$Treatment)
variance_e2i<- betadisper(dist_e2,m2_e_nod$V15)
plot(betadisper(dist_e2,m2_e_nod$Origin),main = "Beta diversity M2 Origin")
anova(betadisper(dist_e2,m2_e_nod$Origin))
plot(betadisper(dist_e2,m2_e_nod$Treatment),main = "Beta diversity M2 Treatment",
     col = c("brown3","darkslategray"))
plot(betadisper(dist_e2,m2_e_nod$V15),main="Beta diversity M2 Interaction")
anova(betadisper(dist_e2,m2_e_nod$Treatment))
anova(betadisper(dist_e2,m2_e_nod$V15))

labs_e2 <- paste("Dimension",1:4,"(",
                 round(100*variance_e2$eig/sum(variance_e2$eig),2),"%)")
plot(variance_e2, cex = 0.7,pch = 15:17,
     main = "MDS coordinates Legacy effects Day8", cex.lab=1,
     xlab = labs_e2[1],ylab = labs_e2[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#66CC99","#FF6666"))
labs_e2t <- paste("Dimension",1:4,"(",
                 round(100*variance_e2t$eig/sum(variance_e2t$eig),2),"%)")
plot(variance_e2t, cex = 0.7,pch = 15:17,
     main = "", cex.lab=1,
     xlab = labs_e2t[1],ylab = labs_e2t[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#FF6666","#333333"))
title("PCoA Legacy effects Day 8")

labs_e2i <- paste("Dimension",1:4,"(",
                 round(100*variance_e2i$eig/sum(variance_e2i$eig),2),"%)")
plot(variance_e2i, cex = 0.7,pch = 15:17,
     main = "", cex.lab=1,
     xlab = labs_e2i[1],ylab = labs_e2i[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#66CC99","#FF6666"))
title("PCoA Absolute abundance")

permutest(betadisper(dist_e2,m2_e_nod$Origin),pairwise=T)
permutest(betadisper(dist_e2,m2_e_nod$Treatment),pairwise=T)
permutest(betadisper(dist_e2,m2_e_nod$V15),pairwise=T)

#PCoA####
pcoa_ord_m2 <- capscale(species_e2~1)
screeplot(pcoa_ord_m2,type="l")
eigenvals(pcoa_ord_m2) %>% summary()->evm2
plot(pcoa_ord_m2,display="sites",main="Multivariate dispersion/M2/Origin")
ordispider(pcoa_ord_m2,m2_e_nod$Origin,display = "sites",
           col = c("#333333","#66CC99","#FF6666"))
legend("bottomleft",legend = c("Origin","E","C","D"),pch = 19,
       col = c("white","#FF6666","#333333","#66CC99"),cex = 0.7)
plot(pcoa_ord_m2,display="sites",main="Multivariate dispersion/M2/Treatment")
ordispider(pcoa_ord_m2,m2_e_nod$Treatment,display = "sites",
           col = c("brown3","darkslategray"))
legend("bottomleft",legend = c("Treatment","Add","Con"),pch = 19,
       col = c("white","brown3","darkslategray"),cex = 0.7)

#RELATIVE ABUNDANCE####
r_e1_nod$V12 <- paste(r_e1_nod$Treatment,r_e1_nod$Origin)
r_e2_nod$V12 <- paste(r_e2_nod$Treatment,r_e2_nod$Origin)
rspecies_e1 <- r_e1_nod[,5:11]
rspecies_e2 <- r_e2_nod[,5:12]
#permanova rm1####
set.seed(41)
adonis2(rspecies_e1~r_e1_nod$Origin,method = "bray",permutations = 999)
adonis2(rspecies_e1~r_e1_nod$Treatment,method = "bray",permutations = 999)
adonis2(rspecies_e1~r_e1_nod$Origin*r_e1_nod$Treatment,method = "bray",permutations = 999)
permanova_m1r <- adonis(rspecies_e1~r_e1_nod$V12,method = "bray",permutations = 999)
rcoef_m1 <- coefficients(permanova_m1r)["r_e1_nod$V121",]
par(mar = c(3, 9, 2, 1))
barplot(sort(rcoef_m1),horiz = T,las=1,main = "Taxa contributing to PCoA")

#homogeneity of variance####
rdist_e1 <- vegdist(rspecies_e1,method = "bray")
hist(rdist_e1)
qqnorm(rdist_e1)
qqline(rdist_e1)#not really normally distributed

variance_rm1o <- betadisper(rdist_e1,r_e1_nod$Origin)
variance_rm1t <- betadisper(rdist_e1,r_e1_nod$Treatment)
variance_rm1i <- betadisper(rdist_e1,r_e1_nod$V12)
plot(betadisper(rdist_e1,r_e1_nod$Origin),main = "Beta diversity rM1 Origin")
anova(betadisper(rdist_e1,r_e1_nod$Origin))
plot(betadisper(rdist_e1,r_e1_nod$Treatment),main = "Beta diversity rM1 Treatment",
     col = c("brown3","darkslategray"))
plot(betadisper(rdist_e1,r_e1_nod$V12),main="Beta diversity rM1 Interaction")
anova(betadisper(rdist_e1,r_e1_nod$Treatment))
anova(betadisper(rdist_e1,r_e1_nod$V12))

labs_rm1o <- paste("Dimension",1:2,"(",
                   round(100*variance_rm1o$eig/sum(variance_rm1o$eig),2),"%)")
plot(variance_rm1o, cex = 0.7,pch = 15:17,
     main = "", cex.lab=1,
     xlab = labs_rm1o[1],ylab = labs_rm1o[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#66CC99","#FF6666"))
title("PCoA Second disturbance Day 8 relative abundance",cex=0.5,line = 2,adj=0.5)

labs_rm1t <- paste("Dimension",1:2,"(",
                   round(100*variance_rm1t$eig/sum(variance_rm1t$eig),2),"%)")
plot(variance_rm1t, cex = 0.7,pch = 15:17,
     main = "", cex.lab=1,
     xlab = labs_rm1t[1],ylab = labs_rm1t[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#FF6666","#333333"))
title("PCoA Second disturbance Day 8 relative abundance",cex=0.5,line = 2,adj=0.5)

labs_rm1i <- paste("Dimension",1:2,"(",
                   round(100*variance_rm1i$eig/sum(variance_rm1i$eig),2),"%)")
plot(variance_rm1i, cex = 0.7,pch = 15:17,
     main = "", cex.lab=1,
     xlab = labs_rm1i[1],ylab = labs_rm1i[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#66CC99","#FF6666"))
title("PCoA Relative abundance",cex=0.5,line = 2,adj=0.5)

permutest(betadisper(rdist_e1,r_e1_nod$Origin),pairwise=T)
permutest(betadisper(rdist_e1,r_e1_nod$Treatment),pairwise=T)
permutest(betadisper(rdist_e1,r_e1_nod$V12),pairwise=T)

#PCoA####
rpcoa_ord_m1 <- capscale(rspecies_e1~1)
screeplot(rpcoa_ord_m1,type="l")
eigenvals(rpcoa_ord_m1) %>% summary()->evm1r
plot(rpcoa_ord_m1,display="sites",main="Multivariate dispersion/rM1/Origin")
ordispider(rpcoa_ord_m1,r_e1_nod$Origin,display = "sites",
           col = c("#333333","#66CC99","#FF6666"))
legend("topleft",legend = c("Origin","E","C","D"),pch = 19,
       col = c("white","#FF6666","#333333","#66CC99"),cex = 0.7)
plot(rpcoa_ord_m1,display="sites",main="Multivariate dispersion/rM1/Treatment")
ordispider(rpcoa_ord_m1,r_e1_nod$Treatment,display = "sites",
           col = c("brown3","darkslategray"))
legend("topleft",legend = c("Treatment","Add","Con"),pch = 19,
       col = c("white","brown3","darkslategray"),cex = 0.7)

#permanova rm2####
set.seed(41)
adonis2(rspecies_e2~r_e2_nod$Origin,method = "bray",permutations = 999)
adonis2(rspecies_e2~r_e2_nod$Treatment,method = "bray",permutations = 999)
adonis2(rspecies_e2~r_e2_nod$Origin*r_e2_nod$Treatment,method = "bray",permutations = 999)
permanova_m2r <- adonis(rspecies_e2~r_e2_nod$Treatment,method = "bray",permutations = 999)
coef_m2r <- coefficients(permanova_m2r)["r_e2_nod$Treatment1",]
par(mar = c(3, 9, 2, 1))
barplot(sort(coef_m2r),horiz = T,las=1,main = "Top taxa rM2 Treatment")
permanova_m2ri <- adonis(rspecies_e2~r_e2_nod$V12,method = "bray",permutations = 999)
coef_m2ri <- coefficients(permanova_m2ri)["r_e2_nod$V121",]
barplot(sort(coef_m2ri),horiz = T,las=1,main = "Taxa contributing to PCoA")

#homogeneity of variance####
rdist_e2 <- vegdist(rspecies_e2,method = "bray")
hist(rdist_e2)
qqnorm(rdist_e2)
qqline(rdist_e2)

variance_me2r <- betadisper(rdist_e2,r_e2_nod$Treatment)
variance_me2o <- betadisper(rdist_e2,r_e2_nod$Origin)
variance_me2i <- betadisper(rdist_e2,r_e2_nod$V12)
plot(betadisper(rdist_e2,r_e2_nod$Origin),main = "Beta diversity rM2 Origin")
anova(betadisper(rdist_e2,r_e2_nod$Origin))
plot(betadisper(rdist_e2,r_e2_nod$Treatment),main = "Beta diversity rM2 Treatment",
     col = c("brown3","darkslategray"))
plot(betadisper(rdist_e2,r_e2_nod$V13),main="Beta diversity rM2 Interaction")
anova(betadisper(rdist_e2,r_e2_nod$Treatment))
anova(betadisper(rdist_e2,r_e2_nod$V12))
par(mfrow=c(1,1))
labs_me2r <- paste("Dimension",1:2,"(",
                    round(100*variance_me2r$eig/sum(variance_me2r$eig),2),"%)")
plot(variance_me2r, cex = 0.7,pch = 15:17,
     main = "", cex.lab=1,
     xlab = labs_me2r[1],ylab = labs_me2r[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#FF6666"))
title("PCoA Legacy Effects Day 8 relative abundance",cex=0.5,line = 2,adj=0.5)

labs_me2o <- paste("Dimension",1:2,"(",
                   round(100*variance_me2o$eig/sum(variance_me2o$eig),2),"%)")
plot(variance_me2o, cex = 0.7,pch = 15:17,
     main = "", cex.lab=1,
     xlab = labs_me2o[1],ylab = labs_me2o[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#66CC99","#FF6666"))
title("PCoA Legacy Effects Day 8 relative abundance",cex=0.5,line = 2,adj=0.5)

labs_me2i <- paste("Dimension",1:2,"(",
                   round(100*variance_me2i$eig/sum(variance_me2i$eig),2),"%)")
plot(variance_me2i, cex = 0.7,pch = 15:17,
     main = "", cex.lab=1,
     xlab = labs_me2i[1],ylab = labs_me2i[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#66CC99","#FF6666"))
title("PCoA Relative abundance",cex=0.5,line = 2,adj=0.5)
permutest(betadisper(rdist_e2,r_e2_nod$Origin),pairwise=T)
permutest(betadisper(rdist_e2,r_e2_nod$Treatment),pairwise=T)
permutest(betadisper(rdist_e2,r_e2_nod$V12),pairwise=T)

#PCoA####
rpcoa_ord_m2 <- capscale(rspecies_e2~1)
screeplot(rpcoa_ord_m2,type="l")
eigenvals(rpcoa_ord_m2) %>% summary()->evrm2
plot(rpcoa_ord_m2,display="sites",main="Multivariate dispersion/rM2/Origin")
ordispider(rpcoa_ord_m2,r_e2_nod$Origin,display = "sites",
           col = c("#333333","#66CC99","#FF6666"))
legend("bottomleft",legend = c("Origin","E","C","D"),pch = 19,
       col = c("white","#FF6666","#333333","#66CC99"),cex = 0.7)
plot(rpcoa_ord_m2,display="sites",main="Multivariate dispersion/rM2/Treatment")
ordispider(rpcoa_ord_m2,r_e2_nod$Treatment,display = "sites",
           col = c("brown3","darkslategray"))
legend("bottomleft",legend = c("Treatment","Add","Con"),pch = 19,
       col = c("white","brown3","darkslategray"),cex = 0.7)

#GENUS####
ord_genus_m1 <- read.csv("D:/UPPSALA/Master_thesis/Results/PC_countings/Data_MI/raw-vis_Mi1_ordination.csv",
                     sep = ",")
ord_genus_m2 <- read.csv("D:/UPPSALA/Master_thesis/Results/PC_countings/Data_MI/raw-vis_Mi2_ordination.csv",
                     sep = ",")
#M1####
#split by start/end
start_genus_m1 <- as.data.frame(split(ord_genus_m1[,1:37],ord_genus_m1$Timepoint)[2])
names(start_genus_m1) <- names(ord_genus_m1[,1:37])
end_genus_m1 <- as.data.frame(split(ord_genus_m1[,1:37],ord_genus_m1$Timepoint)[1])
names(end_genus_m1) <- names(ord_genus_m1[,1:37])

#Permanova####
set.seed(76)
end_genus_m1[,38] <- paste(end_genus_m1$Treatment,end_genus_m1$Origin)
adonis2(end_genus_m1[,10:37]~end_genus_m1$Origin,method = "bray",permutations = 999)
adonis2(end_genus_m1[,10:37]~end_genus_m1$Treatment,method = "bray",permutations = 999)
adonis2(end_genus_m1[,10:37]~end_genus_m1$Treatment*end_genus_m1$Origin,method = "bray",permutations = 999)
permanova_m1_ge <- adonis(end_genus_m1[,10:37]~end_genus_m1$V38,
                          method = "bray",permutations = 999)
coef_m1_ge <- coefficients(permanova_m1_ge)["end_genus_m1$V381",]
par(mar = c(3, 9, 2, 1))
barplot(rev(sort(coef_m1_ge))[1:6],horiz = T,las=1,
        main = "Top contributing Genus",
        xlim = c(0,30),
        col = c("#CC3333","#CC3333","#FF00FF","#FF9933","#CC3333","#FFCC33"),
        names.arg = c("Fragilaria","Cyclotella","Mesodinium",
                      "Rhodomonas","Acanthoceras","Mallomonas"),
        font=3)


#PCoA####
pcoa_ord_m1_genus <- capscale(end_genus_m1[,10:37]~1)
plot(pcoa_ord_m1_genus,display="sites",
     main="Multivariate dispersion/M1 Genus/Origin")#row number
plot(pcoa_ord_m1_genus,type="n",
     main="Multivariate dispersion/M1 Genus/Origin")
points(pcoa_ord_m1_genus,display = "sites",pch=c(rep(19,9),rep(15,9)),
     col=c(rep("#FF6666",9),rep("#333333",9)),cex=0.7)
ordihull(pcoa_ord_m1_genus,
         as.factor(end_genus_m1$Origin),
         col = c("#333333","#66CC99","#FF6666"))
legend("bottomleft",legend = c("Origin","E","C","D"),pch = 12,
       col = c("white","#FF6666","#333333","#66CC99"),cex = 0.7)
legend("bottomright",legend = c("Treatment","Add","Con"),pch = c(10,19,15),
       col = c("white","#FF6666","#666666"),cex = 0.7)
#treatment
plot(pcoa_ord_m1_genus,type="n",
     main="PCoA M1 Genus")
points(pcoa_ord_m1_genus,display = "sites",pch=c(rep(19,9),rep(15,9)),
       col=c(rep("#FF6666",9),rep("#333333",9)),cex=0.7)
ordihull(pcoa_ord_m1_genus,
         as.factor(end_genus_m1$Treatment),
         col = c("#FF6666","#333333"))
legend("bottomright",legend = c("Treatment","Add","Con"),pch = c(10,19,15),
       col = c("white","#FF6666","#666666"),cex = 0.7)
ordispider(pcoa_ord_m1_genus,end_genus_m1$Origin,display = "sites",
           col = c("#666666","#66CC99","#FF6666"))
legend("bottomleft",legend = c("Origin","E","C","D"),pch = 19,
       col = c("white","#FF6666","#333333","#66CC99"),cex = 0.7)
#homogeneity####

ge_dist_e1 <- vegdist(end_genus_m1[,10:37],method = "bray")
hist(ge_dist_e1)
qqnorm(ge_dist_e1)
qqline(ge_dist_e1)

betadisper(ge_dist_e1,end_genus_m1$Origin)
plot(betadisper(ge_dist_e1,end_genus_m1$Origin),
     main = "Beta diversity M1 Origin",
     col = c("#333333","#66CC99","#FF6666"))
anova(betadisper(ge_dist_e1,end_genus_m1$Origin))
plot(betadisper(ge_dist_e1,end_genus_m1$Treatment),main = "Beta diversity M1 Treatment",
     col = c("brown3","darkslategray"))
plot(betadisper(ge_dist_e1,end_genus_m1$V38),main="Beta diversity M1")
anova(betadisper(ge_dist_e1,end_genus_m1$Treatment))
anova(betadisper(ge_dist_e1,end_genus_m1$V38))

permutest(betadisper(ge_dist_e1,end_genus_m1$Origin),pairwise=T)
permutest(betadisper(ge_dist_e1,end_genus_m1$Treatment),pairwise=T)
permutest(betadisper(ge_dist_e1,end_genus_m1$V38),pairwise=T)

#M2####
start_genus_m2 <- as.data.frame(split(ord_genus_m2[,1:48],ord_genus_m2$Timepoint)[2])
names(start_genus_m2) <- names(ord_genus_m2[,1:48])
end_genus_m2 <- as.data.frame(split(ord_genus_m2[,1:48],ord_genus_m2$Timepoint)[1])
names(end_genus_m2) <- names(ord_genus_m2[,1:48])

#Permanova####
set.seed(76)
end_genus_m2[,49] <- paste(end_genus_m2$Treatment,end_genus_m2$Origin)
adonis2(end_genus_m2[,10:48]~end_genus_m2$Origin,method = "bray",permutations = 999)
adonis2(end_genus_m2[,10:48]~end_genus_m2$Treatment,method = "bray",permutations = 999)
adonis2(end_genus_m2[,10:48]~end_genus_m2$Treatment*end_genus_m2$Origin,method = "bray",permutations = 999)
permanova_m2_ge <- adonis(end_genus_m2[,10:48]~end_genus_m2$V49,
                          method = "bray",permutations = 999)
coef_m2_ge <- coefficients(permanova_m2_ge)["end_genus_m2$V491",]
par(mar = c(3, 9, 2, 1))
barplot(rev(sort(coef_m2_ge))[1:6],horiz = T,las=1,
        main = "Top contributing Genus",
        xlim = c(0,6),
        col = c("#CC3333","#66FFFF","#FF00FF","#330033","#CC3333",
                "#99FF99"),
        names.arg = c("Fragilaria","Dolichospermum2","Ciliates",
                      "Heliozoa","Cyclotella","Chlorophyceae"),font=3)

#PCoA####
pcoa_ord_m2_genus <- capscale(end_genus_m2[,10:48]~1)
plot(pcoa_ord_m2_genus,display="sites",
     main="Multivariate dispersion/M2 Genus/Origin")#row number
plot(pcoa_ord_m2_genus,type="n",
     main="Multivariate dispersion/M2 Genus/Origin")
points(pcoa_ord_m2_genus,display = "sites",pch=c(rep(19,9),rep(15,9)),
       col=c(rep("#FF6666",9),rep("#333333",9)),cex=0.7)
ordihull(pcoa_ord_m2_genus,
         as.factor(end_genus_m2$Origin),
         col = c("#333333","#66CC99","#FF6666"))
legend("bottomleft",legend = c("Origin","E","C","D"),pch = 12,
       col = c("white","#FF6666","#333333","#66CC99"),cex = 0.7)
legend("bottomright",legend = c("Treatment","Add","Con"),pch = c(10,19,15),
       col = c("white","#FF6666","#666666"),cex = 0.7)
#treatment
plot(pcoa_ord_m2_genus,type="n",
     main="PCoA M2 Genus")
points(pcoa_ord_m2_genus,display = "sites",pch=c(rep(19,9),rep(15,9)),
       col=c(rep("#FF6666",9),rep("#333333",9)),cex=0.7)
ordihull(pcoa_ord_m2_genus,
         as.factor(end_genus_m2$Treatment),
         col = c("#FF6666","#333333"))
legend("bottomright",legend = c("Treatment","Add","Con"),pch = c(10,19,15),
       col = c("white","#FF6666","#666666"),cex = 0.7)
ordispider(pcoa_ord_m2_genus,end_genus_m2$Origin,display = "sites",
           col = c("#666666","#66CC99","#FF6666"))
legend("bottomleft",legend = c("Origin","E","C","D"),pch = 12,
       col = c("white","#FF6666","#333333","#66CC99"),cex = 0.7)
#homogeneity####

ge_dist_e2 <- vegdist(end_genus_m2[,10:48],method = "bray")
hist(ge_dist_e2)
qqnorm(ge_dist_e2)
qqline(ge_dist_e2)

betadisper(ge_dist_e2,end_genus_m2$Origin)
plot(betadisper(ge_dist_e2,end_genus_m2$Origin),
     main = "Beta diversity M2 Origin",
     col = c("#333333","#66CC99","#FF6666"))
anova(betadisper(ge_dist_e2,end_genus_m2$Origin))
plot(betadisper(ge_dist_e2,end_genus_m2$Treatment),main = "Beta diversity M2 Treatment",
     col = c("brown3","darkslategray"))
plot(betadisper(ge_dist_e2,end_genus_m2$V49),main="Beta diversity M2")
anova(betadisper(ge_dist_e2,end_genus_m2$Treatment))
anova(betadisper(ge_dist_e2,end_genus_m2$V49))

permutest(betadisper(ge_dist_e2,end_genus_m2$Origin),pairwise=T)
permutest(betadisper(ge_dist_e2,end_genus_m2$Treatment),pairwise=T)
permutest(betadisper(ge_dist_e2,end_genus_m2$V49),pairwise=T)

#relative genus####
relative_genus_m1 <- ord_genus_m1[,40:67]
relative_genus_m1[,29] <- end_genus_m1$V38
permanova_rm1_ge <- adonis(relative_genus_m1[,1:28]~relative_genus_m1$V29,
                          method = "bray",permutations = 999)
coef_rm1_ge <- coefficients(permanova_rm1_ge)["relative_genus_m1$V291",]
par(mar = c(3, 9, 2, 1))
barplot(rev(sort(coef_rm1_ge))[1:6],horiz = T,las=1)
#by origin
permanova_rom1_ge <- adonis(relative_genus_m1[,1:28]~ord_genus_m1$Origin,
                           method = "bray",permutations = 999)
coef_rom1_ge <- coefficients(permanova_rom1_ge)["ord_genus_m1$Origin1",]
par(mar = c(3, 9, 2, 1))
barplot(rev(sort(coef_rom1_ge))[1:6],horiz = T,las=1)

#M2####
relative_genus_m2 <- ord_genus_m2[,51:89]
relative_genus_m2[,40] <- paste(ord_genus_m2$Treatment,ord_genus_m2$Origin)
permanova_rm2_ge <- adonis(relative_genus_m2[,1:39]~relative_genus_m2$V40,
                           method = "bray",permutations = 999)
coef_rm2_ge <- coefficients(permanova_rm2_ge)["relative_genus_m2$V401",]
par(mar = c(3, 9, 2, 1))
barplot(rev(sort(coef_rm2_ge))[1:6],horiz = T,las=1)

#by origin
permanova_rom2_ge <- adonis(relative_genus_m2[,1:39]~ord_genus_m2$Origin,
                           method = "bray",permutations = 999)
coef_rom2_ge <- coefficients(permanova_rom2_ge)["ord_genus_m2$Origin1",]
par(mar = c(3, 9, 2, 1))
barplot(rev(sort(coef_rom2_ge))[1:6],horiz = T,las=1)

#paired wilcoxon test for Hypothesis1####
#m1
m1_s_nod$Origin <- as.factor(m1_s_nod$Origin)
m1_originC <- as.data.frame(split(m1_s_nod,m1_s_nod$Origin)[1])
m1_originD <- as.data.frame(split(m1_s_nod,m1_s_nod$Origin)[2])
m1_originE <- as.data.frame(split(m1_s_nod,m1_s_nod$Origin)[3])

m1_e_nod$Origin <- as.factor(m1_e_nod$Origin)
m1_end_originC <- as.data.frame(split(m1_e_nod,m1_e_nod$Origin)[1])
m1_end_originD <- as.data.frame(split(m1_e_nod,m1_e_nod$Origin)[2])
m1_end_originE <- as.data.frame(split(m1_e_nod,m1_e_nod$Origin)[3])

#separated by origin
test_originC <- as.data.frame(cbind(rep("start",6),
                                    m1_originC$C.Treatment,
                                    rowSums(m1_originC[,8:14])))

test_originC <- as.data.frame(rbind(test_originC,
                                    cbind(rep("end",6),
                                          m1_end_originC$C.Treatment,
                                          rowSums(m1_end_originC[,8:14]))))
test_originC$V3 <- as.numeric(test_originC$V3)
test_originD <- as.data.frame(rbind(cbind(rep("start",6),
                                    m1_originD$D.Treatment,
                                    rowSums(m1_originD[,8:14])),
                              cbind(rep("end",6),
                                    m1_end_originD$D.Treatment,
                                    rowSums(m1_end_originD[,8:14]))))
test_originD$V3 <- as.numeric(test_originD$V3)
test_originE <- as.data.frame(rbind(cbind(rep("start",6),
                                    m1_originE$E.Treatment,
                                    rowSums(m1_originE[,8:14])),
                              cbind(rep("end",6),
                                    m1_end_originE$E.Treatment,
                                    rowSums(m1_end_originE[,8:14]))))
test_originE$V3 <- as.numeric(test_originE$V3)
boxplot(test_originC$V3~test_originC$V1)
wilcox.test(test_originC$V3~test_originC$V1,
            paired=T,alternative="g")
boxplot(test_originD$V3~test_originD$V1)
wilcox.test(test_originD$V3~test_originD$V1,
            paired=T,alternative="g")
boxplot(test_originE$V3~test_originE$V1)
wilcox.test(test_originE$V3~test_originE$V1,
            paired=T,alternative="g")

#still paired wilcox test but only on the END comm
rowSums(m1_end_originC[,8:14])
boxplot(rowSums(m1_end_originC[,8:14])~m1_end_originC$C.Treatment)
wilcox.test(rowSums(m1_end_originC[,8:14])~m1_end_originC$C.Treatment,
            paired=T,alternative="g")
#wilcox.test(test_originC$V2~test_originC$V3,
#            paired=T,alternative="g")
boxplot(rowSums(m1_end_originD[,8:14])~m1_end_originD$D.Treatment)
wilcox.test(rowSums(m1_end_originD[,8:14])~m1_end_originD$D.Treatment,
            paired=T,alternative="g")

boxplot(rowSums(m1_end_originE[,8:14])~m1_end_originE$E.Treatment)
wilcox.test(rowSums(m1_end_originE[,8:14])~m1_end_originE$E.Treatment,
            paired=T,alternative="g")
#m2
species_e2[,9] <- rowSums(species_e2)

m2_s_nod$Origin <- as.factor(m2_s_nod$Origin)
m2_originC <- as.data.frame(split(m2_s_nod,m2_s_nod$Origin)[1])
m2_originD <- as.data.frame(split(m2_s_nod,m2_s_nod$Origin)[2])
m2_originE <- as.data.frame(split(m2_s_nod,m2_s_nod$Origin)[3])
#remove replicate 3 from addition treatment, origin E since paired control was lost
m2_originE <-m2_originE[-3,] 

m2_e_nod$Origin <- as.factor(m2_e_nod$Origin)
m2_end_originC <- as.data.frame(split(m2_e_nod,m2_e_nod$Origin)[1])
m2_end_originD <- as.data.frame(split(m2_e_nod,m2_e_nod$Origin)[2])
m2_end_originE <- as.data.frame(split(m2_e_nod,m2_e_nod$Origin)[3])
#remove replicate 3 from addition treatment, origin E since paired control was lost
m2_end_originE <-m2_end_originE[-3,] 

#separated by origin too
test_originC_2 <- as.data.frame(rbind(
                                    cbind(rep("start",6),
                                    m2_originC$C.Treatment,
                                    rowSums(m2_originC[,7:14])),
                                    cbind(rep("end",6),
                                          m2_end_originC$C.Treatment,
                                          rowSums(m2_end_originC[,7:14]))))
test_originC_2$V3 <- as.numeric(test_originC_2$V3)
test_originD_2 <- as.data.frame(rbind(
                                    cbind(rep("start",6),
                                    m2_originD$D.Treatment,
                                    rowSums(m2_originD[,7:14])),
                                    cbind(rep("end",6),
                                    m2_end_originD$D.Treatment,
                                    rowSums(m2_end_originD[,7:14]))))
test_originD_2$V3 <- as.numeric(test_originD_2$V3)
test_originE_2 <- as.data.frame(rbind(
                                    cbind(rep("start",4),
                                    m2_originE$E.Treatment,
                                    rowSums(m2_originE[,7:14])),
                                    cbind(rep("end",4),
                                    m2_end_originE$E.Treatment,
                                    rowSums(m2_end_originE[,7:14]))))
test_originE_2$V3 <- as.numeric(test_originE_2$V3)
boxplot(test_originC_2$V3~test_originC_2$V1)
wilcox.test(test_originC_2$V3~test_originC_2$V1,
            paired=T,alternative="g")
boxplot(test_originD_2$V3~test_originD_2$V1)
wilcox.test(test_originD_2$V3~test_originD_2$V1,
            paired=T,alternative="g")
boxplot(test_originE_2$V3~test_originE_2$V1)
wilcox.test(test_originE_2$V3~test_originE_2$V1,
            paired=T,alternative="g")
boxplot(test_originE_2$V3~paste(test_originE_2$V2,test_originE_2$V1))

#only for END communities. M2
boxplot(rowSums(m2_end_originC[,7:14])~m2_end_originC$C.Treatment)
wilcox.test(rowSums(m2_end_originC[,7:14])~m2_end_originC$C.Treatment,
            paired=T,alternative="g")

boxplot(rowSums(m2_end_originD[,7:14])~m2_end_originD$D.Treatment)
wilcox.test(rowSums(m2_end_originD[,7:14])~m2_end_originD$D.Treatment,
            paired=T,alternative="g")
m2_end_originE<- m2_end_originE[c(3,4,2,1),]
boxplot(rowSums(m2_end_originE[,7:14])~m2_end_originE$E.Treatment)
wilcox.test(rowSums(m2_end_originE[,7:14])~m2_end_originE$E.Treatment,
            paired=T,alternative="g")
