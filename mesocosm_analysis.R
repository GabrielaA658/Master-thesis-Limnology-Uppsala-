#Mesocosm data Analysis
library(vegan)
library(tidyverse)
library(ggpubr)
library(multcompView)
library(vegan3d)
library(gplots)
library(reshape2)
library(ggplot2)


msc <- read.csv("visualisation_400_countings.csv")
grouped_data <- msc[,47:73]
abs_abun <- grouped_data[,1:13]
abs_abun[,14] <- paste(abs_abun$Treatment,abs_abun$Sample,sep = "-")
names(abs_abun) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                     "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                     "Euglenozoa","Streptophyta","Int")
abs_abun_list <- split(abs_abun,abs_abun$Sample)
abs_abun_00 <- as.data.frame(abs_abun_list[1])
names(abs_abun_00) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                        "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                        "Euglenozoa","Streptophyta","Int")
abs_abun_03 <- as.data.frame(abs_abun_list[2])
names(abs_abun_03) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                        "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                        "Euglenozoa","Streptophyta","Int")
abs_abun_05 <- as.data.frame(abs_abun_list[3])
names(abs_abun_05) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                        "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                        "Euglenozoa","Streptophyta","Int")
abs_abun_09 <- as.data.frame(abs_abun_list[4])
names(abs_abun_09) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                        "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                        "Euglenozoa","Streptophyta","Int")
#permanova####
set.seed(5)
adonis2(abs_abun[,5:13]~abs_abun$Treatment,
        method = "bray",permutations = 999)
adonis2(abs_abun[,5:13]~abs_abun$Sample,
        method = "bray",permutations = 999)
#checking homogeneity condition
dist <- vegdist(abs_abun[,5:13],method = "bray")
hist(dist)

par(mfrow=c(1,1))
variance_all <- betadisper(dist,abs_abun$Sample)
plot(betadisper(dist,abs_abun$Treatment),main = "Multivariate disperson / all data",
     ellipse = F,hull = T,xlab = "PCoA 1")
plot(betadisper(dist,abs_abun$Sample),main = "Mesocosm ordination",
     ellipse = F,hull = T,xlab = "PCoA 1",col = c("orange","purple","brown","gray"))
labs <- paste("Dimension",1:4,"(",
                round(100*variance_all$eig/sum(variance_all$eig),2),"%)")
plot(variance_all, cex = 0.7,pch = 15:17,
     main = "PCoA for mesocosm experiment by experimental day", cex.lab=1,
     xlab = labs[1],ylab = labs[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("orange","purple","brown","gray"))

plot(betadisper(dist,abs_abun$Int),main = "Multivariate disperson / all data")
#PCoA####
ord <- capscale(abs_abun[,5:13]~1)
screeplot(ord,type="l")
eigenvals(ord) %>% summary()->ev
plot(ord,main="Multivariate dispersion/all data/sample",type="n")
points(ord,display="sites", pch=c(rep(19,15),rep(15,16),rep(17,16),rep(12,16)),
       cex=0.7)
legend("bottomleft",legend = c("S0","S3","S5","S9"),
       pch = c(19,15,17,12))
ordihull(ord,as.factor(abs_abun$Sample))
ordispider(ord,abs_abun$Treatment,display = "sites",
           col = c("black","green","blue","red"))
ordihull(ord,
         as.factor(abs_abun$Treatment),
         col = c("black","green","blue","red"))
#by sampling point
ordispider(ord,abs_abun$Sample,display = "sites",
           col = c("orange","purple","brown","gray"))
legend("bottomleft",legend = c("S00","S03","S05","S09"),
       pch = 19,col = c("orange","purple","brown","gray"))

permanova <- adonis(abs_abun[,5:13]~abs_abun$Treatment*abs_abun$Sample,
                     method = "bray",permutations = 999)
par(mar = c(3, 9, 2, 1))
coef <- coefficients(permanova)["abs_abun$Treatment1",]

barplot(sort(coef),horiz = T,las=1,main = "Top taxa in Treatment ")
coef_sample<- coefficients(permanova)["abs_abun$Sample1",]
barplot(sort(coef_sample),horiz = T,las=1,main = "Top taxa in sample ")

#Homogeneity of variances####
anova(betadisper(dist,abs_abun$Treatment))#for treatment. Problem, groups all treatments, 4samples
anova(betadisper(dist,abs_abun$Sample))
anova(betadisper(dist,abs_abun$Int))

pmod <- permutest(betadisper(dist,abs_abun$Treatment),pairwise = T)
permutest(betadisper(dist,abs_abun$Sample),pairwise = T)
permutest(betadisper(dist,abs_abun$Int),pairwise = T)#interesting

pstat <- permustats(pmod)
densityplot(pstat)
qqmath(pstat)

#Same but by sampling####
set.seed(4)
#s00####
#PCoA####
ord_0 <- capscale(abs_abun_00[,5:13]~1)
screeplot(ord_0,type="l")
eigenvals(ord_0) %>% summary()->ev_0
plot(ord_0,display="sites",main="Multivariate dispersion/s0")
ordispider(ord_0,abs_abun_00$Treatment,display = "sites",
           col = c("black","green","red","blue"))
legend("topleft",legend = c("C","D","I","E"),
       pch = 19,col = c("black","green","blue","red"))
ordihull(ord_0,as.factor(abs_abun_00$Treatment),
         col = c("black","green","red","blue"))
permanova_0 <- adonis(abs_abun_00[,5:13]~abs_abun_00$Treatment,
                     method = "bray",permutations = 999)
coef_sample_0<- coefficients(permanova_0)["abs_abun_00$Treatment1",]
barplot(sort(coef_sample_0),horiz = T,las=1,main = "Top taxa in s0 treatment ")
#permanova####
adonis2(abs_abun_00[,5:13]~abs_abun_00$Treatment,
        method = "bray",permutations = 999)
#Homogeneity####
dist_0 <- vegdist(abs_abun_00[,5:13],method = "bray")
hist(dist_0)
variance_0 <- betadisper(dist_0,abs_abun_00$Treatment)
anova(betadisper(dist_0,abs_abun_00$Treatment))
plot(betadisper(dist_0,abs_abun_00$Treatment),
     col = c("#333333","#0066CC","#FF6666","#66CC99"))
permutest_00 <- permutest(betadisper(dist_0,abs_abun_00$Treatment),pairwise = T)

labs_0 <- paste("Dimension",1:4,"(",
              round(100*variance_0$eig/sum(variance_0$eig),2),"%)")
plot(variance_0, cex = 0.7,pch = 15:17,
     main = "MDS coordinates s0", cex.lab=1,
     xlab = labs_0[1],ylab = labs_0[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#0066CC","#FF6666","#66CC99"))
#s03####
set.seed(4)
#PCoA####
ord_3 <- capscale(abs_abun_03[,5:13]~1)
screeplot(ord_3,type="l")
eigenvals(ord_3) %>% summary()->ev_3
plot(ord_3,display="sites",main="Multivariate dispersion/s3")
ordispider(ord_3,abs_abun_03$Treatment,display = "sites",
           col = c("black","green","red","blue"))
legend("topleft",legend = c("C","D","I","E"),
       pch = 19,col = c("black","green","blue","red"))
ordihull(ord_3,as.factor(abs_abun_03$Treatment),
         col = c("black","green","red","blue"))
permanova_3 <- adonis(abs_abun_03[,5:13]~abs_abun_03$Treatment,
                      method = "bray",permutations = 999)
coef_sample_3<- coefficients(permanova_3)["abs_abun_03$Treatment1",]
barplot(sort(coef_sample_3),horiz = T,las=1,main = "Top taxa in s3 treatment ")
#permanova####
adonis2(abs_abun_03[,5:13]~abs_abun_03$Treatment,
        method = "bray",permutations = 999)
#Homogeneity####
dist_3 <- vegdist(abs_abun_03[,5:13],method = "bray")
hist(dist_3)
variance_3 <- betadisper(dist_3,abs_abun_03$Treatment)
anova(betadisper(dist_3,abs_abun_03$Treatment))
plot(betadisper(dist_3,abs_abun_03$Treatment),
     col = c("#333333","#66CC99","#FF6666","#0066CC"))
permutest(betadisper(dist_3,abs_abun_03$Treatment),pairwise = T)
labs_3 <- paste("Dimension",1:4,"(",
                round(100*variance_3$eig/sum(variance_3$eig),2),"%)")
plot(variance_3, cex = 0.7,pch = 15:17,
     main = "MDS coordinates s3", cex.lab=1,
     xlab = labs_3[1],ylab = labs_3[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#0066CC","#FF6666","#66CC99"))
#s05####
set.seed(4)
#PCoA####
permanova_5 <- adonis(abs_abun_05[,5:13]~abs_abun_05$Treatment,
                      permutations = 999,method = "bray")
coef_5 <- coefficients(permanova_5)["abs_abun_05$Treatment1",]
top.coef_5 <- coef[rev(order(abs(coef_5)))[1:9]]
par(mar = c(3, 9, 2, 1))
barplot(sort(coef_5),horiz = T,las=1,
        main = "Top taxa s05")
#permanova####
adonis2(abs_abun_05[,5:13]~abs_abun_05$Treatment,
        method = "bray",permutations = 999) 
#Homogeneity####
dist_05 <- vegdist(abs_abun_05[,5:13],method = "bray")
hist(dist_05)

variance_5 <- betadisper(dist_05,abs_abun_05$Treatment)
anova(betadisper(dist_05,abs_abun_05$Treatment))
plot(betadisper(dist_05,abs_abun_05$Treatment),
     col = c("#333333","#66CC99","#FF6666","#0066CC"),
     main = "Homogeneity of variances S05")

permutest(betadisper(dist_05,abs_abun_05$Treatment),pairwise = T)
labs_5 <- paste("Dimension",1:4,"(",
                round(100*variance_5$eig/sum(variance_5$eig),2),"%)")
plot(variance_5, cex = 0.7,pch = 15:17,
     main = "MDS coordinates s5", cex.lab=1,
     xlab = labs_5[1],ylab = labs_5[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#0066CC","#FF6666","#66CC99"))
#s09####
set.seed(4)
#PCoA####
permanova_9 <- adonis(abs_abun_09[,5:13]~abs_abun_09$Treatment,
                      permutations = 999,method = "bray")
coef_9 <- coefficients(permanova_9)["abs_abun_09$Treatment1",]
barplot(sort(coef_9),horiz = T,las=1,
        main = "Top taxa s09")
#permanova####
adonis2(abs_abun_09[5:13]~abs_abun_09$Treatment,
        method = "bray",permutations = 999)
#Homogeneity####
dist_09 <- vegdist(abs_abun_09[,5:13],method = "bray")
hist(dist_09)
variance_9 <- betadisper(dist_09,abs_abun_09$Treatment)
anova(betadisper(dist_09,abs_abun_09$Treatment))
betadisper(dist_09,abs_abun_09$Treatment)
plot(betadisper(dist_09,abs_abun_09$Treatment),
     col = c("#333333","#66CC99","#FF6666","#0066CC"),
     main = "Homogeneity of variances S09")
permutest(betadisper(dist_09,abs_abun_09$Treatment),pairwise = T)
labs_9 <- paste("Dimension",1:4,"(",
                round(100*variance_9$eig/sum(variance_9$eig),2),"%)")
plot(variance_9, cex = 0.7,pch = 15:17,
     main = "MDS coordinates s9", cex.lab=1,
     xlab = labs_9[1],ylab = labs_9[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#0066CC","#FF6666","#66CC99"))
#combined####
par(mfrow=c(2,2))
 plot(betadisper(dist_0,abs_abun_00$Treatment),
       col = c("#333333","#66CC99","#FF6666","#0066CC"),
      main = "   Day 1")
 plot(betadisper(dist_3,abs_abun_03$Treatment),
      col = c("#333333","#66CC99","#FF6666","#0066CC"),
      main = "   Day 13")
 plot(betadisper(dist_05,abs_abun_05$Treatment),
      col = c("#333333","#66CC99","#FF6666","#0066CC"),
      main = "   Day 21")
 plot(betadisper(dist_09,abs_abun_09$Treatment),
      col = c("#333333","#66CC99","#FF6666","#0066CC"),
      main = "   Day 38")
par(mar=c(4,5,2,1))
par(mfrow=c(2,2))
plot(variance_0, cex = 0.7,pch = 15:17,
      main = " Day 1", cex.lab=1,
      xlab = labs_0[1],ylab = labs_0[2],
      hull = T,lwd = 1,conf = 0.5,
      col = c("#333333","#66CC99","#FF6666","#0066CC"))

plot(variance_3, cex = 0.7,pch = 15:17,
     main = " Day 13", cex.lab=1,
     xlab = labs_3[1],ylab = labs_3[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#66CC99","#FF6666","#0066CC"))

plot(variance_5, cex = 0.7,pch = 15:17,
     main = " Day 21", cex.lab=1,
     xlab = labs_5[1],ylab = labs_5[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#66CC99","#FF6666","#0066CC"))

plot(variance_9, cex = 0.7,pch = 15:17,
     main = " Day 38", cex.lab=1,
     xlab = labs_9[1],ylab = labs_9[2],
     hull = T,lwd = 1,conf = 0.5,
     col = c("#333333","#66CC99","#FF6666","#0066CC"))

#BARPLOTS####
data_all <- read.csv("visualisation_400_countings.csv")

grouped_data <- data_all[,47:73]
abs_abun <- grouped_data[,1:13]
names(abs_abun) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                     "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                     "Euglenozoa","Streptophyta")

rel_abun <- grouped_data[,15:27]
names(rel_abun) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                     "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                     "Euglenozoa","Streptophyta")
msc_plot_0 <- gather(abs_abun_00,
                     species,
                     Abun,
                     Cyanobacteria:Streptophyta)
msc_plot_3 <- gather(abs_abun_03,
                     species,
                     Abun,
                     Cyanobacteria:Streptophyta)
msc_plot_5 <- gather(abs_abun_05,
                     species,
                     Abun,
                     Cyanobacteria:Streptophyta)
msc_plot_9 <- gather(abs_abun_09,
                     species,
                     Abun,
                     Cyanobacteria:Streptophyta)
s00<- ggbarplot(msc_plot_0,
                x="Treatment",
                y="Abun",
                color = "species",
                title = "Day 1",
                combine = TRUE,
                add = "mean",
                fill = "species",
                legend.title="Group",
                palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                            "#660099","#330033","#FFCC33","#336600"))+
  ylab("")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
s09 <- ggbarplot(msc_plot_9,
                 x="Treatment",
                 y="Abun",
                 color = "species",
                 title = "Day 38",
                 combine = TRUE,
                 add = "mean",
                 fill = "species",
                 legend.title="Group",
                 palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                             "#660099","#330033","#FFCC33","#336600"))+
  ylab("")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
s03 <- ggbarplot(msc_plot_3,
                 x="Treatment",
                 y="Abun",
                 color = "species",
                 title = "Day 13",
                 combine = TRUE,
                 add = "mean",
                 fill = "species",
                 legend.title="Group",
                 palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                             "#660099","#330033","#FFCC33","#336600"))+
  ylab("")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
s05 <- ggbarplot(msc_plot_5,
                 x="Treatment",
                 y="Abun",
                 color = "species",
                 title = "Day 21",
                 combine = TRUE,
                 add = "mean",
                 fill = "species",
                 legend.title="Group",
                 palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                             "#660099","#330033","#FFCC33","#336600"))+
  ylab("")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
plot_general <- ggarrange(s00,s03,s05,s09,
          common.legend = T,
          ncol = 2,
          nrow = 2)
annotate_figure(plot_general,
                top = text_grob("Community Abundance",face = "bold"),
                bottom = "Treatment",
                left = "Cells / ml")
#Relative####
rel_abun <- grouped_data[,15:27]
names(rel_abun) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                     "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                     "Euglenozoa","Streptophyta")

rel_abun_list <- split(rel_abun,rel_abun$Sample)
rel_abun_00 <- as.data.frame(rel_abun_list[1])
names(rel_abun_00) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                        "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                        "Euglenozoa","Streptophyta")
rel_abun_03 <- as.data.frame(rel_abun_list[2])
names(rel_abun_03) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                        "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                        "Euglenozoa","Streptophyta")
rel_abun_05 <- as.data.frame(rel_abun_list[3])
names(rel_abun_05) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                        "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                        "Euglenozoa","Streptophyta")
rel_abun_09 <- as.data.frame(rel_abun_list[4])
names(rel_abun_09) <- c("Mesocosm","Treatment","Replicate","Sample","Cyanobacteria","Chlorophyta",
                        "Ciliophora","Cryptophyta","Heterokontophyta","Bacillariophyta","Heliozoa",
                        "Euglenozoa","Streptophyta")

r_msc_plot_0 <- gather(rel_abun_00,
                       species,
                       Abun,
                       Cyanobacteria:Streptophyta)
r_msc_plot_3 <- gather(rel_abun_03,
                       species,
                       Abun,
                       Cyanobacteria:Streptophyta)
r_msc_plot_5 <- gather(rel_abun_05,
                       species,
                       Abun,
                       Cyanobacteria:Streptophyta)
r_msc_plot_9 <- gather(rel_abun_09,
                       species,
                       Abun,
                       Cyanobacteria:Streptophyta)
rs00<- ggbarplot(r_msc_plot_0,
                 x="Treatment",
                 y="Abun",
                 color = "species",
                 title = "Day 1",
                 combine = TRUE,
                 add = "mean",
                 fill = "species",
                 legend.title="Group",
                 palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                             "#660099","#330033","#FFCC33","#336600"))+
  ylab("")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
rs09 <- ggbarplot(r_msc_plot_9,
                  x="Treatment",
                  y="Abun",
                  color = "species",
                  title = "Day 38",
                  combine = TRUE,
                  add = "mean",
                  fill = "species",
                  legend.title="Group",
                  palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                              "#660099","#330033","#FFCC33","#336600"))+
  ylab("")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
rs03 <- ggbarplot(r_msc_plot_3,
                  x="Treatment",
                  y="Abun",
                  color = "species",
                  title = "Day 13",
                  combine = TRUE,
                  add = "mean",
                  fill = "species",
                  legend.title="Group",
                  palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                              "#660099","#330033","#FFCC33","#336600"))+
  ylab("")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
rs05 <- ggbarplot(r_msc_plot_5,
                  x="Treatment",
                  y="Abun",
                  color = "species",
                  title = "Day 21",
                  combine = TRUE,
                  add = "mean",
                  fill = "species",
                  legend.title="Group",
                  palette = c("#CC3333","#99FF99","#FF00FF","#FF9933","#66FFFF",
                              "#660099","#330033","#FFCC33","#336600"))+
  ylab("")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
plot_general_r <- ggarrange(rs00,rs03,rs05,rs09,
          common.legend = T,
          ncol = 2,
          nrow = 2)
annotate_figure(plot_general_r,
                top = text_grob("Community Composition with Relative Abundance",
                                face = "bold"),
                bottom = "Treatment",
                left = "Proportion")
#permanova####
set.seed(5)
adonis2(rel_abun[,5:13]~rel_abun$Treatment,method = "bray")
adonis2(rel_abun[,5:13]~rel_abun$Sample,method = "bray")
permanovar <- adonis(rel_abun[,5:13]~rel_abun$Treatment*rel_abun$Sample,
                     method="bray",permutations=999)
coefr <- coefficients(permanovar)["rel_abun$Treatment1",]
top.coef <- coef[rev(order(abs(coefr)))[1:9]]
par(mar = c(3, 9, 2, 1))
barplot(sort(coefr),horiz = T,las=1,main = "Top taxa contributting to Treatment")
set.seed(5)
#Homogeneity####
distr <- vegdist(rel_abun[,5:13],method = "bray")#vector
rel_abun[,14]<-paste(rel_abun$Treatment.2,rel_abun$Sample.2)
par(mfrow=c(1,1))
plot(betadisper(distr,rel_abun$Treatment),main = "Multivariate disperson / r all data",
     ellipse = F,hull = T,xlab = "PCoA 1")
plot(betadisper(distr,rel_abun$Sample),main = "Multivariate disperson / all data",
     ellipse = F,hull = T,xlab = "PCoA 1",col = c("orange","purple","brown","gray"))
plot(betadisper(distr,rel_abun$V14),main = "Multivariate disperson / all data")
#PCoA####
ordr <- capscale(rel_abun[,5:13]~1)
screeplot(ordr,type="l")
eigenvals(ordr) %>% summary()->evr
plot(ordr,display="sites",main="Multivariate dispersion/all data/sample")
ordispider(ordr,rel_abun$Treatment,display = "sites",
           col = c("black","green","blue","red"))
legend("topright",legend = c("C","D","I","E"),
       pch = 19,col = c("Black","green","blue","red"))
ordispider(ordr,rel_abun$Sample,display = "sites",
           col = c("orange","purple","brown","gray"))
legend("topright",legend = c("S00","S03","S05","S09"),
       pch = 19,col = c("orange","purple","brown","gray"))

anova(betadisper(distr,
                 rel_abun$Treatment))#for treatment. Problem, groups all treatments, 4samples
anova(betadisper(distr,
                 rel_abun$Sample))#for sampling
anova(betadisper(distr,
                 rel_abun$V14))

permutest(betadisper(distr,rel_abun$Treatment),pairwise = T)
permutest(betadisper(distr,rel_abun$Sample),pairwise = T)
permutest(betadisper(distr,rel_abun$V14),pairwise = T)#interesting

#By sampling point####
set.seed(4)
#permanova####
adonis2(rel_abun_00[,5:13]~rel_abun_00$Treatment,
        method = "bray",permutations = 999)
adonis2(rel_abun_03[,5:13]~rel_abun_03$Treatment,
        method = "bray",permutations = 999)
adonis2(rel_abun_05[,5:13]~rel_abun_05$Treatment,
        method = "bray",permutations = 999)
adonis2(rel_abun_09[,5:13]~rel_abun_09$Treatment,
        method = "bray",permutations = 999)
#check permutest in each smapling point, specially relevant in s05 and s09
dist_05r <- vegdist(rel_abun_05[,5:13],method = "bray")
hist(dist_05r)
dist_09r <- vegdist(rel_abun_09[,5:13],method = "bray")
hist(dist_09r)
betadisper(dist_05r,rel_abun_05$Treatment)
anova(betadisper(dist_05r,rel_abun_05$Treatment))
plot(betadisper(dist_05r,rel_abun_05$Treatment),
     col = c("#333333","#66CC99","#FF6666","#0066CC"),
     main = "Homogeneity of variances S05")
betadisper(dist_09r,rel_abun_09$Treatment)
anova(betadisper(dist_09r,rel_abun_09$Treatment))
plot(betadisper(dist_09r,rel_abun_09$Treatment),
     col = c("#333333","#66CC99","#FF6666","#0066CC"),
     main = "Homogeneity of variances S09")
permutest(betadisper(dist_09r,rel_abun_09$Treatment),pairwise = T)
permutest(betadisper(dist_05r,rel_abun_05$Treatment),pairwise = T)

#check which taxa contributes to the comm diff. With Adonis
permanova_5r <- adonis(rel_abun_05[,5:13]~rel_abun_05$Treatment,
                       permutations = 999,method = "bray")
coef_5r <- coefficients(permanova_5r)["rel_abun_05$Treatment1",]
top.coef_5r <- coef[rev(order(abs(coef_5r)))[1:9]]
par(mar = c(3, 9, 2, 1))
barplot(sort(coef_5r),horiz = T,las=1,
        main = "Top taxa s05 relative")
permanova_9r <- adonis(rel_abun_09[,5:13]~rel_abun_09$Treatment,
                       permutations = 999,method = "bray")
coef_9r <- coefficients(permanova_9r)["rel_abun_09$Treatment1",]
barplot(sort(coef_9r),horiz = T,las=1,
        main = "Top taxa s09 relative")

#bY GENUS####
raw_msc_genus <- read.csv("raw-visualisation_400_countings.csv")
raw_abun_list <- split(raw_msc_genus,raw_msc_genus$Sample)
raw_0 <- as.data.frame(raw_abun_list[1])
names(raw_0) <- names(raw_msc_genus)
raw_3 <- as.data.frame(raw_abun_list[2])
names(raw_3) <- names(raw_msc_genus)
raw_5 <- as.data.frame(raw_abun_list[3])
names(raw_5) <- names(raw_msc_genus)
raw_9 <- as.data.frame(raw_abun_list[4])
names(raw_9) <- names(raw_msc_genus)

#Alpha diversity####
alpha_d0_c <- as.data.frame(split(raw_0,raw_0$Treatment)[1])
alpha_d0_d <- as.data.frame(split(raw_0,raw_0$Treatment)[2])
alpha_d0_e <- as.data.frame(split(raw_0,raw_0$Treatment)[3])
alpha_d0_i <- as.data.frame(split(raw_0,raw_0$Treatment)[4])

mean(alpha_d0_c$C.No_species)
sd(alpha_d0_c$C.No_species)
mean(alpha_d0_d$D.No_species)
sd(alpha_d0_d$D.No_species)
mean(alpha_d0_e$E.No_species)
sd(alpha_d0_e$E.No_species)
mean(alpha_d0_i$I.No_species)
sd(alpha_d0_i$I.No_species)

alpha_d13_c <- as.data.frame(split(raw_3,raw_3$Treatment)[1])
alpha_d13_d <- as.data.frame(split(raw_3,raw_3$Treatment)[2])
alpha_d13_e <- as.data.frame(split(raw_3,raw_3$Treatment)[3])
alpha_d13_i <- as.data.frame(split(raw_3,raw_3$Treatment)[4])

mean(alpha_d13_c$C.No_species)
sd(alpha_d13_c$C.No_species)
mean(alpha_d13_d$D.No_species)
sd(alpha_d13_d$D.No_species)
mean(alpha_d13_e$E.No_species)
sd(alpha_d13_e$E.No_species)
mean(alpha_d13_i$I.No_species)
sd(alpha_d13_i$I.No_species)

alpha_d21_c <- as.data.frame(split(raw_5,raw_5$Treatment)[1])
alpha_d21_d <- as.data.frame(split(raw_5,raw_5$Treatment)[2])
alpha_d21_e <- as.data.frame(split(raw_5,raw_5$Treatment)[3])
alpha_d21_i <- as.data.frame(split(raw_5,raw_5$Treatment)[4])

mean(alpha_d21_c$C.No_species)
sd(alpha_d21_c$C.No_species)
mean(alpha_d21_d$D.No_species)
sd(alpha_d21_d$D.No_species)
mean(alpha_d21_e$E.No_species)
sd(alpha_d21_e$E.No_species)
mean(alpha_d21_i$I.No_species)
sd(alpha_d21_i$I.No_species)

alpha_d38_c <- as.data.frame(split(raw_9,raw_9$Treatment)[1])
alpha_d38_d <- as.data.frame(split(raw_9,raw_9$Treatment)[2])
alpha_d38_e <- as.data.frame(split(raw_9,raw_9$Treatment)[3])
alpha_d38_i <- as.data.frame(split(raw_9,raw_9$Treatment)[4])

mean(alpha_d38_c$C.No_species)
sd(alpha_d38_c$C.No_species)
mean(alpha_d38_d$D.No_species)
sd(alpha_d38_d$D.No_species)
mean(alpha_d38_e$E.No_species)
sd(alpha_d38_e$E.No_species)
mean(alpha_d38_i$I.No_species)
sd(alpha_d38_i$I.No_species)

genus_msc_0 <- gather(raw_0,
                       species,
                       Abun,
                       Ochromonas:Stauridium)
genus_msc_3 <- gather(raw_3,
                       species,
                       Abun,
                      Ochromonas:Stauridium)
genus_msc_5 <- gather(raw_5,
                       species,
                       Abun,
                      Ochromonas:Stauridium)
genus_msc_9 <- gather(raw_9,
                       species,
                       Abun,
                      Ochromonas:Stauridium)
raw00<- ggbarplot(genus_msc_0,
                 x="Treatment",
                 y="Abun",
                 color = "species",
                 title = "Day 1",
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
raw9 <- ggbarplot(genus_msc_9,
                  x="Treatment",
                  y="Abun",
                  color = "species",
                  title = "Day 38",
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
raw3 <- ggbarplot(genus_msc_3,
                  x="Treatment",
                  y="Abun",
                  color = "species",
                  title = "Day 13",
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
raw5 <- ggbarplot(genus_msc_5,
                  x="Treatment",
                  y="Abun",
                  color = "species",
                  title = "Day 21",
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
plot_general_genus <- ggarrange(raw00,raw3,raw5,raw9,
                            common.legend = T,
                            ncol = 2,
                            nrow = 2,
                            labels = c("A","B","C","D"))
annotate_figure(plot_general_genus,
                top = text_grob("Mesocosm Treatment Community Composition by Genus",
                                face = "bold"),
                bottom = "Treatment")
#contributing species
set.seed(4)
permanova_5_genus <- adonis(raw_5[,4:43]~raw_5$Treatment,
                             permutations = 999,method = "bray")
coef_5g <- coefficients(permanova_5_genus)["raw_5$Treatment1",]
top.coef_5g <- coef[rev(order(abs(coef_5g)))[1:9]]
par(mar = c(3, 9, 2, 1))
barplot(sort(coef_5g),horiz = T,las=1,
        main = "Top taxa s05 absolute genus")
permanova_9_genus <- adonis(raw_9[,4:43]~raw_9$Treatment,
                             permutations = 999,method = "bray")
coef_9g <- coefficients(permanova_9_genus)["raw_9$Treatment1",]
barplot(sort(coef_9g),horiz = T,las=1,
        main = "Top taxa s09 absolute genus")
#relative####
msc_relative <- read.csv("relative_raw_visualisation_400_countings.csv")
raw_rel_list <- split(msc_relative,msc_relative$Sample)
raw_r0 <- as.data.frame(raw_rel_list[1])
names(raw_r0) <- names(msc_relative)
raw_r3 <- as.data.frame(raw_rel_list[2])
names(raw_r3) <- names(msc_relative)
raw_r5 <- as.data.frame(raw_rel_list[3])
names(raw_r5) <- names(msc_relative)
raw_r9 <- as.data.frame(raw_rel_list[4])
names(raw_r9) <- names(msc_relative)

genus_msc_r0 <- gather(raw_r0,
                      species,
                      Abun,
                      Ochromonas:Stauridium)
genus_msc_r3 <- gather(raw_r3,
                      species,
                      Abun,
                      Ochromonas:Stauridium)
genus_msc_r5 <- gather(raw_r5,
                      species,
                      Abun,
                      Ochromonas:Stauridium)
genus_msc_r9 <- gather(raw_r9,
                      species,
                      Abun,
                      Ochromonas:Stauridium)
raw00r<- ggbarplot(genus_msc_r0,
                  x="Treatment",
                  y="Abun",
                  color = "species",
                  title = "Day 1",
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
  ylab("Proportion")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
raw9r <- ggbarplot(genus_msc_r9,
                  x="Treatment",
                  y="Abun",
                  color = "species",
                  title = "Day 38",
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

  ylab("Proportion")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
raw3r <- ggbarplot(genus_msc_r3,
                  x="Treatment",
                  y="Abun",
                  color = "species",
                  title = "Day 13",
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
  ylab("Proportion")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
raw5r <- ggbarplot(genus_msc_r5,
                  x="Treatment",
                  y="Abun",
                  color = "species",
                  title = "Day 21",
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
  ylab("Proportion")+
  xlab("")+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
rplot_general_genus <- ggarrange(raw00r,raw3r,raw5r,raw9r,
                                common.legend = T,
                                ncol = 2,
                                nrow = 2,
                                labels = c("A","B","C","D"))
annotate_figure(rplot_general_genus,
                top = text_grob("Mesocosm Treatment Community Composition by Genus",
                                face = "bold"),
                bottom = "Treatment")
#contributing species
set.seed(4)
permanova_5r_genus <- adonis(raw_r5[,4:43]~raw_r5$Treatment,
                       permutations = 999,method = "bray")
coef_5rg <- coefficients(permanova_5r_genus)["raw_r5$Treatment1",]
top.coef_5r <- coef[rev(order(abs(coef_5rg)))[1:9]]
par(mar = c(3, 9, 2, 1))
barplot(sort(coef_5rg),horiz = T,las=1,
        main = "Top taxa s05 relative genus")
permanova_9r_genus <- adonis(raw_r9[,4:43]~raw_r9$Treatment,
                       permutations = 999,method = "bray")
coef_9rg <- coefficients(permanova_9r_genus)["raw_r9$Treatment1",]
barplot(sort(coef_9rg),horiz = T,las=1,
        main = "Top taxa s09 relative genus")

#Abundance stats####
day0_C <- as.data.frame(split(abs_abun_00,abs_abun_00$Treatment)[1])
day0_D <- as.data.frame(split(abs_abun_00,abs_abun_00$Treatment)[2])
day0_E <- as.data.frame(split(abs_abun_00,abs_abun_00$Treatment)[3])
day0_I <- as.data.frame(split(abs_abun_00,abs_abun_00$Treatment)[4])
rowSums(day0_C[,5:13])
mean(rowSums(day0_C[,5:13]))
sd(rowSums(day0_C[,5:13]))
(sd(rowSums(day0_C[,5:13]))/mean(rowSums(day0_C[,5:13])))*100
rowSums(day0_D[,5:13])
mean(rowSums(day0_D[,5:13]))
sd(rowSums(day0_D[,5:13]))
(sd(rowSums(day0_D[,5:13]))/mean(rowSums(day0_D[,5:13])))*100
rowSums(day0_E[,5:13])
mean(rowSums(day0_E[,5:13]))
sd(rowSums(day0_E[,5:13]))
(sd(rowSums(day0_E[,5:13]))/mean(rowSums(day0_E[,5:13])))*100
rowSums(day0_I[,5:13])
mean(rowSums(day0_I[,5:13]))
sd(rowSums(day0_I[,5:13]))
(sd(rowSums(day0_I[,5:13]))/mean(rowSums(day0_I[,5:13])))*100

day13_C <- as.data.frame(split(abs_abun_03,abs_abun_03$Treatment)[1])
day13_D <- as.data.frame(split(abs_abun_03,abs_abun_03$Treatment)[2])
day13_E <- as.data.frame(split(abs_abun_03,abs_abun_03$Treatment)[3])
day13_I <- as.data.frame(split(abs_abun_03,abs_abun_03$Treatment)[4])

rowSums(day13_C[,5:13])
mean(rowSums(day13_C[,5:13]))
sd(rowSums(day13_C[,5:13]))
(sd(rowSums(day13_C[,5:13]))/mean(rowSums(day13_C[,5:13])))*100
rowSums(day13_D[,5:13])
mean(rowSums(day13_D[,5:13]))
sd(rowSums(day13_D[,5:13]))
(sd(rowSums(day13_D[,5:13]))/mean(rowSums(day13_D[,5:13])))*100
rowSums(day13_E[,5:13])
mean(rowSums(day13_E[,5:13]))
sd(rowSums(day13_E[,5:13]))
(sd(rowSums(day13_E[,5:13]))/mean(rowSums(day13_E[,5:13])))*100
rowSums(day13_I[,5:13])
mean(rowSums(day13_I[,5:13]))
sd(rowSums(day13_I[,5:13]))
(sd(rowSums(day13_I[,5:13]))/mean(rowSums(day13_I[,5:13])))*100

day21_C <- as.data.frame(split(abs_abun_05,abs_abun_05$Treatment)[1])
day21_D <- as.data.frame(split(abs_abun_05,abs_abun_05$Treatment)[2])
day21_E <- as.data.frame(split(abs_abun_05,abs_abun_05$Treatment)[3])
day21_I <- as.data.frame(split(abs_abun_05,abs_abun_05$Treatment)[4])

rowSums(day21_C[,5:13])
mean(rowSums(day21_C[,5:13]))
sd(rowSums(day21_C[,5:13]))
(sd(rowSums(day21_C[,5:13]))/mean(rowSums(day21_C[,5:13])))*100
rowSums(day21_D[,5:13])
mean(rowSums(day21_D[,5:13]))
sd(rowSums(day21_D[,5:13]))
(sd(rowSums(day21_D[,5:13]))/mean(rowSums(day21_D[,5:13])))*100
rowSums(day21_E[,5:13])
mean(rowSums(day21_E[,5:13]))
sd(rowSums(day21_E[,5:13]))
(sd(rowSums(day21_E[,5:13]))/mean(rowSums(day21_E[,5:13])))*100
rowSums(day21_I[,5:13])
mean(rowSums(day21_I[,5:13]))
sd(rowSums(day21_I[,5:13]))
(sd(rowSums(day21_I[,5:13]))/mean(rowSums(day0_I[,5:13])))*100

day38_C <- as.data.frame(split(abs_abun_09,abs_abun_09$Treatment)[1])
day38_D <- as.data.frame(split(abs_abun_09,abs_abun_09$Treatment)[2])
day38_E <- as.data.frame(split(abs_abun_09,abs_abun_09$Treatment)[3])
day38_I <- as.data.frame(split(abs_abun_09,abs_abun_09$Treatment)[4])

rowSums(day38_C[,5:13])
mean(rowSums(day38_C[,5:13]))
sd(rowSums(day38_C[,5:13]))
(sd(rowSums(day38_C[,5:13]))/mean(rowSums(day38_C[,5:13])))*100
rowSums(day38_D[,5:13])
mean(rowSums(day38_D[,5:13]))
sd(rowSums(day38_D[,5:13]))
(sd(rowSums(day38_D[,5:13]))/mean(rowSums(day38_D[,5:13])))*100
rowSums(day38_E[,5:13])
mean(rowSums(day38_E[,5:13]))
sd(rowSums(day38_E[,5:13]))
(sd(rowSums(day38_E[,5:13]))/mean(rowSums(day38_E[,5:13])))*100
rowSums(day38_I[,5:13])
mean(rowSums(day38_I[,5:13]))
sd(rowSums(day38_I[,5:13]))
(sd(rowSums(day38_I[,5:13]))/mean(rowSums(day38_I[,5:13])))*100
