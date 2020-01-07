rm(list=ls())
getwd()

setwd("G:/My Drive/AA_PhD_TAMU/Hybrid Wheat Project/Hybrid_F1/AA_F1_manuscript/Plots")
list.files()

GCA <- read.csv("GCA_plot.csv", header = TRUE)
library(dplyr)
GCA1 <- GCA[GCA$Class =="GCA",]
RGCA <- GCA[GCA$Class =="RGCA",]
library(ggplot2)

str(GCA)
#limits <- aes(ymax = GCA$Estimate,
#              ymin = GCA$SE)
tiff("GCA-RGCA.tiff", height = 7, units ='in', width = 7, res = 600)
p <- ggplot(data = GCA1, aes(x=Genotype, y=Estimate, fill = Estimate))+ theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1)) + labs(xlab ="", ylab = "GCA (kg/ha)") +
  geom_bar(stat = "identity", color = "black", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), position = position_dodge(0.9),
                width = 0.5) 

q <- ggplot(data = RGCA, aes(x=Genotype, y=Estimate, fill = Estimate))+ theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1)) + labs(xlab ="", ylab = "Reciprocal GCA (kg/ha)") +
  geom_bar(stat = "identity", color = "black", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), position = position_dodge(0.9),
                width = 0.5) 
library(gridExtra)
grid.arrange(p,q,nrow=2)
dev.off()
