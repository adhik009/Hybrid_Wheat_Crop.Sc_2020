rm(list = ls())

setwd("/Volumes/GoogleDrive/My Drive/AA_PhD_TAMU/Hybrid Wheat Project/Hybrid_F1/AA_F1_manuscript/Plots")
plot.dat <- read.csv("2016_F1_heterosis.csv", header = TRUE)
str(plot.dat)
 
 # plot a histogram of the mean values with checks pointed separately
 library(tidyverse)
 
 # create a tibble
 mydata <- as_data_frame(plot.dat)
 
# filter mydata to select hybrids only
hybrids <- mydata %>% filter(Hyb_class == "Hybrid")
parents <- mydata %>% filter(Hyb_class == "Parent")


tiff("2016.F1.yield.hybrids-parents.tiff", height = 7, units ='in', width = 7, res = 600)

p1 <- hist(hybrids$Mean, breaks = 25)
p2 <- hist(parents$Mean, breaks = 25)

# plot two plots on top of each other
#par(mfrow = c(2,1))

plot( p1, col=rgb(0,0,1,1/4), xlim=c(500,4000), ylim = c(0,90),
      main = "Hybrid yield McGregor 2016",
      xlab = "Yield in kg/ha", ylab = "Frequency")

# adding symbols on top of the histogram plot denoting checks
# x and y correspond to the labels of the original plot and pch denotes the type of shape added
x <-c(1976,1061,1650,1171, 3118)
y <-c(60,60,60,60,60)
points(x,y, col = "black", bg ="blue", pch =25)

# adding text to the plot
t <- c("Freeman", "Settler CL", "TAM 111", "Wesley", "TAM 304") # the text to be added
tx <- c(1976,1061,1650,1171, 3118) # x co-ordinates for the text
ty <- c(70,70,70,70,70) # y co-ordinates for the text
text(x=tx,y=ty,t, srt = 90, cex = 0.8) # pos = 3 means plot above the specified (x,y) 
# and srt is text rotation. Cex is the font specification

legend("topright", inset = 0.03, legend=c("Hybrids", "Parents"), fill = c(rgb(0,0,1,1/4),
      rgb(1,0,0,1/4)), col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)), cex=0.8)
plot( p2, col=rgb(1,0,0,1/4),xlim=c(0,3500), ylim = c(0,10),
      main = "Parents yield McGregor 2016 ",
     xlab = "Yield in kg/ha", ylab = "Frequency", add=T)#add = T to plot them on top of each other
dev.off()

##########################################################
### Plot for hybrid and parent yield 2017 combined #######
##########################################################
rm(list = ls())

setwd("/Volumes/GoogleDrive/My Drive/AA_PhD_TAMU/PhD Manuscripts/Plots")
plot.dat <- read.csv("2017_F1_heterosis.csv", header = TRUE)
str(plot.dat)
 
 # plot a histogram of the mean values with checks pointed separately
 library(tidyverse)
 
 # create a tibble
 mydata <- as_data_frame(plot.dat)
 
# filter mydata to select hybrids only
hybrids <- mydata %>% filter(Class == "Hybrid")
parents <- mydata %>% filter(Class == "Parent")


tiff("2017.F1.yield.hybrids-parents.tiff", height = 7, units ='in', width = 7, res = 600)

p1 <- hist(hybrids$Mean, breaks = 20)
p2 <- hist(parents$Mean, breaks = 20)

# plot two plots on top of each other
#par(mfrow = c(2,1))

plot( p1, col=rgb(0,0,1,1/4), xlim=c(1700,3000), ylim = c(0,100),
      main = "Hybrid yield 2017",
      xlab = "Yield in kg/ha", ylab = "Frequency")

# adding symbols on top of the histogram plot denoting checks
# x and y correspond to the labels of the original plot and pch denotes the type of shape added
x <-c(2352,2490,2657,2767)
y <-c(70,70,70,70)
points(x,y, col = "black", bg ="blue", pch =25)

# adding text to the plot
t <- c("Wesley", "Ruth", "Freeman", "TAM 111") # the text to be added
tx <- c(2352,2490,2657,2767) # x co-ordinates for the text
ty <- c(80,80,80,80) # y co-ordinates for the text
text(x=tx,y=ty,t, srt = 90, cex = 0.8) # pos = 3 means plot above the specified (x,y) 
# and srt is text rotation. Cex is the font specification

legend("topright", inset = 0.03, legend=c("Hybrids", "Parents"), fill = c(rgb(0,0,1,1/4),
      rgb(1,0,0,1/4)), col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)), cex=0.8)
plot( p2, col=rgb(1,0,0,1/4),xlim=c(0,3500), ylim = c(0,10),
      main = "Parents yield McGregor 2016 ",
     xlab = "Yield in kg/ha", ylab = "Frequency", add=T)#add = T to plot them on top of each other
dev.off()





#### Plots for Mid-parent and High Parent Heterosis
dev.new(-2:2,-4:4)
MPH <- hybrids$MPH[!is.na(hybrids$MPH)]
HPH <- hybrids$HPH[!is.na(hybrids$HPH)]
p3 <- hist(MPH, breaks = 30)
p4 <- hist(HPH, breaks = 30)

plot( p3, col=rgb(0,0,1,1/4), ylim = c(0,100), main = "Histogram of heterosis estimates",
      xlab = "Heterosis %", ylab = "Frequency")
plot( p4, col=rgb(1,0,0,1/4),breaks = 20, add=T)  # second

legend("topleft", inset = 0.03, legend=c("Mid-parent Heterosis", "High-parent Heterosis"),
       fill = c(rgb(0,0,1,2/4), rgb(1,0,0,2/4)),
       col=c(rgb(0,0,1,2/4), rgb(1,0,0,2/4)), cex=0.8)


######### Heterosis plot for 2017 ##############

library(ggplot2)
library(reshape2)

df <- melt(hybrids[,c(1,11:13)]) 

str(df)

tiff("2017.Combined_Com.Heterosis.plots.tiff", height = 3.5, units ='in', width = 7, res = 600)
ggplot(df, aes(value, fill = variable)) + 
  geom_histogram(binwidth = 2,colour="black", fill="grey")+
  facet_wrap(~variable, scales = "free_x") +
  theme_bw() + 
  guides(fill=FALSE)+
  geom_vline(aes(xintercept= 0),
             color="blue", linetype="dashed", size=0.5)+
  xlab("Heterosis %")+
  ylab("Frequency")
dev.off()


#density plot
ggplot(df, aes(value, fill = variable)) + 
    geom_density(alpha=0.8)+
    facet_wrap(~variable, scales = "free_x") +
  theme_bw() + 
    guides(fill=FALSE)



######### Heterosis plot for 2016 ##############

library(ggplot2)
library(reshape2)

df <- melt(hybrids[,c(1,4:5,9)]) 

str(df)
levels(df$variable)[levels(df$variable)=="TAM304"] <- "TAM 304"
table(df$variable)

tiff("2016.McGregor_Com.Heterosis.plots.tiff", height = 3.5, units ='in', width = 7, res = 600)
ggplot(df, aes(value, fill = variable)) + 
  geom_histogram(binwidth = 5,colour="black", fill="grey")+
  facet_wrap(~variable, scales = "free_x") +
  theme_bw() + 
  guides(fill=FALSE)+
  geom_vline(aes(xintercept= 0),
             color="blue", linetype="dashed", size=0.5)+
  xlab("Heterosis %")+
  ylab("Frequency")
dev.off()  


## Heterosis plot for combined data in 2017 ##
setwd("/Volumes/GoogleDrive/My Drive/AA_PhD_TAMU/Hybrid Wheat Project/Hybrid_F1/AA_F1_manuscript/Plots")
plot.dat <- read.csv("2017_F1_heterosis.csv", header = TRUE)
str(plot.dat)

library(tidyverse)

# create a tibble
mydata <- as_data_frame(plot.dat)

# filter mydata to select hybrids only
hybrids <- mydata %>% filter(Class == "Hybrid")
parents <- mydata %>% filter(Class == "Parent")

## setup the data from hybrid subset for plotting histogram
library(ggplot2)
library(reshape2)

df <- melt(hybrids[,c(1,11:13)]) 

str(df)
levels(df$variable)[levels(df$variable)=="TAM111"] <- "TAM 111"
table(df$variable)

tiff("2017.Combined_Com.Heterosis.plots.tiff", height = 3.5, units ='in', width = 7, res = 600)
ggplot(df, aes(value, fill = variable)) + 
  geom_histogram(binwidth = 3,colour="black", fill="grey")+
  facet_wrap(~variable, scales = "free_x") +
  theme_bw() + 
  guides(fill=FALSE)+
  geom_vline(aes(xintercept= 0),
             color="blue", linetype="dashed", size=0.5)+
  xlab("Heterosis %")+
  ylab("Frequency")
dev.off()  

