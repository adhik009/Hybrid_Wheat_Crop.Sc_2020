rm(list =ls)
setwd("/Volumes/GoogleDrive/My Drive/AA_PhD_TAMU/Hybrid Wheat Project/Hybrid_F1/Hybrid_prediction")
list.files()
data <- read.csv("Hybrid_pred.csv", header = TRUE)
str(data)
names(data)

### correlation plot ##########
library(ggplot2)
tiff("Hybrid.pred_GCA.correlation_plot.tiff", width = 7, height = 5, units = 'in', res = 600)
ggplot(data, aes(x=Mean, y=GCA)) +
  xlab("Hybrid yield (kg/ha)") + ylab("GCA predicted yield (kg/ha)") + 
  ggtitle("") + 
  geom_point(shape=19) +    # Use hollow circles
  geom_smooth(method=lm) +
  theme_minimal()+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), asp = 0.7, 
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = 2000, y = 200, label = "r = 0.69**", colour = "red")
dev.off()
# Add linear regression line 
#  (by default includes 95% confidence region)
library(ggplot2)
tiff("Hybrid.pred_MPV.correlation_plot.tiff", width = 7, height = 5, units = 'in', res = 600)
ggplot(data, aes(x=Mean, y=MPV)) +
  xlab("Hybrid yield (kg/ha)") + ylab("Mid-parent value (kg/ha)") + 
  ggtitle("") + 
  geom_point(shape=19) + ylim(1500, 3000) +   # Use hollow circles
  geom_smooth(method=lm) +
  theme_minimal()+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), asp = 0.7, 
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = 2500, y = 2800, label = "r = 0.44**", colour = "red")
dev.off()

## Now lets plot these two graphs together side by side
require(gridExtra)

# open the tiff dialog
tiff("Hybrid.pred_correlation.plots.tiff", width = 5, height = 7, units = 'in', res = 600)
# Plot the first graph
plot1 <- ggplot(data, aes(x=Mean, y=GCA)) +
  xlab("Hybrid yield (kg/ha)") + ylab("GCA predicted yield (kg/ha)") + 
  ggtitle("") + 
  geom_point(shape=19) +    # Use hollow circles
  geom_smooth(method=lm) +
  theme_minimal()+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), asp = 0.7, 
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = 2000, y = 200, label = "r = 0.69***", colour = "black")
# plot the second graph
plot2 <- ggplot(data, aes(x=Mean, y=MPV)) +
  xlab("Hybrid yield (kg/ha)") + ylab("Mid-parent value (kg/ha)") + 
  ggtitle("") + 
  geom_point(shape=19) + ylim(1500, 3000) +   # Use hollow circles
  geom_smooth(method=lm) +
  theme_minimal()+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), asp = 0.7, 
        plot.title = element_text(hjust = 0.5))+
  annotate("text", x = 2000, y = 2800, label = "r = 0.44***", colour = "black")

grid.arrange(plot1, plot2, nrow=2)
dev.off()