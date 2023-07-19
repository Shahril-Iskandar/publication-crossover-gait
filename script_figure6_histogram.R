# Plotting Figure 6 - Histogram data

library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plyr)

setwd("C:/Users/14000/Downloads/URECA/Final URECA files/Gait and Posture") #<----------    to change accordingly
filename <- "44 Participants Data.xlsx"
df <- read_excel(filename)
#view(df)
#head(df)

df2 <- df %>% drop_na(Left2)
df3 <- df %>% drop_na(Right2)

left <- df2$Left2
right <- df3$Right2
sex <- df$Sex

#1st method (NOT USED - Each sub-figure only contain x-ticks in which they have values)
leftpic <- ggplot(df2, aes(factor(Left2),fill=Sex), xlab="Left") + geom_bar(width=.45) + theme(plot.title = element_text(hjust = 0.5, size=18), axis.title=element_text(size=15), legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + scale_fill_manual(values = c("#D9008D","#018288")) + ylim(0,4) + labs(y="Number of participants", x="Number of crossover gait") + ggtitle("Left foot")
rightpic <- ggplot(df3, aes(factor(Right2),fill=Sex)) + geom_bar() + scale_fill_manual(values = c("#D9008D","#018288")) + labs(y="Number of participants", x="Number of crossover gait") + theme(plot.title = element_text(hjust = 0.5, size=18), axis.title=element_text(size=15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +ggtitle("Right foot")

# Combine both plot into one figure
figure <- ggarrange(leftpic, rightpic,
                    labels = c("a","b"),
                    ncol=2, nrow=1)
# Show figure
figure # 1000 x 594

#2nd method (USED IN PUBLICATION - Each sub-figure contains the same x-tick values)
leftpic <- ggplot(df2, aes(factor(Left2),fill=Sex), xlab="Left") + geom_bar(width=.75)+ theme(plot.title = element_text(hjust = 0.5, size=15), axis.title=element_text(size=15), legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + scale_fill_manual(values = c("#D9008D","#018288")) + ylim(0,4) + labs(y="Number of participants", x="Number of crossover gait") + ggtitle("Left foot") + scale_x_discrete(limits = c("1", "2", "3", "4", "7", "8", "10", "13"))
leftpic
rightpic <- ggplot(df3, aes(factor(Right2),fill=Sex)) + geom_bar(width=.75) + scale_fill_manual(values = c("#D9008D","#018288")) + labs(y="Number of participants", x="Number of crossover gait") + theme(plot.title = element_text(hjust = 0.5, size=15), axis.title=element_text(size=15), legend.position=c(0.9,0.8), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +ggtitle("Right foot") + scale_x_discrete(limits = c("1", "2", "3", "4", "7", "8", "10", "13"))
# Combine both plot into one figure
figure <- ggarrange(leftpic, rightpic,
                    labels = c("(A)","(B)"),
                    ncol=2, nrow=1)

# Show figure
figure

# Change accordingly to the directory you want to save 
setwd("C:/Users/14000/Downloads/Publications/Frontiers in Bioengineering") 

# Save plot to specific size
ggsave(
  "Figure6.jpeg",
  width = 2000,
  height = 1200,
  units = "px",
  dpi=300
)

# Plotting Figure 7 - Correlation and regression


