library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(rcompanion)

setwd("C:/Users/14000/Downloads/URECA/Final URECA files/Foot angle [Rearfoot Inversion] - 2x2 ANOVA") #<----------    to change accordingly
filename <- "Foot angle data for 2x2 ANOVA.xlsx"
data <- read_excel(filename)
#view(data)

leftfront <- data$`RearfootInversion(Front)_L_avg18_day1`
leftback <- data$`RearfootInversion(Back)_L_avg18_day1`

rightfront <- data$`RearfootInversion(Front)_R_avg18_day1`
rightback <- data$`RearfootInversion(Back)_R_avg18_day1`



combinefront <- leftfront + rightfront
combineback <- leftback + rightback

# 1st method

plot(leftfront, leftback, pch = 10, col="lightblue")

#Regression line
abline(lm(leftfront ~ leftback), col="red", lwd = 3)

# Pearson correlation
text(paste("Correlation:", round(cor(leftfront, leftback), 2)), x = 25, y = 95)

# 2nd method

plot(leftfront ~ leftback,
     data=data,
     pch=16,
     xlab = "Front",
     ylab = "Back")

cor.test( ~ leftfront + leftback,
          data=data,
          method = "pearson")

model = lm(leftfront ~ leftback,
           data = data)
#summary(model)

x = residuals(model)
plotNormalHistogram(x)

abline(model,
       col = "blue",
       lwd = 2)

# 3rd method (Left and right foot separately)

library(ggplot2)
library(ggpubr)

left <- ggplot(data=data, aes(x=leftfront, y=leftback)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_cor(method = "pearson", label.x = 5.5, label.y = 22, size=4) + #Correlation p and R value
  stat_cor(aes(label=..rr.label..), label.x = 5.5, label.y=21, size=4) + #R squared
  stat_regline_equation(label.x=5.5, label.y=19.6, size=4) + #Regression equation
  xlab("Front view angle (°)") + ylab("Back view angle (°)") +
  ggtitle("Left foot") +
  theme(plot.title = element_text(hjust = 0.5, size=18), axis.title=element_text(size=15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

right <- ggplot(data=data, aes(x=rightfront, y=rightback)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_cor(method = "pearson", label.x = 5.5, label.y = 22, size=4) + #Correlation p and R value
  stat_cor(aes(label=..rr.label..), label.x = 5.5, label.y=21, size=4) + #R squared
  stat_regline_equation(label.x=5.5, label.y=19.6, size=4) + #Regression equation
  xlab("Front view angle (°)") + ylab("Back view angle (°)") +
  ggtitle("Right foot") +
  theme(plot.title = element_text(hjust = 0.5, size=18), axis.title=element_text(size=15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Combine both plot into one figure
figure <- ggarrange(left, right,
                   labels = c("a","b"),
                   ncol=2, nrow=1)

figure
#1000 x 594

# 4th method, combine left and right feet

library(ggplot2)
library(ggpubr)

figure <- ggplot(data=data, aes(x=combinefront, y=combineback)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_cor(method = "pearson", label.x = 5.5, label.y = 40, size=4) + #Correlation p and R value
  stat_cor(aes(label=..rr.label..), label.x = 5.5, label.y=38.8, size=4) + #R squared
  stat_regline_equation(label.x=5.5, label.y=37, size=4) + #Regression equation
  xlab("Front view angle (°)") + ylab("Back view angle (°)") +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5, size=18), axis.title=element_text(size=15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

figure

cor.test( ~ combinefront + combineback,
          data=data,
          method = "pearson")
#1000 x 594

# 5th method (USE THIS - 4 August 2022)
data2 <- read_excel('CombineDatasetForCorrelation.xlsx')

cor.test( ~ Front + Back,
          data=data2,
          method = "pearson")

ggplot(data2, aes(x=Front, y=Back)) +
  geom_smooth(method="lm", colour = 'black') +
  geom_point(aes(shape=Foot, colour=Foot)) +
  scale_color_manual(values=c("#FF6E00","#0000FF")) +
  stat_cor(method = "pearson", label.x = 5.5, label.y = 24, size=4, r.digits = 3,p.accuracy = 0.001, cor.coef.name = "r") +
  stat_cor(aes(label=..rr.label..), label.x = 5.5, label.y= 23, size=4, r.digits = 3) + #R squared
  stat_regline_equation(label.x=5.5, label.y=21.6, size=4) + #Regression equation
  xlab("Front view angle (°)") + ylab("Back view angle (°)") +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5, size=18), axis.title=element_text(size=15), legend.position=c(0.9,0.9), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
# 735 x 480

setwd("C:/Users/14000/Downloads/URECA/Final URECA files/Gait and Posture/Submitted/2nd Edit/Journal Of Applied Biomechanics/The Foot/Frontiers in Physiology") #<----------    to change accordingly

ggsave(
  "Figure7.jpeg",
  width = 1800,
  height = 1400,
  units = "px",
  dpi=300
)


ggscatter(data2, x="Front", y="Back",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Front view angle", ylab = "Back view angle")
