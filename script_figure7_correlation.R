# Plotting Figure 7 - Correlation and regression

library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(rcompanion)
library(ggplot2)
library(tidyr)

setwd("C:/Users/14000/Downloads/Publications/Frontiers in Bioengineering") 

# Read Excel file
df <- read_excel('Data_crossover study.xlsx', sheet='Angle Measurements', range="A1:E45")
df_long <- pivot_longer(df, cols = starts_with(c("Front_", "Back_")),
                        names_to = c(".value", "Foot"),
                        names_sep = "_")

# Map Foot values to "Left" or "Right"
df_long$Foot <- ifelse(df_long$Foot == "L", "Left", "Right")

# Correlation test
cor.test( ~ Front + Back,
          data=df_long,
          method = "pearson")

# Plot figure
plot <- ggplot(df_long, aes(x=Front, y=Back)) +
        geom_smooth(method="lm", colour = 'black') +
        geom_point(aes(shape=Foot, colour=Foot)) +
        scale_color_manual(values=c("#FF6E00","#0000FF")) +
        stat_cor(method = "pearson", label.x = 5.5, label.y = 24, size=4, r.digits = 3,p.accuracy = 0.001, cor.coef.name = "r") +
        stat_cor(aes(label=..rr.label..), label.x = 5.5, label.y= 23, size=4, r.digits = 3) + #R squared
        stat_regline_equation(label.x=5.5, label.y=21.6, size=4) + #Regression equation
        xlab("Front view angle (°)") + ylab("Back view angle (°)") +
        ggtitle("") +
        theme(plot.title = element_text(hjust = 0.5, size=18), axis.title=element_text(size=15), legend.position=c(0.9,0.9), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) # 735 x 480

# Show figure
windows();plot(plot) # If using VSCode to run script

# Change accordingly to the directory you want to save 
# setwd("C:/Users/14000/Downloads/Publications/Frontiers in Bioengineering") 

# Save plot to specific size
ggsave(
  "Figure7.jpeg",
  width = 1800,
  height = 1400,
  units = "px",
  dpi=300
)
