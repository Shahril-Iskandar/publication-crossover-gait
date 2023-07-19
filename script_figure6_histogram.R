# Plotting Figure 6 - Histogram data

library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plyr)

setwd("C:/Users/14000/Downloads/Publications/Frontiers in Bioengineering") 

# Read Excel file
pCharacteristic <- read_excel('Data_crossover study.xlsx', sheet='Participant Characteristics')
df <- read_excel('Data_crossover study.xlsx', sheet='Crossover')

# Copy column from one dataframe to another 
df$Sex <- pCharacteristic$Sex

# Filtered data if both columns contain 0
filtered_df <- df[!(df$`Left foot blocked` == 0 & df$`Right foot blocked` == 0), ] # Dropping if one column contains 0

# Rename column
colnames(filtered_df)[2] = "Left"
colnames(filtered_df)[3] = "Right"

# Dataframe for left side only
left_data <- filter(filtered_df, filtered_df$Left > 0)
left_data <- left_data[,-3]

# Dataframe for right side only
right_data <- filter(filtered_df, filtered_df$Right > 0)
right_data <- right_data[,-2]

# # 1st method (NOT USED - Each sub-figure only contain x-ticks in which they have values)
# left_plot <- ggplot(left_data, aes(factor(Left),fill=Sex), xlab="Left") +
#             geom_bar(width=.45) +
#             theme(plot.title = element_text(hjust = 0.5, size=18), axis.title=element_text(size=15), legend.position="none", 
#             panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
#             scale_fill_manual(values = c("#D9008D","#018288")) + 
#             ylim(0,4) + 
#             labs(y="Number of participants", x="Number of crossover gait") + 
#             ggtitle("Left foot")

# right_plot <- ggplot(right_data, aes(factor(Right),fill=Sex)) +
#             geom_bar(width=.45) +
#             theme(plot.title = element_text(hjust = 0.5, size=18), axis.title=element_text(size=15), legend.position="none", 
#             panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
#             scale_fill_manual(values = c("#D9008D","#018288")) + 
#             ylim(0,4) + 
#             labs(y="Number of participants", x="Number of crossover gait") + 
#             ggtitle("Right foot")

# # # Combine both plot into one figure
# figure <- ggarrange(left_plot, right_plot,
#                     labels = c("a","b"),
#                     ncol=2, nrow=1)
# # Show figure
# windows();plot(stuff) # If using VSCode to run script
# figure # 1000 x 594

#2nd method (USED IN PUBLICATION - Each sub-figure contains the same x-tick values)
left_plot <- ggplot(left_data, aes(factor(Left),fill=Sex), xlab="Left") + 
          geom_bar(width=.75)+ 
          theme(plot.title = element_text(hjust = 0.5, size=15), axis.title=element_text(size=15), legend.position="none", 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
          scale_fill_manual(values = c("#D9008D","#018288")) + 
          ylim(0,4) + 
          labs(y="Number of participants", x="Number of crossover gait") + 
          ggtitle("Left foot") + 
          scale_x_discrete(limits = c("1", "2", "3", "4", "7", "8", "10", "13"))

right_plot <- ggplot(right_data, aes(factor(Right),fill=Sex)) + 
          geom_bar(width=.75) + 
          scale_fill_manual(values = c("#D9008D","#018288")) + 
          labs(y="Number of participants", x="Number of crossover gait") + 
          theme(plot.title = element_text(hjust = 0.5, size=15), axis.title=element_text(size=15), legend.position=c(0.9,0.8), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
          ggtitle("Right foot") + 
          scale_x_discrete(limits = c("1", "2", "3", "4", "7", "8", "10", "13"))

# Combine both plot into one figure
figure <- ggarrange(left_plot, right_plot,
                    labels = c("(A)","(B)"),
                    ncol=2, nrow=1)

# Show figure
windows();plot(stuff) # If using VSCode to run script
figure

# Change accordingly to the directory you want to save 
# setwd("C:/Users/14000/Downloads/Publications/Frontiers in Bioengineering") 

# Save plot to specific size
ggsave(
  "Figure6.jpeg",
  width = 2000,
  height = 1200,
  units = "px",
  dpi=300
)

