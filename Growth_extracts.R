setwd("C:/Users/mglar/Desktop/Doctorado/R/Placas/Cyanobacteria growth regolith - Antonio")
library(ggplot2)
library(ggplot2)
library(tidyverse)
library(dplyr)
library("RColorBrewer")
library(vctrs)
library(rstatix)
library(ggpubr)
library(zoo)

data <- read.table("Extract_Growth.txt", header = T)
data <- data[!grepl("10x", data$Substrate),]
#data <- data[!grepl("1x", data$Substrate),]

data2 <- data %>% 
  group_by(Cyanobacteria, Media, Substrate, Time) %>%
  summarise(across(everything(), mean))

data3 <- rbind(data2, data2, data2, data2, data2, data2)

p <- ggplot(data, aes(x = Time, y = Intensity, color = Substrate)) +
  geom_smooth(size = 1.5, se = T)  + 
  geom_point(data = data2, size = 3)+
  facet_grid(vars(Media), vars(Cyanobacteria)) +
  scale_y_continuous(trans='log10', limits = c(100,50000))+
  xlim(0, 70)
  

p + labs(title = "", 
         x = "Time (days)", y = "Fluorescence intensity") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), 
        legend.text=element_text(size=12), legend.title=element_text(size=14), 
        strip.text.x = element_markdown(lineheight = 1, size =12))

