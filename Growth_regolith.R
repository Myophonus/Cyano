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

data <- read.table("Regolith_Growth.txt", header = T)
data <- data[!grepl("10x", data$Substrate),]
#data <- data[!grepl("1x", data$Substrate),]
data2 <- data %>% 
  group_by(Cyanobacteria, Media, Substrate, Time) %>%
  summarise(across(everything(), mean))

#Se plotean las curvas

p <- ggplot(data = data, aes(x = Time, y = Intensity, color = Substrate, linetype = Media, shape = Media)) +
  #geom_point(size = 1.5) +
  geom_smooth(size = 1.5, se = T)  +
  geom_point(data = data2, size = 3)+
  facet_wrap("Cyanobacteria") +
  #facet_wrap("Cyanobacteria~Media") +
  scale_y_continuous(trans='log10', limits = c(5,40000))
  #scale_y_continuous(limits = c(5,3500))
  

p + labs(title = "Growth of three cyanobacteria in Martian regolith simulants", 
         x = "Time (days)", y = "Fluorescence intensity") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), 
        legend.text=element_text(size=12), legend.title=element_text(size=14))

#Se modelizan las curvas

data <- read.table("Regolith_Growth.txt", header = T)
data <- data[grepl("Anabaena_26", data$Cyanobacteria),]

data$Intensity <- log10(data$Intensity) #hago el log10 de la intensidad

data2 <- subset(data, !Time %in% c()) #elimino los primeros puntos porque queda clorofila y estropea el análisis

list <- data2 %>%
  group_split(Media, Substrate) #Divido la df en 42 dfs pequeñitas en base a dos variables

tg <- list[[1]]$Time
summG <- function(x) {SummarizeGrowth(tg,x$Intensity)} #hago un vector con los tiempos de crecimiento y una funcion que hace el analisis
fun <- function(x) {x$vals} #Esta funcion es para extraer los parámetros

models.all <- lapply(list, summG) #Ejecuto la función, se genera una lista con todo
models.all2 <- lapply (models.all, fun) #Saco los parámetros de esa lista

csv <- as.data.frame(do.call(cbind, models.all2)) #Uno la lista con parámetros resultante

plot(models.all[[4]])
models.all[[29]]$vals

#Se representan los parametros

param <- read.table("Regolith_Growth_Parameters.txt", header = T)
param <- param[!grepl("1x", param$Substrate),]
param <- param[!grepl("PRT", param$Substrate),]


group.colors <- c(MBL = "black", dH2O = "red")


p <- ggplot(data = param) +
  geom_point(aes(x = t_mid, y = r, color = Media, fill = Substrate, shape = Cyanobacteria )
             , size = 10, stroke = 1) +
  scale_color_manual(values=group.colors)+
  scale_shape_manual(values = c(21,22,23)) +
  scale_y_continuous(limits = c(0,0.35)) +
  scale_x_continuous(limits = c(0,30)) +
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  guides(color=guide_legend(override.aes=list(shape=21)))
#facet_wrap("Time_UV") +

#scale_y_continuous(limits = c(5,3500))


p + labs(title = "Parameters - Rego Growth", 
         x = "Inflexion point (days)", y = "Growth rate") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), 
        legend.text=element_text(size=12), legend.title=element_text(size=14))
