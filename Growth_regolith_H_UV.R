setwd("C:/Users/mglar/Desktop/Doctorado/R/Placas/Cyanobacteria growth regolith - Antonio/Final graphs")
library(ggplot2)
library(tidyverse)
library(dplyr)
library("RColorBrewer")
library(vctrs)
library(rstatix)
library(ggpubr)
library(zoo)
library(broom)
library(mgcv)
library(ggbump)
library(growthcurver)
library(growthrates)
library(concaveman)




#datos de una de las cianos
data <- read.table("Regolith_Humid_UV_Growth.txt", header = T)
data <- data[!grepl("Anabaena_26", data$Cyanobacteria),]


#se hace los mismo pero logarítmico y del d1 al d67 para Nostoc 10

data <- read.table("Regolith_Humid_UV_Growth.txt", header = T)
data <- data[grepl("Nostoc_10", data$Cyanobacteria),]

data$Intensity <- log10(data$Intensity) #hago el log10 de la intensidad

data2 <- subset(data, !Time_growth %in% c()) #elimino los primeros puntos porque queda clorofila y estropea el análisis

list <- data2 %>%
  group_split(Substrate, Time_UV) #Divido la df en 42 dfs pequeñitas en base a dos variables

tg <- list[[1]]$Time_growth
summG <- function(x) {SummarizeGrowth(tg,x$Intensity)} #hago un vector con los tiempos de crecimiento y una funcion que hace el analisis
fun <- function(x) {x$vals} #Esta funcion es para extraer los parámetros

models.all <- lapply(list, summG) #Ejecuto la función, se genera una lista con todo
models.all2 <- lapply (models.all, fun) #Saco los parámetros de esa lista

csv <- as.data.frame(do.call(cbind, models.all2)) #Uno la lista con parámetros resultante

plot(models.all[[20]])
models.all[[29]]$vals


#se hace los mismo pero logarítmico y del d1 al d67 para Anabaena 26

data <- read.table("Regolith_Humid_UV_Growth.txt", header = T)
data <- data[!grepl("Nostoc_10", data$Cyanobacteria),]

data$Intensity <- log10(data$Intensity) #hago el log10 de la intensidad

data2 <- subset(data, !Time_growth %in% c()) #elimino los primeros puntos porque queda clorofila y estropea el análisis

list <- data2 %>%
  group_split(Substrate, Time_UV) #Divido la df en 42 dfs pequeñitas en base a dos variables

tg <- list[[1]]$Time_growth
summG <- function(x) {SummarizeGrowth(tg,x$Intensity)} #hago un vector con los tiempos de crecimiento y una funcion que hace el analisis
fun <- function(x) {x$vals} #Esta funcion es para extraer los parámetros

models.all <- lapply(list, summG) #Ejecuto la función, se genera una lista con todo
models.all2 <- lapply (models.all, fun) #Saco los parámetros de esa lista

csv <- as.data.frame(do.call(cbind, models.all2)) #Uno la lista con parámetros resultante

plot(models.all[[9]])
models.all[[29]]$vals


#Para Nostoc 10 curvas

data <- read.table("Regolith_Humid_UV_Growth.txt", header = T)
data <- data[!grepl("Anabaena_26", data$Cyanobacteria),]

data2 <- data %>% 
  group_by(Time_growth, Time_UV, Substrate) %>%
  summarise(across(everything(), median))

p <- ggplot(data = data, aes(x = Time_growth, y = Intensity, color = Time_UV)) +
  scale_color_discrete(limits=c("None", "1min", "10min","1h","2h","3h","4h")) +
  scale_color_brewer(palette = "RdYlGn", limits=c("None", "1min", "10min","1h","2h","3h","4h"), 
                     direction = -1) +
  geom_point(data = data2, aes(x = Time_growth, y = Intensity, color = Time_UV)) +
  geom_smooth(method="loess", size = 1.5, se = F)  +
  facet_wrap("Substrate") +
  #facet_wrap("Time_UV") +
  scale_y_continuous(trans='log10', limits = c(1,40000))

#scale_y_continuous(limits = c(5,3500))


p + labs(title = "", 
         x = "Time (days)", y = "Fluorescence intensity") +
  theme_light() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), 
        legend.text=element_text(size=12), legend.title=element_text(size=14))



#Para Anabaena 26 curvas

data <- read.table("Regolith_Humid_UV_Growth.txt", header = T)
data <- data[!grepl("Nostoc_10", data$Cyanobacteria),]

data2 <- data %>% 
  group_by(Time_growth, Time_UV, Substrate) %>%
  summarise(across(everything(), median))

p <- ggplot(data = data, aes(x = Time_growth, y = Intensity, color = Time_UV)) +
  scale_color_discrete(limits=c("None", "1min", "10min","1h","2h","3h","4h")) +
  scale_color_brewer(palette = "RdYlGn", limits=c("None", "1min", "10min","1h","2h","3h","4h"), 
                     direction = -1) +
  geom_point(data = data2, aes(x = Time_growth, y = Intensity, color = Time_UV)) +
  geom_smooth(method="loess", size = 1.5, se = F)  +
  facet_wrap("Substrate") +
  #facet_wrap("Time_UV") +
  scale_y_continuous(trans='log10', limits = c(1,40000))
  
  #scale_y_continuous(limits = c(5,3500))
  

p + labs(title = "Growth of Anabaena_26 in five substrates after irradiation under humid conditions", 
         x = "Time (days)", y = "Fluorescence intensity") +
    theme_light() +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), 
        legend.text=element_text(size=12), legend.title=element_text(size=14))



#Representar los parámetros del modelo - no todos

param <- read.table("Regolith_Humid_UV_Parameters.txt", header = T)
param <- param[!grepl("PRT", param$Substrate),]


group.colors <- c(Anabaena_26 = "black", Nostoc_10 = "red")


p <- ggplot(data = param) +
  geom_point(aes(x = t_mid, y = r, color = Cyanobacteria, fill = Substrate), 
             shape = 21, size = 10, stroke = 1) +
  scale_color_manual(values=group.colors)+
  scale_fill_discrete(limits=c("None", "MGS-1", "MMS-2","Monmorillonite","Nontronite")) +
  scale_fill_brewer(palette = "Set2", limits=c("None", "MGS-1", "MMS-2","Monmorillonite","Nontronite")) +
  scale_y_continuous(limits = c(0,0.5)) +
  scale_x_continuous(limits = c(0,30)) 
  #facet_wrap("Time_UV") +
  
#scale_y_continuous(limits = c(5,3500))


p + labs(title = "Parameters - Rego Humid UV - No UV", 
         x = "Inflexion point (days)", y = "Growth rate") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), 
        legend.text=element_text(size=12), legend.title=element_text(size=14))




#Representar los parámetros del modelo - todos
param <- read.table("Regolith_Humid_UV_Parameters.txt", header = T)
param <- param[!grepl("PRT", param$Substrate),]
#param <- param[!grepl("Nostoc_10", param$Cyanobacteria),]

p <- ggplot(data = param) +
  geom_point(aes(x = t_mid, y = r, color = Time_UV, fill = Substrate, shape = Cyanobacteria), size = 8, stroke = 2) +
  scale_shape_manual(values= c(21, 23)) +
  scale_color_discrete(limits=c("None", "1min", "10min","1h","2h","4h","72h")) +
  scale_color_brewer(palette = "RdYlGn", limits=c("None", "1min", "10min","1h","2h","3h","4h"), 
                     direction = -1) +
  scale_fill_discrete(limits=c("None", "MGS-1", "MMS-2","Monmorillonite","Nontronite")) +
  scale_fill_brewer(palette = "Set1", limits=c("None", "MGS-1", "MMS-2","Monmorillonite","Nontronite")) +
  scale_y_continuous(limits = c(0,3)) +
  scale_x_continuous(limits = c(0,30)) +
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  guides(color=guide_legend(override.aes=list(shape=21)))
#facet_wrap("Time_UV") +

#scale_y_continuous(limits = c(5,3500))


p + labs(title = "Parameters - Regolith UV", 
         x = "Inflexion point (days)", y = "Growth rate") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),
        plot.title=element_text(size=16), legend.key.size = unit(1, 'cm'), 
        legend.text=element_text(size=10), legend.title=element_text(size=12))


#Respresentar la evolución del tiempo med en base a UV

param <- read.table("Regolith_Humid_UV_Parameters.txt", header = T)
param <- param[grepl("Nostoc_10", param$Cyanobacteria),]

level_order <- c("None", "1min", "10min","1h","2h","3h","4h") 

p <- ggplot(data = param, aes(x = factor(Time_UV, level = level_order), y = t_mid, fill = Substrate)) +
  geom_col() +
  scale_fill_discrete(limits=c("None", "MGS-1", "MMS-2","Monmorillonite","Nontronite")) +
  scale_fill_brewer(palette = "Set2", limits=c("None", "MGS-1", "MMS-2","Monmorillonite","Nontronite")) +
  scale_y_continuous(limits = c(0,15)) +
  facet_wrap("Substrate")



p + labs(title = "Regolith Nostoc 10 Inflexion Day by UV", 
         x = "Irradiation time", y = "Inflexion point (days)") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),
        plot.title=element_text(size=16), legend.key.size = unit(1, 'cm'), 
        legend.text=element_text(size=10), legend.title=element_text(size=12))




