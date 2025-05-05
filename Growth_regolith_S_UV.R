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
library(ggtext)

#datos de una de las cianos
data <- read.table("Regolith_UV_Growth.txt", header = T)
data <- data[!grepl("Anabaena_26", data$Cyanobacteria),]


#se hace los mismo pero logarítmico y del d10 al d67 para Nostoc 10

data <- read.table("Regolith_UV_Growth.txt", header = T)
data <- data[!grepl("Nostoc_10", data$Cyanobacteria),]

data$Intensity <- log10(data$Intensity) #hago el log10 de la intensidad

data2 <- subset(data, !Time_growth %in% c(0)) #elimino los primeros puntos porque queda clorofila y estropea el análisis

list <- data2 %>%
  group_split(Substrate, Time_UV) #Divido la df en 42 dfs pequeñitas en base a dos variables

tg <- list[[1]]$Time_growth
summG <- function(x) {SummarizeGrowth(tg,x$Intensity)} #hago un vector con los tiempos de crecimiento y una funcion que hace el analisis
fun <- function(x) {x$vals} #Esta funcion es para extraer los parámetros

models.all <- lapply(list, summG) #Ejecuto la función, se genera una lista con todo
models.all2 <- lapply (models.all, fun) #Saco los parámetros de esa lista

csv <- as.data.frame(do.call(cbind, models.all2)) #Uno la lista con parámetros resultante


plot(models.all[[3]])
models.all[[29]]$vals
predict(models.all[[3]]$model)

#se hace los mismo pero logarítmico y del d3 al d67 para Anabaena 26

data <- read.table("Regolith_UV_Growth.txt", header = T)
data <- data[!grepl("Nostoc_10", data$Cyanobacteria),]

data$Intensity <- log10(data$Intensity) #hago el log10 de la intensidad

data2 <- subset(data, !Time_growth %in% c(0)) #elimino los primeros puntos porque queda clorofila y estropea el análisis

list <- data2 %>%
  group_split(Substrate, Time_UV) #Divido la df en 42 dfs pequeñitas en base a dos variables

tg <- list[[1]]$Time_growth
summG <- function(x) {SummarizeGrowth(tg,x$Intensity)} #hago un vector con los tiempos de crecimiento y una funcion que hace el analisis
fun <- function(x) {x$vals} #Esta funcion es para extraer los parámetros

models.all <- lapply(list, summG) #Ejecuto la función, se genera una lista con todo
models.all2 <- lapply (models.all, fun) #Saco los parámetros de esa lista

csv <- as.data.frame(do.call(cbind, models.all2)) #Uno la lista con parámetros resultante

plot(models.all[[32]])
models.all[[29]]$vals


#Para Nostoc 10 curvas

data <- read.table("Regolith_UV_Growth.txt", header = T)
data <- data[!grepl("Anabaena_26", data$Cyanobacteria),]

data2 <- data %>% 
  group_by(Time_growth, Time_UV, Substrate) %>%
  summarise(across(everything(), median))


p <- ggplot(data = data, aes(x = Time_growth, y = Intensity/max(Intensity), color = Time_UV)) +
  scale_color_discrete(limits=c("None", "1min", "10min","1h","2h","4h","72h")) +
  scale_color_brewer(palette = "RdYlGn", limits=c("None", "1min", "10min","1h","2h","4h","72h"), 
                     direction = -1) +
  geom_point(data = data2, aes(x = Time_growth, y = Intensity/max(Intensity), color = Time_UV)) +
  facet_wrap("Substrate") +
  stat_smooth(method = "loess", se =F, size = 1.5) +
  scale_y_continuous(labels = function(x) x * max(data$Intensity), trans = "log10")



p + labs(title = "", 
         x = "Time (days)", y = "Fluorescence intensity") +
  theme_light() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), 
        legend.text=element_text(size=12), legend.title=element_text(size=14))



#Para Anabaena 26 curvas

data <- read.table("Regolith_UV_Growth.txt", header = T)
data <- data[!grepl("Nostoc_10", data$Cyanobacteria),]

data2 <- data %>% 
  group_by(Time_growth, Time_UV, Substrate) %>%
  summarise(across(everything(), median))

p <- ggplot(data = data, aes(x = Time_growth, y = Intensity, color = Time_UV)) +
  scale_color_discrete(limits=c("None", "1min", "10min","1h","2h","4h","72h")) +
  scale_color_brewer(palette = "RdYlGn", limits=c("None", "1min", "10min","1h","2h","4h","72h"), 
                     direction = -1) +
  geom_point(data = data2, aes(x = Time_growth, y = Intensity, color = Time_UV)) +
  stat_smooth(method="loess", size = 1.5, se = F)  +
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



#Representar los parámetros del modelo - Seco

param <- read.table("Final param.txt", header = T)
param <- param[grepl("Dry", param$Condition),]
param <- param[!grepl("PRT", param$Substrate),]
param <- param[!grepl("72h", param$Time_UV),]

group.colors <- c(Anabaena_26 = "black", Nostoc_10 = "red")


p <- ggplot(data = param, aes(x = t_mid, y = r, color = Cyanobacteria, fill = Substrate)) +
  geom_point(aes(x = t_mid, y = r, color = Cyanobacteria, fill = Substrate, size = rse), 
             shape = 21, stroke = 1) +
  scale_size_continuous(range = c(3,30), guide = "none") +
  scale_color_manual(values=group.colors, labels=c("*Desmonostoc* sp.", "*Desmonostoc muscorum* UTAD N213"))+
  scale_fill_discrete(limits=c("None", "MGS-1", "MMS-2","Montmorillonite","Nontronite")) +
  scale_fill_brewer(palette = "Set2", limits=c("None", "MGS-1", "MMS-2","Montmorillonite","Nontronite")) +
  scale_y_continuous(limits = c(0.01,0.9)) +
  scale_x_continuous(limits = c(0.01,50)) +
  theme_classic()
  

p + labs(title = "", x = "Inflexion point (days)", y = "Growth rate (r)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), legend.title=element_text(size=14),
        legend.text = element_markdown(lineheight = 1, size =14)) +
  guides(fill = guide_legend(override.aes = list(size = 10))) +
  guides(color = guide_legend(override.aes = list(size = 10)))


#Representar los parámetros del modelo - Desmonostoc sp o muscorum

param <- read.table("Final param.txt", header = T)
param <- param[grepl("Nostoc_10", param$Cyanobacteria),]
param <- param[!grepl("PRT", param$Substrate),]
param <- param[!grepl("72h", param$Time_UV),]

group.colors <- c(Dry = "red", Wet = "black")

p <- ggplot(data = param, aes(x = t_mid, y = r, color = Condition, 
                              fill = Substrate, size = rse)) +
  geom_point(aes(x = t_mid, y = r, color = Condition, fill = Substrate), 
             shape = 21, stroke = 1) +
  scale_size_continuous(range = c(3,30), guide = "none") +
  scale_color_manual(values=group.colors, labels=c("Dry", "Humid"))+
  scale_fill_discrete(limits=c("None", "MGS-1", "MMS-2","Montmorillonite","Nontronite")) +
  scale_fill_brewer(palette = "Set2", limits=c("None", "MGS-1", "MMS-2","Montmorillonite","Nontronite")) +
  scale_y_continuous(limits = c(0.01,3)) +
  scale_x_continuous(limits = c(0.01, 40)) +
  theme_classic()

#scale_y_continuous(limits = c(5,3500))

p + labs(title = "", x = "Inflexion point (days)", y = "Growth rate (r)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title=element_text(size=20), legend.key.size = unit(1.5, 'cm'), legend.title=element_text(size=14),
        legend.text = element_markdown(lineheight = 1, size =14)) +
  guides(fill = guide_legend(override.aes = list(size = 10))) +
  guides(color = guide_legend(override.aes = list(size = 10)))



#Respresentar la evolución del tiempo med en base a UV

param <- read.table("Final param.txt", header = T)
param <- param[grepl("Nostoc_10", param$Cyanobacteria),]
param <- param[grepl("Dry", param$Condition),]

level_order <- c("None", "1min", "10min","1h","2h","4h","72h") 

ylim.prim <- c(0.01, 60)
ylim.sec <- c(0, 1)

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

p <- ggplot(data = param, aes(x = factor(Time_UV, level = level_order), y = t_mid)) +
  geom_col(aes (fill = Substrate)) +
  scale_fill_discrete(limits=c("None", "MGS-1", "MMS-2","Montmorillonite","Nontronite")) +
  scale_fill_brewer(palette = "Set2", limits=c("None", "MGS-1", "MMS-2","Montmorillonite","Nontronite")) +
  scale_y_continuous(limits = c(0,60), sec.axis = sec_axis(~ (. - a)/b, name = "Growth rate (r)")) +
  facet_wrap("Substrate") +
  geom_errorbar(aes(ymin = t_mid - tmidse, ymax = t_mid + tmidse), width = 0.5) +
  geom_point(aes(x =factor(Time_UV, level = level_order), y = a + r*b)) +
  geom_line(aes(x =factor(Time_UV, level = level_order), y = a + r*b, group = 1)) + 
  geom_errorbar(aes(ymin = a + r*b - rse*b, ymax = a + r*b + rse*b), width = 0.5)



p + labs(title = "", 
         x = "Irradiation time", y = "Inflexion point (days)") +
  theme_light() +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),
        plot.title=element_text(size=16), legend.key.size = unit(1, 'cm'), 
        legend.text=element_text(size=10), legend.title=element_text(size=12), 
        legend.position = "none")




