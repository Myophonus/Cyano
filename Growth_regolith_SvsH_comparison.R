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


#Representar los parámetros del modelo - no todos

param <- read.table("Regolith_UV_Parameters_All_Mod_Mont.txt", header = T) #he rebajado el growth rate de Monmorillonite Wet porque se salia
param <- param[!grepl("PRT", param$Substrate),]
#param <- param[!grepl("None", param$Substrate),]
#param <- param[grepl("Anabaena_26", param$Cyanobacteria),]
#param <- param[grepl("None", param$Time_UV),]


group.colors <- c(Wet = "black", Dry = "red")


p <- ggplot(data = param) +
  geom_point(aes(x = t_mid, y = r, color = Condition, fill = Substrate), 
             shape = 21, size = 10, stroke = 1) +
  scale_color_manual(values=group.colors)+
  scale_fill_discrete(limits=c("None", "MGS-1", "MMS-2","Monmorillonite","Nontronite")) +
  scale_fill_brewer(palette = "Set2", limits=c("None", "MGS-1", "MMS-2","Monmorillonite","Nontronite")) +
  scale_y_continuous(limits = c(0.01,1)) +
  scale_x_continuous(limits = c(0,42)) 
  #facet_wrap("Time_UV") +
  
#scale_y_continuous(limits = c(5,3500))


p + labs(title = "Parameters - Rego Humid vs Dry - Anabaena 26", 
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
  scale_y_continuous(limits = c(0,15)) +
  facet_wrap("Substrate")



p + labs(title = "Regolith Nostoc 10 Inflexion Day by UV", 
         x = "Irradiation time", y = "Inflexion point (days)") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),
        plot.title=element_text(size=16), legend.key.size = unit(1, 'cm'), 
        legend.text=element_text(size=10), legend.title=element_text(size=12))


#Representar los cambios en parámetros a t_uv 0 en todos los casos


param <- read.table("Final param.txt", header = T)
param <- param[grepl("None", param$Time_UV),]
param <- param[!grepl("None", param$Substrate),]
param <- param[!grepl("PRT", param$Substrate),]

level_order <- c("None", "1min", "10min","1h","2h","3h","4h")
group.colors <- c(Wet = "black", Dry = "red")



p <- ggplot(data = param, aes(x = Condition, y = t_mid, fill = Condition)) +
  geom_col() +
  scale_fill_grey(start = 0.3, end = 0.7) +
  scale_y_continuous(limits = c(0.5,40), trans = "log10") +
  facet_grid(vars(Substrate), vars(Cyanobacteria, levels = c(rep("Anabaena_26", 4),
                                        rep("Nostoc_10",8), rep("Anabaena_26", 4)), 
                                   labels = c(rep("*Desmonostoc* sp",4), 
                                  rep("*Desmonostoc muscorum* UTAD N213",8), rep("*Desmonostoc* sp", 4)))) +
  geom_errorbar(aes(ymin = t_mid - tmidse, ymax = t_mid + tmidse), width = 0.5)


p + labs(title = "", 
         x = "Water presence upon irradiation", y = "Inflexion point (days)") +
  theme_light() +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),
        plot.title=element_text(size=16), legend.key.size = unit(1, 'cm'), legend.title=element_text(size=12), 
        legend.position = "none", strip.text.x = element_markdown(lineheight = 1, size =12))
