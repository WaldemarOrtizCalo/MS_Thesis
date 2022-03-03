#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-02-23 

# Purpose: Used to Available Analysis

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(raster)
library(tidyverse)
library(adehabitatHR)
library(mapview)
library(FedData)
library(lubridate)
library(foreach)

#      Functions                                                            ####

# Simulation Function
source("2.Chapter1\\2.Functions\\Used2Available_sim.R")

#      Data                                                                 ####

#        [Deer]                                                             ####

df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

df_deer$quarteryear <- quarter(df_deer$t,
                               type = "year.quarter")

#        [NLCD]                                                             ####

NLCD_Missouri <- raster("1.DataManagement\\CleanData\\NLCD_Missouri.tif")
NLCD_available_north <- raster("1.DataManagement\\CleanData\\NLCD_available_north.tif")
NLCD_available_south <- raster("1.DataManagement\\CleanData\\NLCD_available_south.tif")
NLCD_available_southeast <- raster("1.DataManagement\\CleanData\\NLCD_available_southeast.tif")

#        [DEM and derivative layers]                                        ####

Topo_Missouri <- stackOpen("1.DataManagement/CleanData/Topo_Missouri.stk")
Topo_available_north <- stackOpen("1.DataManagement/CleanData/Topo_available_north.stk")
Topo_available_south <- stackOpen("1.DataManagement/CleanData/Topo_available_south.stk")
Topo_available_southeast <- stackOpen("1.DataManagement/CleanData/Topo_available_southeast.stk")

#        [Missouri Shapefile]                                               ####

shp_Missouri <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp")

#        [Covariate Stack]                                                  ####

covariate_stack <- stack(NLCD_Missouri,Topo_Missouri)

###############################################################################
#   [Testing]                                              ####
t <- Used2Available_sim(pop_df = df_deer,
                        year_quarter = "2016.1",
                        site.name = "North",
                        raster_stack = covariate_stack,
                        n.avail.range = 1:3,
                        n.sim = 10,
                        export_sim = T,
                        export_dir = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Data")

res <- t

#      [Topo - DensityPlots]                                                ####

density_plot_Elevation <- ggplot(data = res, 
                                aes(x = DEM_Missouri,group = SimID))+
  geom_density(size = 1, colour=alpha("black", 0.3))+
  theme_classic()+
  xlab("Elevation") +
  facet_grid(rows = vars(n.avail))

ggsave(paste0(site.name,"_",year_quarter,"_DenPlot_","Elevation.jpeg"),
       plot = density_plot_Elevation,
       device = "jpeg",
       path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)

density_plot_Slope <- ggplot(data = res, 
                             aes(x = DEM_Slope,group = SimID))+
  geom_density(size = 1, colour=alpha("black", 0.3))+
  theme_classic()+
  xlab("Slope")+
  facet_grid(rows = vars(n.avail))

ggsave(paste0(site.name,"_",year_quarter,"_DenPlot_","Slope.jpeg"),
       plot = density_plot_Slope,
       device = "jpeg",
       path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)

density_plot_Aspect <- ggplot(data = res, 
                             aes(x = DEM_Aspect,group = SimID))+
  geom_density(size = 1, colour=alpha("black", 0.3))+
  theme_classic()+
  xlab("Aspect")+
  facet_grid(rows = vars(n.avail))

ggsave(paste0(site.name,"_",year_quarter,"_DenPlot_","Aspect.jpeg"),
       plot = density_plot_Aspect,
       device = "jpeg",
       path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)

density_plot_TRI <- ggplot(data = res, 
                             aes(x = DEM_TRI,group = SimID))+
  geom_density(size = 1, colour=alpha("black", 0.3))+
  theme_classic()+
  xlab("TRI")+
  facet_grid(rows = vars(n.avail))

ggsave(paste0(site.name,"_",year_quarter,"_DenPlot_","TRI.jpeg"),
       plot = density_plot_TRI,
       device = "jpeg",
       path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)


#      [Topo - Boxplots]                                                    ####

box_plot_Elevation <- ggplot(data = res, 
       aes(x = as.character(n.avail),y = DEM_Missouri))+
  geom_boxplot()+
  theme_classic()+
  xlab("Used to Available Ratio 1:x")+
  ylab("Elevation")

ggsave(paste0(site.name,"_",year_quarter,"_BoxPlot_","Elevation.jpeg"),
       plot = box_plot_Elevation,
       device = "jpeg",
       path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)

box_plot_Slope <- ggplot(data = res, 
                             aes(x = as.character(n.avail),y = DEM_Slope))+
  geom_boxplot()+
  theme_classic()+
  xlab("Used to Available Ratio 1:x")+
  ylab("Slope")

ggsave(paste0(site.name,"_",year_quarter,"_BoxPlot_","Slope.jpeg"),
       plot = box_plot_Slope,
       device = "jpeg",
       path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)

box_plot_Aspect <- ggplot(data = res, 
                             aes(x = as.character(n.avail),y = DEM_Aspect))+
  geom_boxplot()+
  theme_classic()+
  xlab("Used to Available Ratio 1:x")+
  ylab("Aspect")

ggsave(paste0(site.name,"_",year_quarter,"_BoxPlot_","Aspect.jpeg"),
       plot = box_plot_Aspect,
       device = "jpeg",
       path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)

box_plot_TRI <- ggplot(data = res, 
                             aes(x = as.character(n.avail),y = DEM_TRI))+
  geom_boxplot()+
  theme_classic()+
  xlab("Used to Available Ratio 1:x")+
  ylab("TRI")

ggsave(paste0(site.name,"_",year_quarter,"_BoxPlot_","TRI.jpeg"),
       plot = box_plot_TRI,
       device = "jpeg",
       path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)

#      [NLCD - Boxplots]                                                    ####


res <- res %>% group_by(SimID,n.avail)


legend <- pal_nlcd()
legend <- data.frame("NLCD_Missouri" = legend$code,
                     "Category" = legend$class)

res$NLCD_Missouri<-legend[match(res$NLCD_Missouri, legend$NLCD_Missouri),2]

freq_table <- res %>% 
  with(table(SimID, NLCD_Missouri)) %>% 
  as.data.frame() %>% 
  group_by(SimID) %>% 
  mutate("Total" = sum(Freq)) %>% 
  mutate("Proportion" = round(Freq/Total, digits = 3)) 

NLCD_plot <-ggplot(freq_table,aes(x = NLCD_Missouri, y = Proportion))+
  geom_boxplot() +
  theme_classic()+
  xlab("NLCD Category")

ggsave(paste0(site.name,"_",year_quarter,"_","NLCD.jpeg"),
       plot = NLCD_plot,
       device = "jpeg",
       path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)
