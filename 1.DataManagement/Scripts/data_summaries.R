#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-04-21 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(sf)
library(mapview)

#      Functions                                                            ####

#      Data                                                                 ####
state <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp")%>% 
  st_transform(4326)
north <- st_read("1.DataManagement\\shapefiles\\north_studyarea.shp") %>% 
  st_transform(4326)
south <- st_read("1.DataManagement\\shapefiles\\south_studyarea.shp") %>% 
  st_transform(4326)

###############################################################################
#   Study Area Map                                                          ####
study_area <- ggplot() +
  geom_sf(data = state)+
  geom_sf(data = north,
          color = "black",
          fill = "#CDAD00")+
  geom_sf(data = south,
          color = "black",
          fill = "#008B00")+
  theme_bw()

ggsave(filename = "1.DataManagement/StudyAreaDescriptions/viz/studyarea.png",
       plot = study_area,
       device = "png",
       width = 6,
       height = 4,
       units = "in")
