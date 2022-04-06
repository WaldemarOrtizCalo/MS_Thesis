
# Author: Waldemar Ortiz-Calo

# Date:2022-03-30 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(raster)
library(sf)
library(mapview)

#      Functions                                                            ####

#      Data                                                                 ####
#        [Deer]                                                             ####
deer <- read_csv(file = "1.DataManagement/CleanData/deer_all_clean.csv")

#        [Missouri Layers]                                                  ####
Missouri_boundaries <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp")

###############################################################################
#   State Map                                                               ####


#      [North]                                                              ####

points <- st_as_sf(deer,
                   coords = c("x","y"),
                   crs = 5070) %>% filter(site == "North")

intersection <- st_intersection(points,Missouri_boundaries)

counties_north <- Missouri_boundaries %>% filter(countyfips %in% unique(intersection$countyfips))

#      [South]                                                              ####

points <- st_as_sf(deer,
                   coords = c("x","y"),
                   crs = 5070) %>% filter(site == "South")

intersection <- st_intersection(points,Missouri_boundaries)

counties_south <- Missouri_boundaries %>% filter(countyfips %in% unique(intersection$countyfips))


#      [Southeast]                                                          ####

points <- st_as_sf(deer,
                   coords = c("x","y"),
                   crs = 5070) %>% filter(site == "CroplandStudy")

intersection <- st_intersection(points,Missouri_boundaries)

counties_southeast <- Missouri_boundaries %>% filter(countyfips %in% unique(intersection$countyfips))

#      [Map]                                                                ####


map <- ggplot()+
  geom_sf(data = Missouri_boundaries)+
  geom_sf(data = counties_north, fill = "#00AFBB")+
  geom_sf(data = counties_south, fill = "#E7B800")+
  geom_sf(data = counties_southeast, fill = "#FC4E07")+
  coord_sf(crs = st_crs(4326))+ # Changed the projection to make map straight
  theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
                plot.background = element_rect(fill = "transparent", color = NA))+
  theme_void()


ggsave(filename = "z.Figures/study_counties.png",
       plot = map,
       device = "png",
       width = 6,
       height = 5,
       units = "in",
       dpi = 300)


