#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-06-26 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(terra)
library(raster)
library(tidyverse)
library(mapview)
library(sf)

#      Functions                                                            ####

#      Data                                                                 ####

# Deer
deer <- read_csv(file = "1.DataManagement/CleanData/deer_all_clean.csv")

# Polygons
shp <- st_read("1.DataManagement/CleanData/shp_Missouri.shp")

# Rasters
NLCD <- rast("1.DataManagement/CovRasters/base_layers/southeast_nlcd.tif")
###############################################################################
#   [Study Area Statistics]                                                 ####

#      [Square Area]                                                        ####

# Number of cells in a raster
ncell(NLCD)

# Number of cells that are NA
freq(NLCD,value = NA)

# Number of Cells that have a value
ncell(NLCD) - 7890469

# Square Area of Study area (m^2)
(ncell(NLCD) - 7890469)*900

# Square Area of Study area (km^2)
((ncell(NLCD) - 7890469)*900)/1e6

###############################################################################
#   [Study Population Map]                                                  ####
#      [Subsetting Points used in the study area]                           ####

points <- st_as_sf(deer,
                   coords = c("x","y"),
                   crs = 5070) %>% filter(site == "CroplandStudy")

intersection <- st_intersection(points,shp)

counties_southeast <- shp %>% filter(countyfips %in% unique(intersection$countyfips))

#      [Map]                                                                ####

# Map Code
region_map <- ggplot() +
  geom_sf(data = shp, color = "black")+
  geom_sf(data = counties_southeast,color = "black",  fill = "#FC4E07")+
  coord_sf(crs = st_crs(4326))+
  theme_bw()

# Map Export
ggsave(filename = "z.Figures/Missouri_StudyAreaSoutheast.png",
       plot = region_map,
       device = "png",
       width = 6,
       height = 5,
       units = "in",
       dpi = 300)



###############################################################################
