#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-02-18 

# Purpose: To clean and organize Rasters for the project

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####

library(raster)
library(tidyverse)
library(sf)
library(mapview)

#      Functions                                                            ####

#      Data                                                                 ####

# NLCD Raster
NLCD_raw <- raster("1.DataManagement\\RawData\\RasterData\\nlcd_2016_land_cover_l48_20210604.img")
n <- NLCD_raw
# Deer Data 

df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

# Missouri Shapefiles

Missouri_shp <- st_read("1.DataManagement\\RawData\\Shapefiles\\Missouri_Counties.shp")

###############################################################################
#   [Standardizing Projections]                                             ####
#      [NLCD]                                                               ####

# Current CRS 
crs(NLCD_raw)

#      [Missouri Shp]                                                       ####

# Current CRS 
crs(Missouri_shp)

# Transforming Projections
shp_Missouri <- Missouri_shp %>% st_transform(5070)

# Checking new CRS 
crs(shp_Missouri)


###############################################################################
#   [Export: NLCD - Missouri]                                               ####
#      [Cropping]                                                           ####

# Cropping National NLCD to Missouri Extent
NLCD_Missouri<- crop(NLCD_raw,extent(Missouri_shp))

# Visually inspecting
plot(NLCD_Missouri)


#      [Saving and Exporting]                                               ####

# Saving Cropped File

writeRaster(NLCD_Missouri,"1.DataManagement/CleanData/NLCD_Missouri.tif")

# Checking if it works

MTif <- raster("1.DataManagement/CleanData/NLCD_Missouri.tif")

plot(MTif)

###############################################################################
#   [Export: Missouri - Shapefile]                                          ####
#      [Exporting]                                                          ####
st_write(shp_Missouri,
         dsn = "1.DataManagement/CleanData/shp_Missouri.shp",
         driver = "ESRI Shapefile")

#      [Checking]                                                           ####
MShp <- st_read("1.DataManagement/CleanData/shp_Missouri.shp")

###############################################################################