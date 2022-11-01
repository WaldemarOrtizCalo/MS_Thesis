#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-10-31 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(terra)
library(sf)
library(tidyverse)
library(raster)

#      Functions                                                            ####
source("2.Chapter1/2.Functions/reclass_matrices.R")
source("2.Chapter1/2.Functions/proportion_raster_function.R")
unregister <- function() {
  
  # This function is to unregister the parallel backend of the doParallel and
  # foreach loop. Original source: https://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
  
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
  
}

#      Data                                                                 ####
#      Data [DO NOT RUN - Only Run Once]                                    ####
#        [Deer Data]                                                        ####

deer_all <- lapply(list.files(path = "1.DataManagement/CleanData/Chapter1_UsedAvailableLocations",
                              full.names = T), read_csv)

deer_all  <- do.call(rbind,deer_all)


#        [Raster Data]                                                      ####

#      [NLCD Data]                                                          
Missouri_NLCD <- raster("1.DataManagement/CleanData/NLCD_Missouri.tif") %>% ratify()

#      [DEM Data]                                                           
Missouri_DEM <- raster("1.DataManagement\\CleanData\\DEM_Missouri.tif")

#      [Missouri Shapefiles]
Missouri_shp <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp") 

#        [SF objects]                                                       ####

deer_sf_north <- subset(deer_all, site == "North") %>% st_as_sf(coords = c("x", "y"), crs = 5070)
deer_sf_south <- subset(deer_all, site == "South") %>% st_as_sf(coords = c("x", "y"), crs = 5070)
deer_sf_southeast <- subset(deer_all, site == "CroplandStudy") %>% st_as_sf(coords = c("x", "y"), crs = 5070)

#        [Study Area Subsets shapefiles]                                    ####
North_StudyArea <- st_intersects(Missouri_shp,deer_sf_north)
North_StudyArea <- Missouri_shp[which(lengths(North_StudyArea)!=0),]

South_StudyArea <- st_intersects(Missouri_shp,deer_sf_south)
South_StudyArea <- Missouri_shp[which(lengths(South_StudyArea)!=0),]

Southeast_StudyArea <- st_intersects(Missouri_shp,deer_sf_southeast)
Southeast_StudyArea <- Missouri_shp[which(lengths(Southeast_StudyArea)!=0),]

#        [Study Area Exports]                                               ####

# North
NLCD_North <- crop(Missouri_NLCD,North_StudyArea) %>% mask(North_StudyArea) %>% 
  ratify() %>% 
  reclassify(reclass_matrixNorth)

writeRaster(NLCD_North,
            filename = paste0("1.DataManagement/CovRasters/base_layers/north_nlcd.tif"),
            overwrite = T)

DEM_North <- crop(Missouri_DEM,North_StudyArea) %>% mask(North_StudyArea)

writeRaster(DEM_North,
            filename = paste0("1.DataManagement/CovRasters/base_layers/north_dem.tif"),
            overwrite = T)

# South
NLCD_South <- crop(Missouri_NLCD,South_StudyArea) %>% mask(South_StudyArea) %>% 
  ratify() %>% 
  reclassify(reclass_matrixSouth)

writeRaster(NLCD_South,
            filename = paste0("1.DataManagement/CovRasters/base_layers/south_nlcd.tif"),
            overwrite = T)

DEM_South <- crop(Missouri_DEM,South_StudyArea) %>% mask(South_StudyArea)

writeRaster(DEM_South,
            filename = paste0("1.DataManagement/CovRasters/base_layers/south_dem.tif"),
            overwrite = T)

# Southeast
NLCD_Southeast <- crop(Missouri_NLCD,Southeast_StudyArea) %>% mask(Southeast_StudyArea) %>% 
  ratify() %>% 
  reclassify(reclass_matrixSouth)

writeRaster(NLCD_Southeast,
            filename = paste0("1.DataManagement/CovRasters/base_layers/southeast_nlcd.tif"),
            overwrite = T)

DEM_Southeast <- crop(Missouri_DEM,Southeast_StudyArea) %>% mask(Southeast_StudyArea)

writeRaster(DEM_Southeast,
            filename = paste0("1.DataManagement/CovRasters/base_layers/southeast_dem.tif"),
            overwrite = T)

###############################################################################
#   North                                                                   ####
#      Export Filepath                                                      ####

export_path <- "1.DataManagement/CovRasters_Landscape/north"

#      DEM-Based Covs                                                       ####
#        Data                                                               ####

DEM <- rast("1.DataManagement/CovRasters/base_layers/north_dem.tif")

#        Elevation                                                          ####

DEM <- rast("1.DataManagement/CovRasters/base_layers/north_dem.tif")

writeRaster(DEM,
            filename = paste0(export_path,"/north_dem.tif"),
            overwrite = T)

#        Slope                                                              ####

Slope <- DEM %>% terrain(v = "slope",
                         neighbors = 8,
                         unit = "degrees",
                         filename = paste0(export_path,"/north_slope.tif"),
                         overwrite = T)

#        TRI                                                                ####

TRI <- DEM %>% terrain(v = "TRI",
                         neighbors = 8,
                         filename = paste0(export_path,"/north_tri.tif"),
                         overwrite = T)

#      NLCD-Based Covariates                                                ####
#        Data                                                               ####

# Data Import 
NLCD <- rast("1.DataManagement/CovRasters/base_layers/north_nlcd.tif") %>% subst(from = NA, to = 0)

#        Patch Layer Creation                                                     ####

patch <- classify(NLCD,
                  rcl = cbind(3),
                  others = NA)

mapview::mapview(raster(patch))
