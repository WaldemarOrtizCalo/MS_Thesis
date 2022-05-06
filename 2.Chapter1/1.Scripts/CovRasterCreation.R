#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-05-05 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(raster)
library(tidyverse)
library(sf)
library(mapview)
library(pbapply)
library(doParallel)
library(stringr)
library(landscapemetrics)

#      Functions                                                            ####
source("2.Chapter1/2.Functions/reclass_matrices.R")
source("2.Chapter1/2.Functions/proportion_raster_function.R")

#      Data                                                                 ####
#        [Deer Data]                                                        ####

deer_all <- lapply(list.files(path = "2.Chapter1/3.Output/CovariateExtraction/AvailabilityLocations",
                              full.names = T), read_csv)

deer_all  <- do.call(rbind,deer_all)


#        [Raster Data]                                                      ####

#      [NLCD Data]                                                          
Missouri_NLCD <- raster("1.DataManagement/CleanData/NLCD_Missouri.tif") %>% ratify()

#      [DEM Data]                                                           
Missouri_topo <- stackOpen("1.DataManagement/CleanData/Topo_Missouri.stk")

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
###############################################################################
#   [North]                                                                 ####

#      [NLCD raster]                                                        ####

NLCD_North <- crop(Missouri_NLCD,North_StudyArea) %>% mask(North_StudyArea) %>% 
  ratify() %>% 
  reclassify(reclass_matrixNorth)

#        [Proportion of Landcover]                                          ####

# Unique Landcover types
landcover_north_unique <- unique(NLCD_North)

for (i in 1:length(landcover_north_unique)) {
  print(paste0("Start of iteration ", i, " Time: ",Sys.time()))
  proportion_raster_function(raster = NLCD_North,
                             landcover_num = landcover_north_unique[i],
                             buffer_radius = 540,
                             export = T,
                             export.filepath = "1.DataManagement/CovRasters/North_")
  print(paste0("End of iteration ", i, " Time: ",Sys.time()))
}

###############################################################################
#   [South]                                                                 ####
#      [NLCD raster]                                                        ####

NLCD_South <- crop(Missouri_NLCD,South_StudyArea) %>% mask(South_StudyArea) %>% 
  ratify() %>% 
  reclassify(reclass_matrixSouth)

#        [Proportion of Landcover]                                          ####

# Unique Landcover types
landcover_south_unique <- unique(NLCD_South)

for (i in 1:length(landcover_south_unique)) {
  print(paste0("Start of iteration ", i, " Time: ",Sys.time()))
  proportion_raster_function(raster = NLCD_South,
                             landcover_num = landcover_south_unique[i],
                             buffer_radius = 540,
                             export = T,
                             export.filepath = "1.DataManagement/CovRasters/South_")
  print(paste0("End of iteration ", i, " Time: ",Sys.time()))
}
###############################################################################
#   [Southeast]                                                             ####
#      [NLCD raster]                                                        ####

NLCD_Southeast <- crop(Missouri_NLCD,Southeast_StudyArea) %>% mask(Southeast_StudyArea) %>% 
  ratify() %>% 
  reclassify(reclass_matrixSouth)

#        [Proportion of Landcover]                                          ####

# Unique Landcover types
landcover_southeast_unique <- unique(NLCD_Southeast)

for (i in 1:length(landcover_southeast_unique)) {
  print(paste0("Start of iteration ", i, " Time: ",Sys.time()))
  proportion_raster_function(raster = NLCD_Southeast,
                             landcover_num = landcover_southeast_unique[i],
                             buffer_radius = 540,
                             export = T,
                             export.filepath = "1.DataManagement/CovRasters/Southeast_")
  print(paste0("End of iteration ", i, " Time: ",Sys.time()))
}
###############################################################################


#   [Dev]                                                                 ####

library(spatialEco)

buffer_radius <- 540

r <- NLCD_North
r <- r %>% ratify

fw <- ceiling(focalWeight(r, buffer_radius, type='circle'))

print(Sys.time())
lsm_focal <- window_lsm(
  r,
  fw,
  what = "lsm_l_lsi",
  progress = T
)
print(Sys.time())
