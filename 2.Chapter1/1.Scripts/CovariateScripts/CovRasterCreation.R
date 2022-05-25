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
library(foreach)

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
#        [Deer Data]                                                        ####

deer_all <- lapply(list.files(path = "1.DataManagement/CleanData/Chapter1_UsedAvailableLocations",
                              full.names = T), read_csv)

deer_all  <- do.call(rbind,deer_all)


#        [Raster Data]                                                      ####

#      [NLCD Data]                                                          
Missouri_NLCD <- raster("1.DataManagement/CleanData/NLCD_Missouri.tif") %>% ratify()

#      [DEM Data]                                                           
# Missouri_topo <- stackOpen("1.DataManagement/CleanData/Topo_Missouri.stk")

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
#   [Extraction Settings]                                                   ####

# Buffer
buffer_radius <- 600
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
                             buffer_radius = buffer_radius,
                             export = T,
                             export.filepath = "1.DataManagement/CovRasters/North_")
  print(paste0("End of iteration ", i, " Time: ",Sys.time()))
}

#        [Landscape Metrics]                                                ####

# Covariate Metrics
covariate_metrics <- c("lsm_l_lsi","lsm_l_contag","lsm_l_shdi","lsm_l_shape_mn")
covariate_metrics_names <- c("lsi","contag","shdi","meanshapeindex")

# Window
fw <- ceiling(focalWeight(NLCD_North, buffer_radius, type='circle'))

# Registering Parallel Backend
cl <- makeCluster(4)
registerDoParallel(cl)

# Raster Creation 
print(Sys.time())

foreach(i = 1:length(covariate_metrics)) %dopar% {
  
  # Packages
  library(landscapemetrics)
  library(raster)
  
  # Function 
  window_lsm(
    NLCD_North,
    fw,
    what = covariate_metrics[i])
  
  writeRaster(t[[1]][[1]], 
              filename= file.path("1.DataManagement","CovRasters",paste0("North_",covariate_metrics_names[i],".tif")),
              format="GTiff", overwrite=TRUE)
}

print(Sys.time())

# Unregister parallel backend
unregister()

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
                             buffer_radius = buffer_radius,
                             export = T,
                             export.filepath = "1.DataManagement/CovRasters/South_")
  print(paste0("End of iteration ", i, " Time: ",Sys.time()))
}
#        [Landscape Metrics]                                                ####

# Covariate Metrics
covariate_metrics <- c("lsm_l_lsi","lsm_l_contag","lsm_l_shdi","lsm_l_shape_mn")
covariate_metrics_names <- c("lsi","contag","shdi","meanshapeindex")

# Window
fw <- ceiling(focalWeight(NLCD_South, buffer_radius, type='circle'))

# Registering Parallel Backend
cl <- makeCluster(4)
registerDoParallel(cl)

# Raster Creation 
print(Sys.time())

foreach(i = 1:length(covariate_metrics)) %dopar% {
  
  # Packages
  library(landscapemetrics)
  library(raster)
  
  # Function 
  window_lsm(
    NLCD_South,
    fw,
    what = covariate_metrics[i])
  
  writeRaster(t[[1]][[1]], 
              filename= file.path("1.DataManagement","CovRasters",paste0("South_",covariate_metrics_names[i],".tif")),
              format="GTiff", overwrite=TRUE)
}

print(Sys.time())

# Unregister parallel backend
unregister()


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
                             buffer_radius = buffer_radius,
                             export = T,
                             export.filepath = "1.DataManagement/CovRasters/Southeast_")
  print(paste0("End of iteration ", i, " Time: ",Sys.time()))
}
#        [Landscape Metrics]                                                ####

# Covariate Metrics
covariate_metrics <- c("lsm_l_lsi","lsm_l_contag","lsm_l_shdi","lsm_l_shape_mn")
covariate_metrics_names <- c("lsi","contag","shdi","meanshapeindex")

# Window
fw <- ceiling(focalWeight(NLCD_Southeast, buffer_radius, type='circle'))

# Registering Parallel Backend
cl <- makeCluster(4)
registerDoParallel(cl)

# Raster Creation 
print(Sys.time())

foreach(i = 1:length(covariate_metrics)) %dopar% {
  
  # Packages
  library(landscapemetrics)
  library(raster)
  
  # Function 
  ras <- window_lsm(
    NLCD_Southeast,
    fw,
    what = covariate_metrics[i])
  
  writeRaster(ras[[1]][[1]], 
              filename= file.path("1.DataManagement","CovRasters",paste0("Southeast_",covariate_metrics_names[i],".tif")),
              format="GTiff", overwrite=TRUE)
}

print(Sys.time())

# Unregister parallel backend
unregister()


###############################################################################
#   [Dev]                                                                 ####

r <- NLCD_Southeast
r <- r %>% ratify


r_crop <- crop(r, extent(469225, 476555, 1525365, 1559605))

mapview(r_crop)
fw <- ceiling(focalWeight(r_crop, buffer_radius, type='circle'))


#   [Covariate Raster Creation]                                                                 ####

#               [patch density]                                                     ####

mapview(r_crop)

r2 <- r_crop

r2[r2 != 4] = NA

mapview(r2)

fw <- ceiling(focalWeight(r2, 600, type='circle'))

print(Sys.time())

ras <- window_lsm(
  r2,
  fw,
  what = "lsm_l_pd")

print(Sys.time())

mapview(ras[[1]][[1]])+mapview(r_crop)+mapview(r2)

#               [mean of patch area]                                                     ####

mapview(r_crop)

r2 <- r_crop

r2[r2 != 4] = NA

mapview(r2)

fw <- ceiling(focalWeight(r2, 600, type='circle'))

print(Sys.time())

ras <- window_lsm(
  r2,
  fw,
  what = "lsm_l_area_mn")

print(Sys.time())

mapview(ras[[1]][[1]])+mapview(r_crop)+mapview(r2)