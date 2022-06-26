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
library(terra)

#      Functions                                                            ####
source("2.Chapter1/2.Functions/reclass_matrices.R")
source("2.Chapter1/2.Functions/proportion_raster_function.R")
unregister <- function() {
  
  # This function is to unregister the parallel backend of the doParallel and
  # foreach loop. Original source: https://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
  
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
  
}

#      Data [DO NOT RUN]                                                    ####
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
#        [Study Area Exports]                                               ####

# North
NLCD_North <- crop(Missouri_NLCD,North_StudyArea) %>% mask(North_StudyArea) %>% 
  ratify() %>% 
  reclassify(reclass_matrixNorth)

writeRaster(NLCD_North,
            filename = paste0("1.DataManagement/CovRasters/base_layers/north_nlcd.tif"),
            overwrite = T)

# South
NLCD_South <- crop(Missouri_NLCD,South_StudyArea) %>% mask(South_StudyArea) %>% 
  ratify() %>% 
  reclassify(reclass_matrixSouth)

writeRaster(NLCD_South,
            filename = paste0("1.DataManagement/CovRasters/base_layers/south_nlcd.tif"),
            overwrite = T)

# Southeast
NLCD_Southeast <- crop(Missouri_NLCD,Southeast_StudyArea) %>% mask(Southeast_StudyArea) %>% 
  ratify() %>% 
  reclassify(reclass_matrixSouth)

writeRaster(NLCD_Southeast,
            filename = paste0("1.DataManagement/CovRasters/base_layers/southeast_nlcd.tif"),
            overwrite = T)

###############################################################################
#   [Data - Tile Subsetting Study Areas]                                    ####
#      [NCLD Southeast]                                                     ####

NLCD_Southeast <- rast("1.DataManagement\\CovRasters\\base_layers\\southeast_nlcd.tif") 
#          [Data]                                                           ####

# Base Raster
ras <- NLCD_Southeast

# Buffer Radius
buffer_radius <- 600

# Focal Window
fw <- ceiling(focalWeight(ras, buffer_radius, type='circle'))

#          [Creating Raster Polygon and Cropping]                           ####

# Aggregates the Rasters
ras_aggregated <- terra::aggregate(ras,200)

# Creates a Polygon of the raster grid 
ras_polygon <- as.polygons(ras_aggregated,dissolve=F,na.rm=F)

# Expanding Polygon extents 

base_extents <- foreach(i = 1:length(ras_polygon)) %do% {
  ext(ras_polygon[i]) %>% as.vector()
}

extended_extents <- foreach(i = 1:length(ras_polygon)) %do% {
  ext(ras_polygon[i]) %>% extend(c(nrow(fw)*40,ncol(fw)*40)) 
}

# Crops and Subsets the raster based on polygon
ras_tilelist <- lapply(seq_along(extended_extents), function(i) terra::crop(ras, extended_extents[[i]]))

#          [Exporting Subsetted Tiles]                                      ####

# Establishing Progress Bar

pb = txtProgressBar(min = 0, max = length(ras_tilelist), initial = 0,style = 3) 

for (i in 1:length(ras_tilelist)) {
  
  # Progress Bar Iterator
  setTxtProgressBar(pb,i)
  
  # File Exporter
  writeRaster(ras_tilelist[[i]],filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/southeast/southeast_raw_",
                                                  formatC(i,width = 3, format = "d", flag = "0"),".tif"), overwrite=T)
  
  # Progress bar closing
  close(pb)
}

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


#        [Mean Size and Patch Density]                                      ####

# Window
fw <- ceiling(focalWeight(NLCD_Southeast, buffer_radius, type='circle'))

# Registering Parallel Backend
cl <- makeCluster(5)
registerDoParallel(cl)

lc_clasess <-c(4,5,6,8,9)

foreach(i = 1:length(lc_clasess)) %dopar% {
  
  # Packages
  library(landscapemetrics)
  library(raster)
  
  base_rast <- NLCD_Southeast
  
  base_rast[base_rast!= lc_clasess[i]] = NA
  
  # Mean Area 
  ras <- window_lsm(
    base_rast,
    fw,
    what = "lsm_l_area_mn")
  
  ras <-  ras[[1]][[1]]*10000
  
  writeRaster(ras, 
              filename= file.path("1.DataManagement","CovRasters",paste0("Southeast_class",lc_clasess[i],"_MeanPatchArea.tif")),
              format="GTiff", overwrite=TRUE)
  
  # Density
  ras <- window_lsm(
    base_rast,
    fw,
    what = "lsm_l_np")
  
  print(Sys.time())
  
  ras <- ras[[1]][[1]]/1130973.36 
  
  writeRaster(ras, 
              filename= file.path("1.DataManagement","CovRasters",paste0("Southeast_class",lc_clasess[i],"_PatchDensity.tif")),
              format="GTiff", overwrite=TRUE)
}

###############################################################################
#   [Southeast]                                                             ####
#      [NLCD raster]                                                        ####
#        [Data]                                                             ####

NLCD_Southeast <- rast("1.DataManagement\\CovRasters\\base_layers\\southeast_nlcd.tif") 

#        [Proportion of Landcover]                                          ####

# Unique Landcover types
landcover_southeast_unique <- unique(raster(NLCD_Southeast))

# Buffer Radius
buffer_radius <- 600

for (i in 1:length(landcover_southeast_unique)) {
  print(paste0("Start of iteration ", i, " Time: ",Sys.time()))
  proportion_raster_function(raster = raster(NLCD_Southeast),
                             landcover_num = landcover_southeast_unique[i],
                             buffer_radius = buffer_radius,
                             export = T,
                             export.filepath = "1.DataManagement/CovRasters/cov_layers_final/southeast_")
  print(paste0("End of iteration ", i, " Time: ",Sys.time()))
}

#        [Landscape Metrics]                                                ####
#          [Importing Tiles]                                                ####

tiles <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/southeast", pattern = "raw",full.names = T) 

#          [Window Settings]                                                ####

buffer_radius <- 600

fw <- ceiling(focalWeight(rast(tiles[1]), buffer_radius, type='circle'))

#          [Node Setup and Settings]                                        ####

# Cluster Number
cl <- makeCluster(5)
registerDoParallel(cl)

# Exporting Packages
clusterEvalQ(cl,
             {
               library(raster)
               library(terra)
               library(landscapemetrics)
               library(tidyverse)
             })

# Exporting data to clusters
clusterExport(cl=cl, varlist=c("tiles","fw","base_extents"), envir=environment())

#            [Metric Calculation: LSI]                                      ####

# Start
print(paste0("Start: ",Sys.time()))

# Function 
foreach(i = 1:length(tiles), 
        .errorhandling="pass",
        .combine = "rbind") %dopar% {
          
          # Creating a Raster
          ras <- rast(tiles[[i]])
          
          # Metric Calculation
          cov <- window_lsm(landscape = ras,
                            window = fw,
                            what = "lsm_l_lsi",
                            neighbourhood = 8,
                            pad = T,
                            na.rm=TRUE)
          
          cov <- cov[[1]][[1]] %>% 
            rast() %>% 
            crop(ext(base_extents[[i]]))
          
          # Raster Export
          writeRaster(cov,
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/southeast/southeast_lsi_",
                                        formatC(i,width = 3, format = "d", flag = "0"),".tif"), overwrite=T)
          
          return(i)
        }

# End 
print(paste0("End: ",Sys.time()))

#            [Metric Calculation: Contag]                                   ####

# Start
print(paste0("Start: ",Sys.time()))

# Function 
foreach(i = 1:length(tiles), 
        .errorhandling="pass",
        .combine = "rbind") %dopar% {
          
          # Creating a Raster
          ras <- rast(tiles[[i]])
          
          # Metric Calculation
          cov <- window_lsm(landscape = ras,
                            window = fw,
                            what = "lsm_l_contag",
                            neighbourhood = 8,
                            pad = T,
                            na.rm=TRUE)
          
          cov <- cov[[1]][[1]] %>% 
            rast() %>% 
            crop(ext(base_extents[[i]]))
          
          # Raster Export
          writeRaster(cov,
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/southeast/southeast_contag_",
                                        formatC(i,width = 3, format = "d", flag = "0"),".tif"), overwrite=T)
          
          return(i)
        }

# End 
print(paste0("End: ",Sys.time()))

#            [Metric Calculation: SHDI]                                     ####

# Start
print(paste0("Start: ",Sys.time()))

# Function 
foreach(i = 1:length(tiles), 
        .errorhandling="pass",
        .combine = "rbind") %dopar% {
          
          # Creating a Raster
          ras <- rast(tiles[[i]])
          
          # Metric Calculation
          cov <- window_lsm(landscape = ras,
                            window = fw,
                            what = "lsm_l_shdi",
                            neighbourhood = 8,
                            pad = T,
                            na.rm=TRUE)
          
          cov <- cov[[1]][[1]] %>% 
            rast() %>% 
            crop(ext(base_extents[[i]]))
          
          # Raster Export
          writeRaster(cov,
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/southeast/southeast_shdi_",
                                        formatC(i,width = 3, format = "d", flag = "0"),".tif"), overwrite=T)
          
          return(i)
        }

# End 
print(paste0("End: ",Sys.time()))

#            [Metric Calculation: MeanShape]                                ####

# Start
print(paste0("Start: ",Sys.time()))

# Function 
foreach(i = 210:length(tiles), 
        .errorhandling="pass",
        .combine = "rbind") %dopar% {
          
          # Creating a Raster
          ras <- rast(tiles[[i]])
          
          # Metric Calculation
          cov <- window_lsm(landscape = ras,
                            window = fw,
                            what = "lsm_l_shape_mn",
                            neighbourhood = 8,
                            pad = T,
                            na.rm=TRUE)
          
          cov <- cov[[1]][[1]] %>% 
            rast() %>% 
            crop(ext(base_extents[[i]]))
          
          # Raster Export
          writeRaster(cov,
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/southeast/southeast_meanshape_",
                                        formatC(i,width = 3, format = "d", flag = "0"),".tif"), overwrite=T)
          
          return(i)
        }

# End 
print(paste0("End: ",Sys.time()))


###############################################################################
#   [Mosaic of Covariate Tiles and Export]                                  ####
#      [Southeast]                                                          ####
#        [LSI]                                                              ####

lsi_mos_tiles <- list()

for (i in 1:length(tile_val)) {
  print(i)
  lsi_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/southeast",pattern = "lsi",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

lsi_mos <- sprc(lsi_mos_tiles) %>% mosaic()

writeRaster(lsi_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/southeast/southeast_lsi_final.tif"))

#        [Contag]                                                           ####

contag_mos_tiles <- list()

for (i in 1:length(tile_val)) {
  print(i)
  contag_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/southeast",pattern = "contag",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

contag_mos <- sprc(contag_mos_tiles) %>% mosaic()

writeRaster(contag_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/southeast/southeast_contag_final.tif"))

#        [SHDI]                                                             ####

shdi_mos_tiles <- list()

for (i in 1:length(tile_val)) {
  print(i)
  shdi_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/southeast",pattern = "shdi",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

shdi_mos <- sprc(shdi_mos_tiles) %>% mosaic()

writeRaster(shdi_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/southeast/southeast_shdi_final.tif"))

#        [MeanShape]                                                        ####

meanshape_mos_tiles <- list()

for (i in 1:length(tile_val)) {
  print(i)
  meanshape_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/southeast",pattern = "meanshape",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

meanshape_mos <- sprc(meanshape_mos_tiles) %>% mosaic()

writeRaster(meanshape_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/southeast/southeast_meanshape_final.tif"))
###############################################################################