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

#      Data [DO NOT RUN - Only Run Once]                                    ####
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
#      [NCLD North]                                                         ####
#          [Data]                                                           ####

# Import
NLCD_North <- rast("1.DataManagement\\CovRasters\\base_layers\\north_nlcd.tif") 

# Base Raster
ras <- NLCD_North

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
  writeRaster(ras_tilelist[[i]],filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/north/north_raw_",
                                                  formatC(i,width = 4, format = "d", flag = "0"),".tif"), overwrite=T)
  
  # Progress bar closing
  close(pb)
}

#      [NCLD South]                                                         ####
#          [Data]                                                           ####

# Base Raster
ras <- NLCD_South

# Buffer Radius
buffer_radius <- 600

# Focal Window
fw <- ceiling(focalWeight(ras, buffer_radius, type='circle'))


NLCD_South <- rast("1.DataManagement\\CovRasters\\base_layers\\south_nlcd.tif") 

# Base Raster
ras <- NLCD_South

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
  writeRaster(ras_tilelist[[i]],filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/south/south_raw_",
                                                  formatC(i,width = 4, format = "d", flag = "0"),".tif"), overwrite=T)
  
  # Progress bar closing
  close(pb)
}

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
#        [Data]                                                             ####

NLCD_North <- rast("1.DataManagement\\CovRasters\\base_layers\\north_nlcd.tif") 

#        [Proportion of Landcover]                                          ####

# Unique Landcover types
landcover_north_unique <- unique(raster(NLCD_North))

# Buffer Radius
buffer_radius <- 600

for (i in 1:length(landcover_north_unique)) {
  print(paste0("Start of iteration ", i, " Time: ",Sys.time()))
  proportion_raster_function(raster = raster(NLCD_North),
                             landcover_num = landcover_north_unique[i],
                             buffer_radius = buffer_radius,
                             export = T,
                             export.filepath = "1.DataManagement/CovRasters/cov_layers_final/north/north_")
  print(paste0("End of iteration ", i, " Time: ",Sys.time()))
}

#        [Landscape Metrics]                                                ####
#          [Importing Tiles]                                                ####

tiles <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/north", pattern = "raw",full.names = T) 

#          [Window Settings]                                                ####

buffer_radius <- 600

fw <- ceiling(focalWeight(rast(tiles[1]), buffer_radius, type='circle'))

#          [Node Setup and Settings]                                        ####

# Cluster Number
cl <- makeCluster(4)
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
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/north/north_lsi_",
                                        formatC(i,width = 4, format = "d", flag = "0"),".tif"), overwrite=T)
          
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
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/north/north_contag_",
                                        formatC(i,width = 4, format = "d", flag = "0"),".tif"), overwrite=T)
          
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
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/north/north_shdi_",
                                        formatC(i,width = 4, format = "d", flag = "0"),".tif"), overwrite=T)
          
          return(i)
        }

# End 
print(paste0("End: ",Sys.time()))

#            [Metric Calculation: MeanShape]                                ####

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
                            what = "lsm_l_shape_mn",
                            neighbourhood = 8,
                            pad = T,
                            na.rm=TRUE)
          
          cov <- cov[[1]][[1]] %>% 
            rast() %>% 
            crop(ext(base_extents[[i]]))
          
          # Raster Export
          writeRaster(cov,
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/north/north_meanshape_",
                                        formatC(i,width = 4, format = "d", flag = "0"),".tif"), overwrite=T)
          
          return(i)
        }

# End 
print(paste0("End: ",Sys.time()))


###############################################################################
#   [South]                                                                 ####
#      [NLCD raster]                                                        ####
#        [Data]                                                             ####

NLCD_South <- rast("1.DataManagement\\CovRasters\\base_layers\\south_nlcd.tif") 

#        [Proportion of Landcover]                                          ####

# Unique Landcover types
landcover_south_unique <- unique(raster(NLCD_South))

# Buffer Radius
buffer_radius <- 600

for (i in 1:length(landcover_south_unique)) {
  print(paste0("Start of iteration ", i, " Time: ",Sys.time()))
  proportion_raster_function(raster = raster(NLCD_South),
                             landcover_num = landcover_south_unique[i],
                             buffer_radius = buffer_radius,
                             export = T,
                             export.filepath = "1.DataManagement/CovRasters/cov_layers_final/south/south_")
  print(paste0("End of iteration ", i, " Time: ",Sys.time()))
}

#        [Landscape Metrics]                                                ####
#          [Importing Tiles]                                                ####

tiles <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/south", pattern = "raw",full.names = T) 

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
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/south/south_lsi_",
                                        formatC(i,width = 4, format = "d", flag = "0"),".tif"), overwrite=T)
          
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
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/south/south_contag_",
                                        formatC(i,width = 4, format = "d", flag = "0"),".tif"), overwrite=T)
          
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
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/south/south_shdi_",
                                        formatC(i,width = 4, format = "d", flag = "0"),".tif"), overwrite=T)
          
          return(i)
        }

# End 
print(paste0("End: ",Sys.time()))

#            [Metric Calculation: MeanShape]                                ####

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
                            what = "lsm_l_shape_mn",
                            neighbourhood = 8,
                            pad = T,
                            na.rm=TRUE)
          
          cov <- cov[[1]][[1]] %>% 
            rast() %>% 
            crop(ext(base_extents[[i]]))
          
          # Raster Export
          writeRaster(cov,
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/south/south_meanshape_",
                                        formatC(i,width = 4, format = "d", flag = "0"),".tif"), overwrite=T)
          
          return(i)
        }

# End 
print(paste0("End: ",Sys.time()))


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
foreach(i = 1:length(tiles), 
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
#      [North]                                                              ####

# Checking How many tiles there are for the study area 
list.files("1.DataManagement/CovRasters/cov_metric_tiles/north",pattern = "contag",full.names = T) %>% 
  length()

# Indexing List

tile_val <- list(1:100,
                 101:200,
                 201:300,
                 301:400,
                 401:500,
                 501:600,
                 601:700,
                 701:800,
                 801:900,
                 901:1000,
                 1001:1100,
                 1101:1200,
                 1201:1300,
                 1301:1400,
                 1401:1430)

#        [LSI]                                                              ####

# Making an empty list for raster tiles
lsi_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  lsi_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/north",pattern = "lsi",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
lsi_mos <- sprc(lsi_mos_tiles) %>% mosaic()

# Naming Layer 
names(lsi_mos)<- "north_lsi"

# Exporting Mosaic
writeRaster(lsi_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/north/north_lsi_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(lsi_mos_tiles)
rm(lsi_mos)
gc()

#        [Contag]                                                           ####
# Making an empty list for raster tiles
contag_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  contag_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/north",pattern = "contag",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
contag_mos <- sprc(contag_mos_tiles) %>% mosaic()

# Naming Layer 
names(contag_mos)<- "north_contag"

# Exporting Mosaic
writeRaster(contag_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/north/north_contag_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(contag_mos_tiles)
rm(contag_mos)
gc()

#        [SHDI]                                                             ####

# Making an empty list for raster tiles
shdi_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  shdi_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/north",pattern = "shdi",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
shdi_mos <- sprc(shdi_mos_tiles) %>% mosaic()

# Naming Layer 
names(shdi_mos)<- "north_shdi"

# Exporting Mosaic
writeRaster(shdi_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/north/north_shdi_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(shdi_mos_tiles)
rm(shdi_mos)
gc()

#        [MeanShape]                                                        ####

# Making an empty list for raster tiles
meanshape_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  meanshape_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/north",pattern = "meanshape",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
meanshape_mos <- sprc(meanshape_mos_tiles) %>% mosaic()

# Naming Layer 
names(meanshape_mos)<- "north_meanshape"

# Exporting Mosaic
writeRaster(meanshape_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/north/north_meanshape_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(meanshape_mos_tiles)
rm(meanshape_mos)
gc()

#      [South]                                                              ####

# Checking How many tiles there are for the study area 
list.files("1.DataManagement/CovRasters/cov_metric_tiles/south",pattern = "raw",full.names = T) %>% 
  length()

# Indexing List

tile_val <- list(1:100,
                 101:200,
                 201:300,
                 301:400,
                 401:500,
                 501:600,
                 601:700,
                 701:800,
                 801:900,
                 901:1000,
                 1001:1100,
                 1101:1200,
                 1201:1300,
                 1301:1400,
                 1401:1500,
                 1501:1600,
                 1601:1665)

#        [LSI]                                                              ####

# Making an empty list for raster tiles
lsi_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  lsi_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/south",pattern = "lsi",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
lsi_mos <- sprc(lsi_mos_tiles) %>% mosaic()

# Naming Layer 
names(lsi_mos)<- "south_lsi"

# Exporting Mosaic
writeRaster(lsi_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/south/south_lsi_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(lsi_mos_tiles)
rm(lsi_mos)
gc()

#        [Contag]                                                           ####
# Making an empty list for raster tiles
contag_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  contag_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/south",pattern = "contag",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
contag_mos <- sprc(contag_mos_tiles) %>% mosaic()

# Naming Layer 
names(contag_mos)<- "south_contag"

# Exporting Mosaic
writeRaster(contag_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/south/south_contag_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(contag_mos_tiles)
rm(contag_mos)
gc()

#        [SHDI]                                                             ####

# Making an empty list for raster tiles
shdi_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  shdi_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/south",pattern = "shdi",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
shdi_mos <- sprc(shdi_mos_tiles) %>% mosaic()

# Naming Layer 
names(shdi_mos)<- "south_shdi"

# Exporting Mosaic
writeRaster(shdi_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/south/south_shdi_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(shdi_mos_tiles)
rm(shdi_mos)
gc()

#        [MeanShape]                                                        ####

# Making an empty list for raster tiles
meanshape_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  meanshape_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/south",pattern = "meanshape",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
meanshape_mos <- sprc(meanshape_mos_tiles) %>% mosaic()

# Naming Layer 
names(meanshape_mos)<- "south_meanshape"

# Exporting Mosaic
writeRaster(meanshape_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/south/south_meanshape_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(meanshape_mos_tiles)
rm(meanshape_mos)
gc()

#      [Southeast]                                                          ####

# Checking How many tiles there are for the study area 
list.files("1.DataManagement/CovRasters/cov_metric_tiles/southeast",pattern = "raw",full.names = T) %>% 
  length()

# Indexing List

tile_val <- list(1:50,
                 51:100,
                 101:200,
                 201:250,
                 251:300,
                 301:350,
                 351:400,
                 401:450,
                 451:500)

#        [LSI]                                                              ####

# Making an empty list for raster tiles
lsi_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  lsi_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/southeast",pattern = "lsi",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
lsi_mos <- sprc(lsi_mos_tiles) %>% mosaic()

# Naming Layer 
names(lsi_mos)<- "southeast_lsi"

# Exporting Mosaic
writeRaster(lsi_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/southeast/southeast_lsi_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(lsi_mos_tiles)
rm(lsi_mos)
gc()

#        [Contag]                                                           ####
# Making an empty list for raster tiles
contag_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  contag_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/southeast",pattern = "contag",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
contag_mos <- sprc(contag_mos_tiles) %>% mosaic()

# Naming Layer 
names(contag_mos)<- "southeast_contag"

# Exporting Mosaic
writeRaster(contag_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/southeast/southeast_contag_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(contag_mos_tiles)
rm(contag_mos)
gc()

#        [SHDI]                                                             ####

# Making an empty list for raster tiles
shdi_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  shdi_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/southeast",pattern = "shdi",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
shdi_mos <- sprc(shdi_mos_tiles) %>% mosaic()

# Naming Layer 
names(shdi_mos)<- "southeast_shdi"

# Exporting Mosaic
writeRaster(shdi_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/southeast/southeast_shdi_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(shdi_mos_tiles)
rm(shdi_mos)
gc()

#        [MeanShape]                                                        ####

# Making an empty list for raster tiles
meanshape_mos_tiles <- list()

# For loop to create tile subsets
for (i in 1:length(tile_val)) {
  print(i)
  meanshape_mos_tiles[i] <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/southeast",pattern = "meanshape",full.names = T) %>% 
    .[tile_val[[i]]] %>% 
    lapply(rast) %>% 
    sprc() %>% 
    mosaic()
}

# Creating a Mosaic of the tiles
meanshape_mos <- sprc(meanshape_mos_tiles) %>% mosaic()

# Naming Layer 
names(meanshape_mos)<- "southeast_meanshape"

# Exporting Mosaic
writeRaster(meanshape_mos,
            filename = paste0("1.DataManagement/CovRasters/cov_layers_final/southeast/southeast_meanshape_final.tif"),
            overwrite = T)

# Removing big object post export and then cleaning memory 
rm(meanshape_mos_tiles)
rm(meanshape_mos)
gc()

###############################################################################