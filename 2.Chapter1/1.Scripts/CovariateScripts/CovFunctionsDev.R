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

###############################################################################
#   [Test]                                                                  ####
#      [Data]                                                               ####

set.seed(69420)

data <- sample(x = c(NA,1,2,3,4),size = 1000000,replace = T)

ras <- rast(matrix(data, ncol = 1000, nrow = 1000,byrow = T))
#      [Creating Raster Polygon and Cropping]                               ####

# Aggregates the Rasters
ras_aggregated <- terra::aggregate(ras,100)

# Creates a Polygon of the raster grid 
ras_polygon <- as.polygons(ras_aggregated,dissolve=F,na.rm=F)

# Crops and Subsets the raster based on polygon
ras_tilelist <- lapply(seq_along(ras_polygon), function(i) terra::crop(ras, ras_polygon[i]))

#      [Exporting Subsetted Tiles]                                          ####

# Establishing Progress Bar

pb = txtProgressBar(min = 0, max = length(ras_tilelist), initial = 0,style = 3) 

for (i in 1:length(ras_tilelist)) {
  
  # Progress Bar Iterator
  setTxtProgressBar(pb,i)
  
  # File Exporter
  writeRaster(ras_tilelist[[i]],filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/Test/test_raw_",
                                                  formatC(i,width = 3, format = "d", flag = "0"),".tif"), overwrite=T)
  
  # Progress bar closing
  close(pb)
}


#      [Metric Calculation]                                                 ####
#        [Importing Tiles]                                                  ####

tiles <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/Test", pattern = "raw",full.names = T) 

#        [Metric Calculation]                                               ####

#           [Window settings]                                               ####

fw <- ceiling(focalWeight(ras, 10, type='circle'))

#           [Node Setup and Settings]                                       ####

cl <- makeCluster(4)
registerDoParallel(cl)

clusterEvalQ(cl,
             {
               library(raster)
               library(terra)
               library(landscapemetrics)
             })

clusterExport(cl=cl, varlist=c("tiles","fw"), envir=environment())

#           [Metric Calculation and export]                                 ####

# Landscape Shape Index
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
          
          # Raster Export
          writeRaster(cov[[1]][[1]],
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/Test/test_lsi_",
                                        formatC(i,width = 3, format = "d", flag = "0"),".tif"), overwrite=T)
          
          return(i)
        }
 
#      [Mosaic]                                                             ####
#        [LSI]                                                              ####


test_lsi_files <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/Test",pattern = "lsi",full.names = T) %>% 
  lapply(rast) %>% sprc() %>% mosaic()

plot(test_lsi_files)



###############################################################################
