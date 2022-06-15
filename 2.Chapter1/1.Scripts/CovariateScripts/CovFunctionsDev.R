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

data <- sample(x = c(NA,1,2,3,4),size = 400,replace = T)

ras <- rast(matrix(data, ncol = 20, nrow = 20,byrow = T))
#      [Polygon Creation for subsetted]                                     ####

# Aggregates the Rasters
ras_aggregated <- terra::aggregate(ras,4)

# Creates a Polygon of the raster grid 
ras_polygon <- as.polygons(ras_aggregated,dissolve=F,na.rm=F)

# Crops and Subsets the raster based on polygon
ras_tilelist <- lapply(seq_along(ras_polygon), function(i) terra::crop(ras, ras_polygon[i]))

#      [Metric Calculation]                                                 ####

#        [Cov Metrics]                                                      ####
cov_metrics <-  c("lsm_l_lsi","lsm_l_contag","lsm_l_shdi","lsm_l_shape_mn")
cov_metrics_names <- c("lsi","contag","shdi","meanshapeindex")

#        [Parallel Settings]                                                ####

cl <- makeCluster(4)
registerDoParallel(cl)
clusterEvalQ(cl,
             {
               library(raster)
               library(terra)
               library(landscapemetrics)
             })

clusterExport(cl=cl, varlist=c("cov_metrics","cov_metrics_names"), envir=environment())

#        [Metric Calculation]                                               ####

length(ras_tilelist)
for (i in 1:3) {
  print(paste(Sys.time(),": Tile",i,"of",length(ras_tilelist),"initiated"))
  
  r_tile <- ras_tilelist[[i]]
  
  foreach(m = 1:length(cov_metrics)) %dopar%{
    
    cov <- window_lsm(landscape = r_tile,
                      window = matrix(data = 1,nrow = 3, ncol = 3),
                      what = cov_metrics[m])
    
    writeRaster(cov[[1]][[1]],
                filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/Test/test_",
                                  cov_metrics_names[m],
                                  "_tile",formatC(i,width = 3, format = "d", flag = "0"),
                                  ".tif"), overwrite=T)
  }
  
  print(paste(Sys.time(),": Tile",i,"completed"))
}




foreach(m = 1:length(cov_metrics)) %dopar%{
  
  cov <- window_lsm(landscape = r_tile,
             window = matrix(data = 1,nrow = 3, ncol = 3),
             what = cov_metrics[m])
  
  writeRaster(cov[[1]][[1]],
              filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/Test/test_",
                                cov_metrics_names[m],
                                "_tile",formatC(i,width = 3, format = "d", flag = "0"),
                                ".tif"),
              overwrite=T)
}

#      [Mosaic of Tiles]                                                    ####




###############################################################################

#   [Test]                                                                 ####
#      [Data]                                                              ####

set.seed(69420)

data <- sample(x = c(NA,1,2,3,4),size = 10000,replace = T)

ras <- rast(matrix(data, ncol = 100, nrow = 100,byrow = T))
#      [Creating Raster Polygon and Cropping]                              ####

# Aggregates the Rasters
ras_aggregated <- terra::aggregate(ras,6)

# Creates a Polygon of the raster grid 
ras_polygon <- as.polygons(ras_aggregated,dissolve=F,na.rm=F)

# Crops and Subsets the raster based on polygon
ras_tilelist <- lapply(seq_along(ras_polygon), function(i) terra::crop(ras, ras_polygon[i]))

#      [Exporting Subsetted Tiles]                                         ####

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


#      [Importing Tiles]                                                              ####

r_tile <- list.files("1.DataManagement/CovRasters/cov_metric_tiles/Test", pattern = "raw",full.names = T) 

#        [Metric Calculation]                                                            ####

#           [Node Setup and Settings]                                                  ####

cl <- makeCluster(4)
registerDoParallel(cl)

clusterEvalQ(cl,
             {
               library(raster)
               library(terra)
               library(landscapemetrics)
             })

clusterExport(cl=cl, varlist=c("r_tile"), envir=environment())

#           [Metric Calculation]                                                         ####


foreach(i = 1:10, 
        .errorhandling="pass",
        .combine = "rbind") %dopar% {
          
          # Creating a Raster
          ras <- rast(r_tile[[i]])
          
          # Metric Calculation
          cov <- window_lsm(landscape = ras,
                            window = matrix(data = 1,nrow = 3, ncol = 3),
                            what = "lsm_l_lsi")
          
          # Raster Export
          writeRaster(cov[[1]][[1]],
                      filename = paste0("1.DataManagement/CovRasters/cov_metric_tiles/Test/test_lsi_",
                                        formatC(i,width = 3, format = "d", flag = "0"),".tif"), overwrite=T)
          
          return(i)
        }





