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
library(whitebox)
library(stringr)
library(classInt)
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

#      Data [DO NOT RUN - Only Run Once]                                    ####
#        [Deer Data]                                                        ####

deer_all <- lapply(list.files(path = "1.DataManagement/CleanData/Chapter1_UsedAvailableLocations",
                              full.names = T), read_csv)

deer_all  <- do.call(rbind,deer_all)


#        [Cov Data]                                                         ####

#      [NLCD Data]                                                          
Missouri_NLCD <- raster("1.DataManagement/CleanData/NLCD_Missouri.tif") %>% ratify()

#      [DEM Data]                                                           
Missouri_DEM <- raster("1.DataManagement\\CleanData\\DEM_Missouri.tif")

#      [Missouri Shapefiles]
Missouri_shp <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp") 

#      [Roads]                                                           
roads_missouri <- st_read("1.DataManagement\\CleanData\\Missouri_Roads.shp")

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

roads_north <- st_intersection(roads_missouri, North_StudyArea)

st_write(obj = roads_north,
         dsn = "1.DataManagement/CleanData/roads_north.shp")

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

roads_south <- st_intersection(roads_missouri, South_StudyArea)

st_write(obj = roads_south,
         dsn = "1.DataManagement/CleanData/roads_south.shp")

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

roads_southeast <- st_intersection(roads_missouri, Southeast_StudyArea)

st_write(obj = roads_southeast,
         dsn = "1.DataManagement/CleanData/roads_southeast.shp")

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
NLCD <- rast("1.DataManagement/CovRasters/base_layers/north_nlcd.tif") %>% subst(from = NA, to = 0) %>% 
  as.factor()

# List of Classes needed for export
north_classes <- c("water",
                   "developed",
                   "barren",
                   "forest",
                   "shrub",
                   "grassland",
                   "cropland",
                   "wetland")

#        Patch Layer Creation                                               ####

# Making sure that the classes are the ones needed. In other words, removing zeros
# from the data. 

raster_sub <- NLCD %>% classify(cbind(0,NA)) %>% as.factor()

# Make layers for each value
seg_layers <- segregate(raster_sub, keep=F, other=NA)

# Creating patch layers
patch_layers <- patches(seg_layers,
                        directions = 8,
                        allowGaps = F)

plot(patch_layers)

# Exporting Rasters

layernames <- paste0(export_path,"/north_patches_",north_classes, ".tif")

writeRaster(patch_layers, layernames, overwrite=TRUE)

#        Distance to Patch Layer                                            ####
#           File Management                                                 ####

# Export Directory
export_dir <- "1.DataManagement\\CovRasters_Landscape\\north"

# List of tif files
tif_list <- list.files("1.DataManagement\\CovRasters_Landscape\\north",
                       full.names = T,
                       pattern = "patches")

# List of covariate names
cov_name <- list.files("1.DataManagement\\CovRasters_Landscape\\north",
                       full.names = F,
                       pattern = "patches") %>% 
  str_remove("north_patches_") %>% 
  str_remove(".tif")

# Raster of Study area to use as mask 
studyshp <- rast("1.DataManagement/CovRasters/base_layers/north_nlcd.tif")

#           Protocol                                                        ####

for (i in 1:length(tif_list)) {
  
  print(paste("Layer",i,"of", length(tif_list)))
  
  #      Preparing Whitebox raster format and export                          ####
  
  patch_ras <- tif_list[i] %>% rast()
  
  patch_ras[is.na(patch_ras)] <- 0
  patch_ras[patch_ras != 0] <- 1
  
  writeRaster(patch_ras,
              paste0(export_dir,"\\north_wbpatch_",cov_name[i],".tif"),
              overwrite = T)
  
  #      Execution of Whitebox Distance protocol                              ####
  
  input <- paste0(export_dir,"\\north_wbpatch_",cov_name[i],".tif") 
  output <- paste0(export_dir,"\\north_patchdist_",cov_name[i],".tif")
  
  wbt_euclidean_distance(input = input,
                         output = output)
  
  #      Re-importing raster, clean, and export final file                    ####
  
  # Masking by study area raster
  dist_raster <- rast(output) %>% mask(studyshp)
  
  # Raster Export
  writeRaster(dist_raster,
              output,
              overwrite = T)
  
  # Removing whitebox formatted raster
  file.remove(paste0(export_dir,"\\north_wbpatch_",cov_name[i],".tif"))
  
}

# Free up Memory
gc()

#        Distance to Patch by Size                                          ####
#           Directory Information                                           ####

# Layer Directory
directory <- "1.DataManagement/CovRasters_Landscape/north"

#           Creating Size Raster                                            ####

rast_list <- list.files(directory,
                        pattern = "patches",
                        full.names = T)


for (i in 1:length(rast_list)) {
  
  raster_name <- list.files(directory,
                            pattern = "patches",
                            full.names = F) %>% 
    str_split("_") %>% 
    .[[i]] %>% #add iterator here
    .[3] %>% 
    str_remove(".tif")
  
  wbt_raster_area(input = rast_list[[i]],
                  output = paste0(directory,"/north_patcharea_",raster_name,".tif"),
                  units = "map units",
                  zero_back = T)
  
  print(i)
}

#           Creating Distance by Size                                       ####

# Raster List
rast_list <- list.files(directory,
                        pattern = "patcharea",
                        full.names = T) %>% 
  str_subset("small", negate = T) %>% 
  str_subset("medium", negate = T) %>% 
  str_subset("large", negate = T)

for (i in 1:length(rast_list)) {
  
  #   File Metadata                                                         ####
  
  # Extracting landcover name
  raster_name <- list.files(directory,
                            pattern = "patcharea",
                            full.names = F) %>% 
    str_subset("small", negate = T) %>% 
    str_subset("medium", negate = T) %>% 
    str_subset("large", negate = T) %>%  
    str_split("_") %>% 
    .[[i]] %>% #add iterator here
    .[3] %>% 
    str_remove(".tif")
  
  # Classes to Consider
  classes <- c("small","medium","large")
  
  #   Jenks                                                                ####
  
  # Patch ID values
  patchID <- list.files(directory,
                        pattern = "patches",
                        full.names = T) %>% 
    .[i] %>% 
    rast() %>% 
    values(dataframe = T,
           mat = F,
           na.rm = F)
  
  # Patch Area values
  patch_area <- rast_list %>% 
    .[i] %>%
    rast() %>% 
    values(dataframe = T,
           mat = F,
           na.rm = F)
  
  FisherInterval_data <- data.frame(patchID,patch_area) %>% 
    rename("patchID" = names(.)[1],
           "area" = names(.)[2]) %>% 
    filter(!is.na(area)) %>% 
    unique()
  
  # Calculating Fisher breaks
  intervals <- classIntervals(var = FisherInterval_data[,2],
                              n = length(classes),
                              style = "fisher",
                              samp_prop = .25)
  
  FisherBreaks <- matrix(data = c(intervals[[2]][1],intervals[[2]][2],
                                  intervals[[2]][2],intervals[[2]][3],
                                  intervals[[2]][3],intervals[[2]][4]),
                         nrow = 3,
                         ncol = 2,
                         byrow = T) %>% 
    data.frame() %>% 
    rename("start" = "X1","end" = "X2") %>% 
    mutate(class = classes,.before = "start")
  
  
  #   Distance to Patch by Area                                            ####
  
  # Creating Patch area raster
  raster_patcharea <- rast_list[i] %>% rast()
  
  for (j in 1:length(classes)) {
    
    # Making a copy of the raster
    ras <- raster_patcharea
    
    # Fisher Intervals and Class
    class <- FisherBreaks[j,1]
    lowint <- FisherBreaks[j,2]
    highint <- FisherBreaks[j,3]
    
    # Subsetting Rasters
    ras[is.na(ras)]<- 0
    ras[ras <= lowint]<- 0
    ras[ras > highint]<- 0
    
    # Exporting
    writeRaster(ras,
                filename = paste0(directory,"/north_patcharea_",raster_name,"_",class,".tif"),
                overwrite = T)
    
    # Creating Distance Raster
    wbt_euclidean_distance(input = paste0(directory,"/north_patcharea_",raster_name,"_",class,".tif"),
                           output = paste0(directory,"/north_patchdist_",raster_name,"_",class,".tif"))
  }
  
  # Iterator Update and Memory clearer
  print(paste0(i, " out of ",length(rast_list)," raster have been completed"))
  gc()
}

###############################################################################
#   South                                                                   ####
#      Export Filepath                                                      ####

export_path <- "1.DataManagement/CovRasters_Landscape/south"

#      DEM-Based Covs                                                       ####
#        Data                                                               ####

DEM <- rast("1.DataManagement/CovRasters/base_layers/south_dem.tif")

#        Elevation                                                          ####

DEM <- rast("1.DataManagement/CovRasters/base_layers/south_dem.tif")

writeRaster(DEM,
            filename = paste0(export_path,"/south_dem.tif"),
            overwrite = T)

#        Slope                                                              ####

Slope <- DEM %>% terrain(v = "slope",
                         neighbors = 8,
                         unit = "degrees",
                         filename = paste0(export_path,"/south_slope.tif"),
                         overwrite = T)

#        TRI                                                                ####

TRI <- DEM %>% terrain(v = "TRI",
                       neighbors = 8,
                       filename = paste0(export_path,"/south_tri.tif"),
                       overwrite = T)

#      NLCD-Based Covariates                                                ####
#        Data                                                               ####

# Data Import 
NLCD <- rast("1.DataManagement/CovRasters/base_layers/south_nlcd.tif") %>% subst(from = NA, to = 0) %>% 
  as.factor()

# List of Classes needed for export
south_classes <- c("water",
                   "developed",
                   "barren",
                   "deciduous_forest",
                   "evergreen_forest",
                   "mixed_forest",
                   "shrub",
                   "grassland",
                   "cropland",
                   "wetland")

#        Patch Layer Creation                                               ####

# Making sure that the classes are the ones needed. In other words, removing zeros
# from the data. 

raster_sub <- NLCD %>% classify(cbind(0,NA)) %>% as.factor()

# Make layers for each value
seg_layers <- segregate(raster_sub, keep=F, other=NA)

# Creating patch layers
patch_layers <- patches(seg_layers,
                        directions = 8,
                        allowGaps = F)

plot(patch_layers)

# Exporting Rasters

layernames <- paste0(export_path,"/south_patches_",south_classes, ".tif")

writeRaster(patch_layers, layernames, overwrite=TRUE)

#        Distance to Patch Layer                                            ####
#           File Management                                                 ####

# Export Directory
export_dir <- "1.DataManagement\\CovRasters_Landscape\\south"

# List of tif files
tif_list <- list.files("1.DataManagement\\CovRasters_Landscape\\south",
                       full.names = T,
                       pattern = "patches")

# List of covariate names
cov_name <- list.files("1.DataManagement\\CovRasters_Landscape\\south",
                       full.names = F,
                       pattern = "patches") %>% 
  str_remove("south_patches_") %>% 
  str_remove(".tif")

# Raster of Study area to use as mask 
studyshp <- rast("1.DataManagement/CovRasters/base_layers/south_nlcd.tif")

#           Protocol                                                        ####

for (i in 1:length(tif_list)) {
  
  print(paste("Layer",i,"of", length(tif_list)))
  
  #      Preparing Whitebox raster format and export                          ####
  
  patch_ras <- tif_list[i] %>% rast()
  
  patch_ras[is.na(patch_ras)] <- 0
  patch_ras[patch_ras != 0] <- 1
  
  writeRaster(patch_ras,
              paste0(export_dir,"\\south_wbpatch_",cov_name[i],".tif"),
              overwrite = T)
  
  #      Execution of Whitebox Distance protocol                              ####
  
  input <- paste0(export_dir,"\\south_wbpatch_",cov_name[i],".tif") 
  output <- paste0(export_dir,"\\south_patchdist_",cov_name[i],".tif")
  
  wbt_euclidean_distance(input = input,
                         output = output)
  
  #      Re-importing raster, clean, and export final file                    ####
  
  # Masking by study area raster
  dist_raster <- rast(output) %>% mask(studyshp)
  
  # Raster Export
  writeRaster(dist_raster,
              output,
              overwrite = T)
  
  # Removing whitebox formatted raster
  file.remove(paste0(export_dir,"\\south_wbpatch_",cov_name[i],".tif"))
  
}

# Free up Memory
gc()

#        Distance to Patch by Size                                          ####
#           Directory Information                                           ####

# Layer Directory
directory <- "1.DataManagement/CovRasters_Landscape/south"

#           Creating Size Raster                                            ####

rast_list <- list.files(directory,
                        pattern = "patches",
                        full.names = T)


for (i in 1:length(rast_list)) {
  
  raster_name <- list.files(directory,
                            pattern = "patches",
                            full.names = F) %>% 
    str_split("_") %>% 
    .[[i]] %>% #add iterator here
    .[3] %>% 
    str_remove(".tif")
  
  wbt_raster_area(input = rast_list[[i]],
                  output = paste0(directory,"/south_patcharea_",raster_name,".tif"),
                  units = "map units",
                  zero_back = T)
  
  print(i)
}

#           Creating Distance by Size                                       ####

# Raster List
rast_list <- list.files(directory,
                        pattern = "patcharea",
                        full.names = T) %>% 
  str_subset("small", negate = T) %>% 
  str_subset("medium", negate = T) %>% 
  str_subset("large", negate = T)

for (i in 1:length(rast_list)) {
  
  #   File Metadata                                                         ####
  
  # Extracting landcover name
  raster_name <- list.files(directory,
                            pattern = "patcharea",
                            full.names = F) %>% 
    str_subset("small", negate = T) %>% 
    str_subset("medium", negate = T) %>% 
    str_subset("large", negate = T) %>%  
    str_split("_") %>% 
    .[[i]] %>% #add iterator here
    .[3] %>% 
    str_remove(".tif")
  
  # Classes to Consider
  classes <- c("small","medium","large")
  
  #   Jenks                                                                ####
  
  # Patch ID values
  patchID <- list.files(directory,
                        pattern = "patches",
                        full.names = T) %>% 
    .[i] %>% 
    rast() %>% 
    values(dataframe = T,
           mat = F,
           na.rm = F)
  
  # Patch Area values
  patch_area <- rast_list %>% 
    .[i] %>%
    rast() %>% 
    values(dataframe = T,
           mat = F,
           na.rm = F)
  
  FisherInterval_data <- data.frame(patchID,patch_area) %>% 
    rename("patchID" = names(.)[1],
           "area" = names(.)[2]) %>% 
    filter(!is.na(area)) %>% 
    unique()
  
  # Calculating Fisher breaks
  intervals <- classIntervals(var = FisherInterval_data[,2],
                              n = length(classes),
                              style = "fisher",
                              samp_prop = .25)
  
  FisherBreaks <- matrix(data = c(intervals[[2]][1],intervals[[2]][2],
                                  intervals[[2]][2],intervals[[2]][3],
                                  intervals[[2]][3],intervals[[2]][4]),
                         nrow = 3,
                         ncol = 2,
                         byrow = T) %>% 
    data.frame() %>% 
    rename("start" = "X1","end" = "X2") %>% 
    mutate(class = classes,.before = "start")
  
  
  #   Distance to Patch by Area                                            ####
  
  # Creating Patch area raster
  raster_patcharea <- rast_list[i] %>% rast()
  
  for (j in 1:length(classes)) {
    
    # Making a copy of the raster
    ras <- raster_patcharea
    
    # Fisher Intervals and Class
    class <- FisherBreaks[j,1]
    lowint <- FisherBreaks[j,2]
    highint <- FisherBreaks[j,3]
    
    # Subsetting Rasters
    ras[is.na(ras)]<- 0
    ras[ras <= lowint]<- 0
    ras[ras > highint]<- 0
    
    # Exporting
    writeRaster(ras,
                filename = paste0(directory,"/south_patcharea_",raster_name,"_",class,".tif"),
                overwrite = T)
    
    # Creating Distance Raster
    wbt_euclidean_distance(input = paste0(directory,"/south_patcharea_",raster_name,"_",class,".tif"),
                           output = paste0(directory,"/south_patchdist_",raster_name,"_",class,".tif"))
  }
  
  # Iterator Update and Memory clearer
  print(paste0(i, " out of ",length(rast_list)," raster have been completed"))
  gc()
}

###############################################################################
#   Southeast                                                               ####
#      Export Filepath                                                      ####

export_path <- "1.DataManagement/CovRasters_Landscape/southeast"

#      DEM-Based Covs                                                       ####
#        Data                                                               ####

DEM <- rast("1.DataManagement/CovRasters/base_layers/southeast_dem.tif")

#        Elevation                                                          ####

DEM <- rast("1.DataManagement/CovRasters/base_layers/southeast_dem.tif")

writeRaster(DEM,
            filename = paste0(export_path,"/southeast_dem.tif"),
            overwrite = T)

#        Slope                                                              ####

Slope <- DEM %>% terrain(v = "slope",
                         neighbors = 8,
                         unit = "degrees",
                         filename = paste0(export_path,"/southeast_slope.tif"),
                         overwrite = T)

#        TRI                                                                ####

TRI <- DEM %>% terrain(v = "TRI",
                       neighbors = 8,
                       filename = paste0(export_path,"/southeast_tri.tif"),
                       overwrite = T)

#      NLCD-Based Covariates                                                ####
#        Data                                                               ####

# Data Import 
NLCD <- rast("1.DataManagement/CovRasters/base_layers/southeast_nlcd.tif") %>% subst(from = NA, to = 0) %>% 
  as.factor()

# List of Classes needed for export
southeast_classes <- c("water",
                   "developed",
                   "barren",
                   "deciduous_forest",
                   "evergreen_forest",
                   "mixed_forest",
                   "shrub",
                   "grassland",
                   "cropland",
                   "wetland")

#        Patch Layer Creation                                               ####

# Making sure that the classes are the ones needed. In other words, removing zeros
# from the data. 

raster_sub <- NLCD %>% classify(cbind(0,NA)) %>% as.factor()

# Make layers for each value
seg_layers <- segregate(raster_sub, keep=F, other=NA)

# Creating patch layers
patch_layers <- patches(seg_layers,
                        directions = 8,
                        allowGaps = F)

plot(patch_layers)

# Exporting Rasters

layernames <- paste0(export_path,"/southeast_patches_",southeast_classes, ".tif")

writeRaster(patch_layers, layernames, overwrite=TRUE)

#        Distance to Patch Layer                                            ####
#           File Management                                                 ####

# Export Directory
export_dir <- "1.DataManagement\\CovRasters_Landscape\\southeast"

# List of tif files
tif_list <- list.files("1.DataManagement\\CovRasters_Landscape\\southeast",
                       full.names = T,
                       pattern = "patches")

# List of covariate names
cov_name <- list.files("1.DataManagement\\CovRasters_Landscape\\southeast",
                       full.names = F,
                       pattern = "patches") %>% 
  str_remove("southeast_patches_") %>% 
  str_remove(".tif")

# Raster of Study area to use as mask 
studyshp <- rast("1.DataManagement/CovRasters/base_layers/southeast_nlcd.tif")

#           Protocol                                                        ####

for (i in 1:length(tif_list)) {
  
  print(paste("Layer",i,"of", length(tif_list)))
  
  #      Preparing Whitebox raster format and export                          ####
  
  patch_ras <- tif_list[i] %>% rast()
  
  patch_ras[is.na(patch_ras)] <- 0
  patch_ras[patch_ras != 0] <- 1
  
  writeRaster(patch_ras,
              paste0(export_dir,"\\southeast_wbpatch_",cov_name[i],".tif"),
              overwrite = T)
  
  #      Execution of Whitebox Distance protocol                              ####
  
  input <- paste0(export_dir,"\\southeast_wbpatch_",cov_name[i],".tif") 
  output <- paste0(export_dir,"\\southeast_patchdist_",cov_name[i],".tif")
  
  wbt_euclidean_distance(input = input,
                         output = output)
  
  #      Re-importing raster, clean, and export final file                    ####
  
  # Masking by study area raster
  dist_raster <- rast(output) %>% mask(studyshp)
  
  # Raster Export
  writeRaster(dist_raster,
              output,
              overwrite = T)
  
  # Removing whitebox formatted raster
  file.remove(paste0(export_dir,"\\southeast_wbpatch_",cov_name[i],".tif"))
  
}

# Free up Memory
gc()

#        Distance to Patch by Size                                          ####
#           Directory Information                                           ####

# Layer Directory
directory <- "1.DataManagement/CovRasters_Landscape/southeast"

#           Creating Size Raster                                            ####

rast_list <- list.files(directory,
                        pattern = "patches",
                        full.names = T)


for (i in 1:length(rast_list)) {
  
  raster_name <- list.files(directory,
                            pattern = "patches",
                            full.names = F) %>% 
    str_split("_") %>% 
    .[[i]] %>% #add iterator here
    .[3] %>% 
    str_remove(".tif")
  
  wbt_raster_area(input = rast_list[[i]],
                  output = paste0(directory,"/southeast_patcharea_",raster_name,".tif"),
                  units = "map units",
                  zero_back = T)
  
  print(i)
}

#           Creating Distance by Size                                       ####

# Raster List
rast_list <- list.files(directory,
                        pattern = "patcharea",
                        full.names = T) %>% 
  str_subset("small", negate = T) %>% 
  str_subset("medium", negate = T) %>% 
  str_subset("large", negate = T)

for (i in 1:length(rast_list)) {
  
  #   File Metadata                                                         ####
  
  # Extracting landcover name
  raster_name <- list.files(directory,
                            pattern = "patcharea",
                            full.names = F) %>% 
    str_subset("small", negate = T) %>% 
    str_subset("medium", negate = T) %>% 
    str_subset("large", negate = T) %>%  
    str_split("_") %>% 
    .[[i]] %>% #add iterator here
    .[3] %>% 
    str_remove(".tif")
  
  # Classes to Consider
  classes <- c("small","medium","large")
  
  #   Jenks                                                                ####
  
  # Patch ID values
  patchID <- list.files(directory,
                        pattern = "patches",
                        full.names = T) %>% 
    .[i] %>% 
    rast() %>% 
    values(dataframe = T,
           mat = F,
           na.rm = F)
  
  # Patch Area values
  patch_area <- rast_list %>% 
    .[i] %>%
    rast() %>% 
    values(dataframe = T,
           mat = F,
           na.rm = F)
  
  FisherInterval_data <- data.frame(patchID,patch_area) %>% 
    rename("patchID" = names(.)[1],
           "area" = names(.)[2]) %>% 
    filter(!is.na(area)) %>% 
    unique()
  
  # Calculating Fisher breaks
  intervals <- classIntervals(var = FisherInterval_data[,2],
                              n = length(classes),
                              style = "fisher",
                              samp_prop = .25)
  
  FisherBreaks <- matrix(data = c(intervals[[2]][1],intervals[[2]][2],
                                  intervals[[2]][2],intervals[[2]][3],
                                  intervals[[2]][3],intervals[[2]][4]),
                         nrow = 3,
                         ncol = 2,
                         byrow = T) %>% 
    data.frame() %>% 
    rename("start" = "X1","end" = "X2") %>% 
    mutate(class = classes,.before = "start")
  
  
  #   Distance to Patch by Area                                            ####
  
  # Creating Patch area raster
  raster_patcharea <- rast_list[i] %>% rast()
  
  for (j in 1:length(classes)) {
    
    # Making a copy of the raster
    ras <- raster_patcharea
    
    # Fisher Intervals and Class
    class <- FisherBreaks[j,1]
    lowint <- FisherBreaks[j,2]
    highint <- FisherBreaks[j,3]
    
    # Subsetting Rasters
    ras[is.na(ras)]<- 0
    ras[ras <= lowint]<- 0
    ras[ras > highint]<- 0
    
    # Exporting
    writeRaster(ras,
                filename = paste0(directory,"/southeast_patcharea_",raster_name,"_",class,".tif"),
                overwrite = T)
    
    # Creating Distance Raster
    wbt_euclidean_distance(input = paste0(directory,"/southeast_patcharea_",raster_name,"_",class,".tif"),
                           output = paste0(directory,"/southeast_patchdist_",raster_name,"_",class,".tif"))
  }
  
  # Iterator Update and Memory clearer
  print(paste0(i, " out of ",length(rast_list)," raster have been completed"))
  gc()
}

###############################################################################