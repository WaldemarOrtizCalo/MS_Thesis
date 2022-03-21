#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-03-12 

# Purpose:This script is to extract covariates. 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(raster)
library(tidyverse)
library(adehabitatHR)
library(mapview)
library(FedData)
library(lubridate)
library(foreach)
library(stringr)
library(doParallel)
library(terra)
library(landscapemetrics)

#      Functions                                                            ####

unregister <- function() {

# This function is to unregister the parallel backend of the doParallel and
# foreach loop. Original source: https://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
  
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
  
}

#      Data                                                                 ####
#        [Deer Data]                                                        ####
df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

###############################################################################
#   Sampling Available Points - [Est time of Completion: 48 hrs]            ####
#      [Making and registering cluster]                                     ####
cl <- makeCluster(5)
registerDoParallel(cl)

#      [Sampler]                                                            ####

# List of ID
deer_id_list <- unique(df_deer$id)

# Sampling for loop
for (a in 1:length(deer_id_list)) {
  
  #      [Subsetting deer from original data]                                 #
  
  deer <- df_deer %>% filter(id == deer_id_list[a])
  
  #      [spdf object]                                                        #
  spdf_deer <- deer
  
  # Setting Coordinates 
  coordinates(spdf_deer) <- c("x","y")
  
  # Setting projection system
  proj4string(spdf_deer) <- CRS("+init=epsg:5070")
  
  #      [ltraj object]                                                       #
  
  # Creating ltraj object
  ltraj_deer <- as.ltraj(xy = coordinates(spdf_deer),
                         date = spdf_deer$t,
                         id= spdf_deer$id, 
                         typeII=TRUE)
  
  
  #      [Sampling and Data Export]                                           #
  
  # Calculating Buffer size for each location 
  buffer_calculations <- ltraj_deer[[1]] %>% 
    transform(dt = dt/3600) %>% 
    mutate(sampling_buffer = (dist/dt)+(2*sd((dist/dt),na.rm = T))) 
  
  buffer_calculations <-if_else(buffer_calculations$dist > buffer_calculations$sampling_buffer,
                                buffer_calculations$dist,
                                buffer_calculations$sampling_buffer)
  
  # Sampling loop
  used_available_data <- foreach(i = 2:nrow(deer),.errorhandling = 'remove',.combine = "rbind") %dopar% {
    
    #      Library                                                              
    library(tidyverse)
    library(adehabitatHR)
    library(foreach)
    library(sf)
    library(doParallel)
    
    # Creating sf object for point create buffer
    sf_object <- st_as_sf(spdf_deer[i-1,])
    
    #Creating buffer
    buffer <- st_buffer(sf_object,
                        buffer_calculations[i-1])
    
    # Sampling the Buffer 
    available_samples <- st_sample(buffer,5,multipoint,type = "random")
    
    #      [Joining Data for one Location]                                     
    
    # Prepping Used Data 
    used_loc <- deer[i,] %>% 
      as.data.frame() %>% 
      add_column(location_type = "used", .after = "id")
    
    # Preparing Available data 
    available_locs <- available_samples %>% as("Spatial") %>% 
      as.data.frame() %>% 
      rename(x = lon, y= lat) %>% 
      add_column(location_type = "available", .before = "x") %>% 
      add_column(id = used_loc$id, .before = "location_type") %>% 
      add_column(t = used_loc$t,
                 year = used_loc$year,
                 month = used_loc$month,
                 week = used_loc$week,
                 sex = used_loc$sex,
                 site = used_loc$site,
                 age = used_loc$age)
    
    # Joining  
    location_set <- rbind(used_loc,available_locs) %>% 
      add_column(observation_id = i-1, .before = "id")
  }
  
  write.csv(used_available_data,
            paste0("2.Chapter1/3.Output/CovariateExtraction/AvailabilityLocations/",deer_id_list[a],".csv"),
            row.names = F)
  
  print(a)
}

#      [Closing back-end cluster]                                           ####

unregister()

###############################################################################
#   Covariate Extraction                                                    ####
#      [Data for Covariate Extraction]                                      ####

#      [Used-Available Data from Sampling]                                  
used_available_list <- list.files("2.Chapter1/3.Output/CovariateExtraction/AvailabilityLocations",
                                  full.names = T) %>% read_csv()
#      [NLCD Data]                                                          
Missouri_NLCD <- raster("1.DataManagement/CleanData/NLCD_Missouri.tif") %>% ratify()

#      [DEM Data]                                                           
Missouri_topo <- stackOpen("1.DataManagement/CleanData/Topo_Missouri.stk")

#      [Road Data]                                                          
Missouri_roads <- st_read("1.DataManagement\\CleanData\\Missouri_Roads.shp")

#      [County Boundaries Data]                                             
Missouri_boundaries <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp")

#      [Covariate Extraction]                                               ####
#        [Extraction Settings]                                              ####

# List of Unique deer
id_list <- unique(used_available_list$id)

# Radius of sampling buffer around each point
buffer_radius <- 420

#        [NLCD] - in development                                            ####

# Registering Cluster
cl <- makeCluster(5)
registerDoParallel(cl)

# For Loop

# inspiration for NLCD extraction:
# https://mbjoseph.github.io/posts/2018-12-27-categorical-spatial-data-extraction-around-buffered-points-in-r/

for (i in 96:length(id_list)) {
  
  # Subsetting a Deer 
  deer <- used_available_list %>% filter(id == id_list[i])
  
  # Making the deer a SpatialPointsDataFrame
  spdf_deer <- deer
  
  # Setting Coordinates 
  coordinates(spdf_deer) <- c("x","y")
  
  # Setting projection system
  proj4string(spdf_deer) <- CRS("+init=epsg:5070")
  
  # Cropping Raster to the extent of the deer's movement track 
  cropped <- crop(Missouri_NLCD,spdf_deer)
  
  extracts <- terra::extract(cropped, spdf_deer, buffer = buffer_radius)
  
  landcover_proportions <- foreach(i = 1:length(extracts),.combine = bind_rows) %dopar% {
    library(tidyverse)
    counts_x <- table(extracts[[i]])
    proportions_x <- prop.table(counts_x) %>% as.data.frame() %>% pivot_wider(names_from = Var1, values_from = Freq,names_prefix = "proportion_")}
  
  spatialstructure_covs <- foreach(i = 1:nrow(spdf_deer),.combine = bind_rows) %dopar% {
    
    # Packages
    library(raster)
    library(landscapemetrics)
    library(terra)
    library(tidyverse)
    
    buf <- buffer(x = spdf_deer[i,],
                  width = buffer_radius)
    
    buffer_ras <- crop(cropped,buf) %>% mask(buf)
    
    # Landscape Shape Index
    cov_1 <- lsm_l_shape_mn(buffer_ras, directions = 8) %>% 
      dplyr::select(-c(layer,level,metric,id,class)) %>% rename(landscapeshapeindex = value)
    
    # Shannon'Diversity
    cov_2 <- lsm_l_shdi(buffer_ras)%>% 
      dplyr::select(-c(layer,level,metric,id,class)) %>% rename(shannondiversity = value)
    
    # Mean Shape Index
    cov_3 <- lsm_c_shape_mn(buffer_ras, directions = 8) %>% 
      pivot_wider(names_from = class, values_from = value,names_prefix = "meanshapeindex_") %>% 
      dplyr::select(-c(layer,level,metric,id))
    
    # Contagion
    cov_4 <- lsm_l_contag(buffer_ras)%>% 
      dplyr::select(-c(layer,level,metric,id,class)) %>% rename(contagion = value)
    
    # Patch Size
    cov_5 <- lsm_p_area(buffer_ras, directions = 8) %>% 
      pivot_wider(names_from = class, values_from = value,names_prefix = "patchsize_") %>% 
      dplyr::select(-c(layer,level,metric,id)) %>% 
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
    
    # Patch Density 
    cov_6 <- lsm_c_pd(buffer_ras, directions = 8) %>% 
      pivot_wider(names_from = class, values_from = value,names_prefix = "patchdensity_") %>% 
      dplyr::select(-c(layer,level,metric,id)) 
    
    
    covariates <- bind_cols(c(cov_1,cov_2,cov_3,cov_4,cov_5,cov_6))
  }
  
  bind_cols(deer,landcover_proportions,spatialstructure_covs) %>% 
    write_csv(paste0("2.Chapter1/3.Output/CovariateExtraction/Covariates/",id_list[i],".csv"))
  
  print(paste0(i,":",id_list[i]))
  
}

# Closing the Cluster

unregister()
###############################################################################