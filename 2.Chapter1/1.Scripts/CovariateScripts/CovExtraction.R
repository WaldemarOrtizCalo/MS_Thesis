#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-05-16 

# Purpose: Script for Covariate Extraction

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(terra)
library(raster)
library(tidyverse)
library(data.table)
library(mapview)

#      Functions                                                            ####
unregister <- function() {
  
  # This function is to unregister the parallel backend of the doParallel and
  # foreach loop. Original source: https://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
  
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
  
}
source("2.Chapter1/2.Functions/reclass_matrices.R")
#      Data                                                                 ####

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
            paste0("1.DataManagement/CleanData/Chapter1_UsedAvailableLocations/",deer_id_list[a],".csv"),
            row.names = F)
  
  print(a)
}

#      [Closing back-end cluster]                                           ####

unregister()

###############################################################################
#   Covariate Extraction                                                    ####
#      [Data for Covariate Extraction]                                      ####
#        [Used-Available Data from Sampling]                                ####
deer_choicesets <- list.files("1.DataManagement/CleanData/Chapter1_UsedAvailableLocations",
                                  full.names = T) %>% read_csv()

# By region
North_choicesets <- deer_choicesets %>% filter(site == "North")
South_choicesets <- deer_choicesets %>% filter(site == "South")
Southeast_choicesets <- deer_choicesets %>% filter(site == "CroplandStudy") %>% 
  vect(geom = c("x","y"), crs = "+init=EPSG:5070")

#        [Covariate Rasters by Region]                                      ####

# North 
North_covrasters <-list.files("1.DataManagement/CovRasters",
                                  pattern = "North",full.names = T) %>% 
  str_subset(pattern = "aux",negate = T) %>%
  lapply(raster) %>% 
  stack()

# South
South_covrasters <-list.files("1.DataManagement/CovRasters",
                                  pattern = "South",full.names = T) %>% 
  str_subset(pattern = "aux",negate = T) %>%
  lapply(raster) %>% 
  stack()

# Southeast
Southeast_covrasters <-list.files("1.DataManagement/CovRasters",
           pattern = "Southeast",full.names = T) %>% 
  str_subset(pattern = "aux",negate = T) %>%
  rast()

#      [Covariate Extraction]                                               ####
#        [North]                                                            ####
# Extract
North_final <- terra::extract(North_covrasters,North_choicesets) %>% 
  cbind(North_choicesets,.) %>% 
  as.data.frame()

# Export

#        [South]                                                            ####

# Extract
South_final <- terra::extract(South_covrasters,South_choicesets) %>% 
  cbind(South_choicesets,.) %>% 
  as.data.frame()

# Export
#        [Southeast]                                                        ####

# Extract
Southeast_final <- terra::extract(Southeast_covrasters,Southeast_choicesets) %>% 
  cbind(Southeast_choicesets,.) %>% 
  as.data.frame() %>% add_column(dc_ids = 1:nrow(.),.before = "observation_id")

# Export

write_csv(Southeast_final,
          file = "1.DataManagement/CleanData/Chapter1_FinalData/Southeast_final.csv",
          append = F)

###############################################################################

