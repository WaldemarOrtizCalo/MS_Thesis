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


#      Functions                                                            ####

#      Data                                                                 ####
#        [Deer Data]                                                        ####
df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

# Setting Coordinates 
coordinates(df_deer) <- c("x","y")

# Setting projection system
proj4string(df_deer) <- CRS("+init=epsg:5070")

#        [NLCD Data]                                                        ####
Missouri_NLCD <- raster("1.DataManagement\\CleanData\\NLCD_Missouri.tif")

#        [DEM Data]                                                         ####
Missouri_topo <- stackOpen("1.DataManagement/CleanData/Topo_Missouri.stk")

#        [Road Data]                                                        ####
Missouri_roads <- st_read("1.DataManagement\\CleanData\\Missouri_Roads.shp")

#        [County Boundaries Data]                                           ####
Missouri_boundaries <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp")


###############################################################################
#   [Testing with one indiv]                                                ####
#      [Creating ltraj object]                                              ####

# Creating ltraj object
ltraj_deer <- as.ltraj(xy = coordinates(df_deer),
                       date = df_deer$t,
                       id= df_deer$id, 
                       typeII=TRUE)

#      [Buffer Calculations]                                                ####

#Subset an indiv
ltraj_deer_track <- ltraj_deer[id = "N15001"]

# Calculating Buffer size for each location 
ltraj_track <- ltraj_deer_track[[1]] %>% 
  transform(dt = dt/3600) %>% 
  mutate(sampling_buffer = (dist/dt)+(2*sd((dist/dt),na.rm = T))) 

for(i in 1:nrow(ltraj_track)){
  
  # Creating sf object for to create buffer
  sf_object <- st_as_sf(indiv[i,])
  
  #Creating buffer
  buffer <- st_buffer(sf_object,
                      ltraj_track[,"sampling_buffer"][i])
  
  # Sampling the Buffer 
  samples_available <- st_sample(buffer,5,multipoint,type = "random")
  
  #      [Joining Data for one Location]                                      ####
  
  # Prepping Used Data 
  used_loc <- indiv[i,] %>% 
    as.data.frame() %>% 
    add_column(location_type = "used", .after = "id")
  
  # Preparing Available data 
  available_locs <- samples_available %>% as("Spatial") %>% 
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
    add_column(observation_id = i, .before = "id")
  
  print(i)
}






