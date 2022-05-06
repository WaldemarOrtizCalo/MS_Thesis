#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-05-05 

# Purpose: 

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
source("2.Chapter1/2.Functions/reclass_matrices.R")

#      Data                                                                 ####
#        [Deer Data]                                                        ####
df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

# Making age a factor
df_deer$age <- factor(df_deer$age,levels = c("F","Y","A"))
###############################################################################
#   Buffer Size for all individuals                                         ####

# Calculation
deer_buffers <- foreach(i = 1:length(deer_id_list), .combine = c) %do% {
#      [Subsetting deer from original data]                                 #

deer <- df_deer %>% filter(id == deer_id_list[i])

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

}

# Adding to the Dataframe
df_deer$buffer_radius <- deer_buffers

# Summarizing by sex 
df_deer %>% 
  group_by(sex) %>% 
  summarise(avg = mean(buffer_radius,na.rm = T))


# Summarizing by sex and age 
df_deer %>% 
  group_by(sex,age) %>% 
  summarise(avg = mean(buffer_radius,na.rm = T))



###############################################################################
