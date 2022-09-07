#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-09-06 

# Purpose: This code is designed to model animal home ranges from the 3 study 
# populations. 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(ctmm)
library(adehabitatHR)
library(tidyverse)
library(move)
library(mapview)

#      Functions                                                            ####

#      Data                                                                 ####

# Deer Data
deer_df <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv") %>% 
  rename(individual.local.identifier = id,
         timestamp = t,
         location.long = x,
         location.lat = y)

###############################################################################
#   [North]                                                                 ####
###############################################################################
#   [South]                                                                 ####
###############################################################################
#   [Southeast]                                                             ####
#      [Data Prep]                                                          ####

# Creating Telemetry Object for the Data 

data <- deer_df %>% 
  filter(site == "CroplandStudy")

data_aggregated <- data

telemetry_object <- move(x=data$location.long, 
                         y=data$location.lat, time=as.POSIXct(data$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), 
                         proj=CRS("+init=epsg:5070"),
                         data=data, 
                         animal=data$individual.local.identifier) %>% 
  as.telemetry()


#      [Aggregated HR]                                                      ####

# Creating SpatialPointsDataFrame

spdf <- data_aggregated[1:1000,]
coordinates(spdf) <- ~location.long + location.lat
proj4string(spdf) <- CRS("+init=epsg:5070")

# Creating MCP 
mcp <- mcp(spdf, percent=95, unin = c("m"),
                 unout = c("km2"))

# Creating KDE
udbis <- kernelUD(spdf, h = "LSCV")
ud <- kernelUD(spdf, h = "href", grid = 40, same4all = FALSE,
               hlim = c(0.1, 1.5), kern = c("bivnorm"))

ver <- getverticeshr(ud, 95)
plot(ver)





###############################################################################

