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
library(sf)

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


#      [Aggregated Home Ranges]                                             ####
#        [Creating Dataframe]                                               ####

# Creating SpatialPointsDataFrame
spdf <- data_aggregated[1:200000,]
coordinates(spdf) <- ~location.long + location.lat
proj4string(spdf) <- CRS("+init=epsg:5070")

#        [MCP]                                                              ####

# Creating MCP 
mcp <- mcp(spdf, percent=95, 
           unin = c("m"),
           unout = c("km2")) %>% 
  st_as_sf() %>% 
  st_write("1.DataManagement/HomeRangePolygons/southeast/AggregatedPolygons/mcp_southeast.shp",
           append=FALSE)

# Verifying Polygon
mapview(mcp)

#        [KDE]                                                              ####

# Creating KDE
kde <- kernelUD(spdf, h = "href")
kde_UD <- getverticeshr(kde, 95)%>% 
  st_as_sf() %>% 
  st_write("1.DataManagement/HomeRangePolygons/southeast/AggregatedPolygons/kde_southeast.shp",
           append=FALSE)

# Verifying Polygon
mapview(kde_UD)
#      [Individual Home Ranges]                                             ####

# Subsetting and Individual 
indiv <- telemetry_object[[1]]

# Creating CTMM fit objects for the smoothing
M.IID <- ctmm.fit(indiv) 
GUESS <- ctmm.guess(indiv,interactive=FALSE) 
M.OUF <- ctmm.fit(indiv,GUESS) 

# Home Range Polygons
KDE <- akde(indiv,M.IID) # KDE
AKDE <- akde(indiv,M.OUF) # AKDE



fitted.mods <- ctmm.select(indiv, CTMM=GUESS, verbose=TRUE) %>% 
  summary(fitted.mods) %>% 
  as.data.frame() %>% 
  add_column(model = row.names(.), .before = 1) %>% 
  add_column(ID = indiv@info[[1]], .before = 1)

rownames(fitted.mods)<- c()

###############################################################################