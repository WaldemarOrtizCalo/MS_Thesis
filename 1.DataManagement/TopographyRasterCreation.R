#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-02-25 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(raster)
library(spatialEco)
library(tidyverse)

#      Functions                                                            ####

#      Data                                                                 ####

DEM_Missouri <- raster("1.DataManagement\\CleanData\\DEM_Missouri.tif")

###############################################################################
#   [DEM-derived Topography Layers]                                         ####

#      [Slope]                                                              ####

DEM_Slope <- terrain(DEM_Missouri,
                     opt = "slope",
                     unit = "degrees",
                     neighbors = 8,
                     filename = "1.DataManagement/CleanData/DEM_Slope.tif",
                     overwrite = T)

#      [Aspect]                                                             ####

DEM_Aspect <- terrain(DEM_Missouri,
                     opt = "aspect",
                     unit = "degrees",
                     neighbors = 8,
                     filename = "1.DataManagement/CleanData/DEM_Aspect.tif",
                     overwrite = T)

#      [TRI]                                                                ####

DEM_TRI <- terrain(DEM_Missouri,
                   opt = "TRI",
                   filename = "1.DataManagement/CleanData/DEM_TRI.tif",
                   overwrite = T)

###############################################################################
#   [Stack and Export]                                                      ####

# Creating Stack 

TopoStack_Missouri <- stack(c(DEM_Missouri,DEM_Slope,
                              DEM_Aspect,DEM_TRI)) %>% 
  stackSave(filename = "1.DataManagement/CleanData/Topo_Missouri.stk")

# Exporting 

Stack_Check <- stackOpen("1.DataManagement/CleanData/Topo_Missouri.stk")

plot(Stack_Check)

###############################################################################
