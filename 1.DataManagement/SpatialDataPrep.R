#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-02-18 

# Purpose: To clean and organize Rasters for the project

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####

library(raster)
library(tidyverse)
library(sf)
library(mapview)
library(FedData)
library(readr)
library(stringr)
library(adehabitatHR)
#      Functions                                                            ####

#      Data                                                                 ####

# NLCD Raster
NLCD_raw <- raster("1.DataManagement\\RawData\\RasterData\\nlcd_2016_land_cover_l48_20210604.img")

# Deer Data 

df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

# Missouri Shapefiles

Missouri_shp <- st_read("1.DataManagement\\RawData\\Shapefiles\\Missouri_Counties.shp")

# DEM List
DEM_txt_names <- read.table("1.DataManagement\\RawData\\TextFiles\\DEM_List.txt",
                       header = F)

###############################################################################
#   [Standardizing Projections]                                             ####
#      [NLCD]                                                               ####

# Current CRS 
crs(NLCD_raw)

#      [Missouri Shp]                                                       ####

# Current CRS 
crs(Missouri_shp)

# Transforming Projections
shp_Missouri <- Missouri_shp %>% st_transform(5070)

# Checking new CRS 
crs(shp_Missouri)


###############################################################################
#   [NLCD Raster - Cropping and Reclassifying]                              ####
#      [Cropping]                                                           ####

# Cropping National NLCD to Missouri Extent
NLCD_Missouri<- terra::crop(x = NLCD_raw,
                            y = shp_Missouri,
                            snap = "near",
                            mask = T)

#      [Reclassifying]                                                      ####

# Loading NLCD Legend
legend <- pal_nlcd() 

# Making Raster Categorical (https://rspatial.org/raster/rs/5-supclassification.html#reference-data)
NLCD_Missouri <- NLCD_Missouri %>% ratify()

###############################################################################
#   [NLCD - Availability per area ]                                         ####
#      [North]                                                              ####

# Subsetting only North locations
df_deer_north <- df_deer %>% 
  filter(site == "North")

# Making a SpatialPoints Object for MCP
spatialpoints_deer_north <-SpatialPoints(coords = cbind(df_deer_north$x,df_deer_north$y),
                                         proj4string = crs(NLCD_Missouri))

# MCP 
mcp_deer_north <- mcp(xy = spatialpoints_deer_north,
                      percent = 95,
                      unin = "m",
                      unout = "km2")

# Masking and Cropping

available_north_NLCD <- terra::crop(x = NLCD_Missouri,
                                    y = mcp_deer_north,
                                    snap = "near",
                                    mask = F) %>% mask(mcp_deer_north) %>%
  writeRaster("1.DataManagement\\CleanData\\NLCD_available_north.tif",overwrite=T)


# Visual Inspection 

mapview(available_north_NLCD)+ mapview(mcp_deer_north)

#      [South]                                                              ####

# Subsetting only South locations
df_deer_south <- df_deer %>% 
  filter(site == "South")

# Making a SpatialPoints Object for MCP
spatialpoints_deer_south <-SpatialPoints(coords = cbind(df_deer_south$x,df_deer_south$y),
                                         proj4string = crs(NLCD_Missouri))

# MCP 
mcp_deer_south <- mcp(xy = spatialpoints_deer_south,
                      percent = 95,
                      unin = "m",
                      unout = "km2")

# Masking and Cropping

available_south_NLCD <- terra::crop(x = NLCD_Missouri,
                                    y = mcp_deer_south,
                                    snap = "near",
                                    mask = F) %>% mask(mcp_deer_south) %>% 
  writeRaster("1.DataManagement\\CleanData\\NLCD_available_south.tif",overwrite=T)

# Visual Inspection 

#mapview(available_south_NLCD)

#      [Southeast]                                                          ####

# Subsetting only Southeast locations
df_deer_southeast <- df_deer %>% 
  filter(site == "Southeast")

# Making a SpatialPoints Object for MCP
spatialpoints_deer_southeast <-SpatialPoints(coords = cbind(df_deer_southeast$x,df_deer_southeast$y),
                                             proj4string = crs(NLCD_Missouri))

# MCP 
mcp_deer_southeast <- mcp(xy = spatialpoints_deer_southeast,
                          percent = 95,
                          unin = "m",
                          unout = "km2")

# Masking and Cropping

available_southeast_NLCD <- terra::crop(x = NLCD_Missouri,
                                        y = mcp_deer_southeast,
                                        snap = "near",
                                        mask = F) %>% mask(mcp_deer_southeast) %>% 
  writeRaster("1.DataManagement\\CleanData\\NLCD_available_southeast.tif",overwrite=T)

# Visual Inspection 

#mapview(available_southeast_NLCD)



###############################################################################
#   [DEM Rasters]                                                           ####

#      [Listing DEMS and Copying to Project Folder]                         ####

# Making List of DEMS
DEM_Missouri_NameList <- lapply(DEM_txt_names, str_extract,"ASTGTMV003_N\\d\\dW\\d\\d\\d_dem.tif") %>% 
  unlist() %>% 
  na.omit()

# Extracting Files from DEM storage 

DEM_database_list <- paste0("E:\\RAW_DEM_Tiles/",DEM_Missouri_NameList)

# Copying Files to Project Directory

file.copy(from = DEM_database_list,
          to = "1.DataManagement/RawData/RasterData",
          recursive = FALSE,
          overwrite = T,
          copy.mode = T)

#      [Joining DEMs into Missouri DEM and homogenizing to other data]      ####

# Making List of DEMs

DEM_MissouriList <- lapply(list.files(path = "1.DataManagement/RawData/RasterData",
                                      pattern = ".tif",full.names = T)
                           ,raster)

# Making Large DEM for Missouri, reprojecting, masking, and cropping

DEM_Missouri <- do.call(merge,DEM_MissouriList) %>% 
  projectRaster(NLCD_Missouri)

DEM_Missouri_Mask <- DEM_Missouri %>% mask(shp_Missouri) 

DEM_Missouri_Crop <- DEM_Missouri_Mask %>% crop(shp_Missouri)

DEM_Missouri <- DEM_Missouri_Crop

###############################################################################
#   [DEM - Derived Topography Layers]                                       ####

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

#      [Stacking and Export]                                                ####

# Creating Stack 

TopoStack_Missouri <- stack(c(DEM_Missouri,DEM_Slope,
                              DEM_Aspect,DEM_TRI)) %>% 
  stackSave(filename = "1.DataManagement/CleanData/Topo_Missouri.stk")

# Checking 

Topo_Stack <- stackOpen("1.DataManagement/CleanData/Topo_Missouri.stk")

###############################################################################
#   [DEM - Derived Topography Layers for availability (95% MCP)]            ####
#      [North]                                                              ####

Topo_available_north <- crop(Topo_Stack,NLCD_available_north) %>% 
  mask(NLCD_available_north)

writeRaster(x = Topo_available_north,
            filename = "1.DataManagement/CleanData/Topo_available_north.tif",
            format = "GTiff",
            overwrite = T,
            bylayer = T,
            suffix = c("DEM_Elevation",
                       "DEM_Slope",
                       "DEM_Aspect",
                       "DEM_TRI"))

Topo_available_north_stack<- stack(lapply(c("1.DataManagement/CleanData/Topo_available_north_DEM_Elevation.tif",
                                            "1.DataManagement/CleanData/Topo_available_north_DEM_Slope.tif",
                                            "1.DataManagement/CleanData/Topo_available_north_DEM_Aspect.tif",
                                            "1.DataManagement/CleanData/Topo_available_north_DEM_TRI.tif"), raster)) %>% 
  stackSave("1.DataManagement/CleanData/Topo_available_north.stk")


#      [South]                                                              ####

Topo_available_south <- crop(Topo_Stack,NLCD_available_south) %>% 
  mask(NLCD_available_south)

writeRaster(x = Topo_available_south,
            filename = "1.DataManagement/CleanData/Topo_available_south.tif",
            format = "GTiff",
            overwrite = T,
            bylayer = T,
            suffix = c("DEM_Elevation",
                       "DEM_Slope",
                       "DEM_Aspect",
                       "DEM_TRI"))

Topo_available_south_stack<- stack(lapply(c("1.DataManagement/CleanData/Topo_available_south_DEM_Elevation.tif",
                                            "1.DataManagement/CleanData/Topo_available_south_DEM_Slope.tif",
                                            "1.DataManagement/CleanData/Topo_available_south_DEM_Aspect.tif",
                                            "1.DataManagement/CleanData/Topo_available_south_DEM_TRI.tif"), raster)) %>% 
  stackSave("1.DataManagement/CleanData/Topo_available_south.stk")

#      [Southeast]                                                          ####

Topo_available_southeast <- crop(Topo_Stack,NLCD_available_southeast) %>% 
  mask(NLCD_available_southeast)

writeRaster(x = Topo_available_southeast,
            filename = "1.DataManagement/CleanData/Topo_available_southeast.tif",
            format = "GTiff",
            overwrite = T,
            bylayer = T,
            suffix = c("DEM_Elevation",
                       "DEM_Slope",
                       "DEM_Aspect",
                       "DEM_TRI"))

Topo_available_southeast_stack<- stack(lapply(c("1.DataManagement/CleanData/Topo_available_southeast_DEM_Elevation.tif",
                                            "1.DataManagement/CleanData/Topo_available_southeast_DEM_Slope.tif",
                                            "1.DataManagement/CleanData/Topo_available_southeast_DEM_Aspect.tif",
                                            "1.DataManagement/CleanData/Topo_available_southeast_DEM_TRI.tif"), raster)) %>% 
  stackSave("1.DataManagement/CleanData/Topo_available_southeast.stk")

###############################################################################
#   [Export: DEM - Missouri]                                                ####

writeRaster(DEM_Missouri_Crop,"1.DataManagement/CleanData/DEM_Missouri.tif")

###############################################################################
#   [Export: NLCD - Missouri]                                               ####

writeRaster(NLCD_Missouri,"1.DataManagement/CleanData/NLCD_Missouri.tif",
            overwrite = T)

###############################################################################
#   [Export: Missouri - Shapefile]                                          ####

st_write(shp_Missouri,
         dsn = "1.DataManagement/CleanData/shp_Missouri.shp",
         driver = "ESRI Shapefile")

###############################################################################