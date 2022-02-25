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
NLCD_Missouri<- crop(NLCD_raw,extent(shp_Missouri))

# Visually inspecting
plot(NLCD_Missouri)

#      [Reclassifying]                                                      ####

# Loading Legend
legend <- pal_nlcd() 

# Raster as Df
ras_df <- as.data.frame(NLCD_Missouri)

reclassify(NLCD_Missouri,legend[,c(2,3)])

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

###############################################################################
#   [Export: DEM - Missouri]                                                ####

writeRaster(DEM_Missouri_Crop,"1.DataManagement/CleanData/DEM_Missouri.tif")

###############################################################################
#   [Export: NLCD - Missouri]                                               ####
#      [Saving and Exporting]                                               ####

# Saving Cropped File

writeRaster(NLCD_Missouri,"1.DataManagement/CleanData/NLCD_Missouri.tif")

# Checking if it works

MTif <- raster("1.DataManagement/CleanData/NLCD_Missouri.tif")

plot(MTif)

###############################################################################
#   [Export: Missouri - Shapefile]                                          ####
#      [Exporting]                                                          ####
st_write(shp_Missouri,
         dsn = "1.DataManagement/CleanData/shp_Missouri.shp",
         driver = "ESRI Shapefile")

#      [Checking]                                                           ####
MShp <- st_read("1.DataManagement/CleanData/shp_Missouri.shp")

###############################################################################