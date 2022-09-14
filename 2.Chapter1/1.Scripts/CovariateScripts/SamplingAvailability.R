#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-09-13 

# Purpose: This script is to sample availability within home range polygons and 
# exporting those points. For now, this sampling only pertains to the 4 seasons
# and divided by sex. 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(sf)
library(stringr)
library(mapview)

#      Functions                                                            ####

#      Data                                                                 ####

# All deer data
deer_df <- read_csv("1.DataManagement/RawData/csv_files/deer_all_revised.csv") %>% 
  mutate(choice = 1, .after = individual.local.identifier)

#        [North]                                                            ####

# Locations
north_locs <- deer_df %>% filter(site == "North")

# Polygon import
north_hr_polygons <-  filepath_list %>% 
  lapply(st_read) %>% 
  do.call(bind_rows, .) %>% 
  mutate(season = rep(c("fall","spring","summer","winter"),4), .after = id) %>% 
  mutate(sex = (str_extract(filepath_list,pattern = "_[:upper:]_") %>% str_extract("[:upper:]")), .after = season) %>% 
  mutate(hr_type = ifelse(id == "homerange", "kde", "mcp"))

#        [South]                                                            ####


#        [Southeast]                                                        ####

###############################################################################
#   [Sampling Availability: North]                                          ####

season_class <- "winter"
sex_class <- "M"

# Dataframe for season
season_sf <- north_locs %>% 
  filter(season == season_class & sex == sex_class) %>% 
  st_as_sf(coords = c("location.long","location.lat"),
           crs = 5070)
  
# Extracting relevant polygons
mcp_hr_polygon <- north_hr_polygons %>% filter(hr_type == "mcp" & season == season_class & sex == sex_class)
kde_hr_polygon <- north_hr_polygons %>% filter(hr_type == "kde" & season == season_class & sex == sex_class)

# Determining used points
used <- st_intersection(season_sf,mcp_hr_polygon)

# Sampling available at a 1:5 ratio
available <- st_sample(x = mcp_hr_polygon,
                      size = (nrow(used)*5))

available <- mcp_hr_polygon %>% 
  st_sample(size = 200) %>% 
  st_as_sf() %>% 
  mutate(choice = 0,.before = 1) %>% 
  rename(geometry = x)

# Joining sf objects

final <- bind_rows(used, available)

st_write(final,
         dsn = paste0("1.DataManagement\\used_available\\north\\","north_",sex_class,"_",season_class,".shp"),
         layer = paste0("north_",sex_class,"_",season_class),
         driver = "ESRI Shapefile")


st_write(final, "crime.csv", layer_options = "GEOMETRY=AS_XY")

st_write(final, dsn = "nc1.shp", layer = "nc.shp", driver = "ESRI Shapefile")

test <- st_read(paste0("1.DataManagement\\used_available\\north\\","north_",sex_class,"_",season_class,".shp"))

mapview(final[1:100,])




###############################################################################
#   [Sampling Availability: South]                                          ####


###############################################################################
#   [Sampling Availability: Southeast]                                      ####


###############################################################################



