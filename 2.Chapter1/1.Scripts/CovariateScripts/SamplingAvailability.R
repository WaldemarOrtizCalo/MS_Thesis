#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-09-13 

# Purpose: This script is to sample availability within home range polygons and 
# exporting those points.

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
deer_df <- read_csv("1.DataManagement/RawData/csv_files/deer_all_revised.csv")

#        [North]                                                            ####

# Locations
north_locs <- deer_df %>% filter(site == "North")

# Polygon import
north_hr_polygons <- list.files("1.DataManagement/HomeRangePolygons/north/AggregatedPolygons",
           pattern = ".shp",
           full.names = T) %>% 
  lapply(st_read) %>% 
  do.call(bind_rows, .) %>% 
  mutate(season = c("all","fall","spring","summer","winter","all","fall","spring","summer","winter"), .after = id) %>%
  mutate(hr_type= rep(c("kde","mcp"),each = 5),.after = season)

north_hr_polygons$id <- list.files("1.DataManagement/HomeRangePolygons/north/AggregatedPolygons",
                       pattern = ".shp",
                       full.names = F) %>% gsub("\\..*","",.)

#        [South]                                                            ####


#        [Southeast]                                                        ####

###############################################################################
#   [Sampling Availability: North]                                          ####

season_class <- "winter"

# Dataframe for season
season_sf <- north_locs %>% 
  filter(season == season_class) %>% 
  st_as_sf(coords = c("location.long","location.lat"),
           crs = 5070)
  
# Extracting relevant polygons
mcp_hr_polygon <- north_hr_polygons %>% filter(hr_type == "mcp" & season == season_class)
kde_hr_polygon <- north_hr_polygons %>% filter(hr_type == "kde" & season == season_class)

# Determining used points
used <- st_intersection(season_sf,mcp_hr_polygon)

# Sampling available at a 1:5 ratio
available <- st_sample(x = mcp_hr_polygon,
                      size = (nrow(used)*5))

available <- st_sample(x = mcp_hr_polygon,
                       size = 200) %>% as.data.frame()

###############################################################################
#   [Sampling Availability: South]                                          ####


###############################################################################
#   [Sampling Availability: Southeast]                                      ####


###############################################################################



