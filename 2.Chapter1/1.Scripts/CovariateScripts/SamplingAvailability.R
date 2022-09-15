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

###############################################################################
#   [Sampling Availability: North]                                          ####
#      [Data Import and Prep]                                               ####

# Locations
locs <- deer_df %>% filter(site == "North")

# Polygon import

filepath_list <- list.files("1.DataManagement/HomeRangePolygons/north/AggregatedPolygons",
                            full.names = T,
                            pattern = ".shp") %>% str_subset(pattern = "_[:upper:]_")

hr_polygons <-  filepath_list %>% 
  lapply(st_read) %>% 
  do.call(bind_rows, .) %>% 
  mutate(season = rep(c("fall","spring","summer","winter"),4), .after = id) %>% 
  mutate(sex = (str_extract(filepath_list,pattern = "_[:upper:]_") %>% str_extract("[:upper:]")), .after = season) %>% 
  mutate(hr_type = ifelse(id == "homerange", "kde", "mcp"))

#      [Sampling Protocol]                                                  ####

season_list <- unique(locs$season)
sex_list <- unique(locs$sex)

for (i in 1:length(season_list)) {
  
  # Update Line
  print(paste(i, "out of",length(season_list), "seasons"))
  
  for(j in 1:length(sex_list)){
    
    # Update Line
    print(paste(j, "out of",length(sex_list), "sexes"))
    
    # Dataframe for season
    season_sf <- locs %>% 
      filter(season == season_list[i] & sex == sex_list[j]) %>% 
      st_as_sf(coords = c("location.long","location.lat"),
               crs = 5070)
    
    # Extracting relevant polygons
    kde_hr_polygon <- hr_polygons %>% filter(hr_type == "kde" & season == season_list[i] & sex == sex_list[j])
    
    # Determining used points
    used <- st_intersection(season_sf,kde_hr_polygon) %>% 
      .[,c("choice","geometry")]
    
    # Sampling available at a 1:5 ratio
    available <- st_sample(x = kde_hr_polygon, size = (nrow(used)*5)) %>% 
      st_as_sf() %>% 
      mutate(choice = 0,.before = 1) %>% 
      rename(geometry = x) 
    
    # Joining sf objects
    
    final <- bind_rows(used, available)
    
    st_write(final,
             dsn = paste0("1.DataManagement/used_available/north/","north_",sex_list[j],"_",season_list[i],".shp"),
             layer = paste0("north_",sex_list[j],"_",season_list[i]),
             driver = "ESRI Shapefile",
             append = F)
  }
}

###############################################################################
#   [Sampling Availability: South]                                          ####
#      [Data Import and Prep]                                               ####

# Locations
locs <- deer_df %>% filter(site == "South")

# Polygon import

filepath_list <- list.files("1.DataManagement/HomeRangePolygons/south/AggregatedPolygons",
                            full.names = T,
                            pattern = ".shp") %>% str_subset(pattern = "_[:upper:]_")

hr_polygons <-  filepath_list %>% 
  lapply(st_read) %>% 
  do.call(bind_rows, .) %>% 
  mutate(season = rep(c("fall","spring","summer","winter"),4), .after = id) %>% 
  mutate(sex = (str_extract(filepath_list,pattern = "_[:upper:]_") %>% str_extract("[:upper:]")), .after = season) %>% 
  mutate(hr_type = ifelse(id == "homerange", "kde", "mcp"))

#      [Sampling Protocol]                                                  ####

season_list <- unique(locs$season)
sex_list <- unique(locs$sex)

for (i in 1:length(season_list)) {
  
  # Update Line
  print(paste(i, "out of",length(season_list), "seasons"))
  
  for(j in 1:length(sex_list)){
    
    # Update Line
    print(paste(j, "out of",length(sex_list), "sexes"))
    
    # Dataframe for season
    season_sf <- locs %>% 
      filter(season == season_list[i] & sex == sex_list[j]) %>% 
      st_as_sf(coords = c("location.long","location.lat"),
               crs = 5070)
    
    # Extracting relevant polygons
    kde_hr_polygon <- hr_polygons %>% filter(hr_type == "kde" & season == season_list[i] & sex == sex_list[j])
    
    # Determining used points
    used <- st_intersection(season_sf,kde_hr_polygon) %>% 
      .[,c("choice","geometry")]
    
    # Sampling available at a 1:5 ratio
    available <- st_sample(x = kde_hr_polygon, size = (nrow(used)*5)) %>% 
      st_as_sf() %>% 
      mutate(choice = 0,.before = 1) %>% 
      rename(geometry = x) 
    
    # Joining sf objects
    
    final <- bind_rows(used, available)
    
    st_write(final,
             dsn = paste0("1.DataManagement/used_available/south/","south_",sex_list[j],"_",season_list[i],".shp"),
             layer = paste0("south_",sex_list[j],"_",season_list[i]),
             driver = "ESRI Shapefile",
             append = F)
  }
}

###############################################################################
#   [Sampling Availability: Southeast]                                      ####
#      [Data Import and Prep]                                               ####

# Locations
locs <- deer_df %>% filter(site == "CroplandStudy")

# Polygon import

filepath_list <- list.files("1.DataManagement/HomeRangePolygons/southeast/AggregatedPolygons",
                            full.names = T,
                            pattern = ".shp") %>% str_subset(pattern = "_[:upper:]_")

hr_polygons <-  filepath_list %>% 
  lapply(st_read) %>% 
  do.call(bind_rows, .) %>% 
  mutate(season = rep(c("fall","spring","summer","winter"),2), .after = id) %>% 
  mutate(sex = (str_extract(filepath_list,pattern = "_[:upper:]_") %>% str_extract("[:upper:]")), .after = season) %>% 
  mutate(hr_type = ifelse(id == "homerange", "kde", "mcp"))

#      [Sampling Protocol]                                                  ####

season_list <- unique(locs$season)
sex_list <- unique(locs$sex)

for (i in 1:length(season_list)) {
  
  # Update Line
  print(paste(i, "out of",length(season_list), "seasons"))
  
  for(j in 1:length(sex_list)){
    
    # Update Line
    print(paste(j, "out of",length(sex_list), "sexes"))
    
    # Dataframe for season
    season_sf <- locs %>% 
      filter(season == season_list[i] & sex == sex_list[j]) %>% 
      st_as_sf(coords = c("location.long","location.lat"),
               crs = 5070)
    
    # Extracting relevant polygons
    kde_hr_polygon <- hr_polygons %>% filter(hr_type == "kde" & season == season_list[i] & sex == sex_list[j])
    
    # Determining used points
    used <- st_intersection(season_sf,kde_hr_polygon) %>% 
      .[,c("choice","geometry")]
    
    # Sampling available at a 1:5 ratio
    available <- st_sample(x = kde_hr_polygon, size = (nrow(used)*5)) %>% 
      st_as_sf() %>% 
      mutate(choice = 0,.before = 1) %>% 
      rename(geometry = x) 
    
    # Joining sf objects
    
    final <- bind_rows(used, available)
    
    st_write(final,
             dsn = paste0("1.DataManagement/used_available/southeast/","southeast_",sex_list[j],"_",season_list[i],".shp"),
             layer = paste0("southeast_",sex_list[j],"_",season_list[i]),
             driver = "ESRI Shapefile",
             append = F)
  }
}

###############################################################################