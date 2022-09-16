#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-09-15 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(sf)
library(terra)

#      Functions                                                            ####

#      Data                                                                 ####
#        [Location Data]                                                    ####

# North 
locs_north <- list.files("1.DataManagement/used_available/north",
           full.names = T,
           pattern = ".shp") %>% 
  lapply(st_read)

names_north_dfs <- list.files("1.DataManagement/used_available/north",
                              full.names = F,
                              pattern = ".shp") %>% 
  str_remove(".shp")

# South 
locs_south <- list.files("1.DataManagement/used_available/south",
                    full.names = T,
                    pattern = ".shp") %>% 
  lapply(st_read)

names_south_dfs <- list.files("1.DataManagement/used_available/south",
                              full.names = F,
                              pattern = ".shp") %>% 
  str_remove(".shp")


# Southeast 
locs_southeast <- list.files("1.DataManagement/used_available/southeast",
                    full.names = T,
                    pattern = ".shp") %>% 
  lapply(st_read)

names_southeast_dfs <- list.files("1.DataManagement/used_available/southeast",
                              full.names = F,
                              pattern = ".shp") %>% 
  str_remove(".shp")

#        [Covariate Layers]                                                 ####

# North
covs_north <- list.files(path = "1.DataManagement/CovRasters/cov_layers_final/north",
                         full.names = T,
                         pattern = ".tif") %>% 
  str_subset(pattern = "aux",negate = T) %>%
  rast()

# South
covs_south <- list.files(path = "1.DataManagement/CovRasters/cov_layers_final/south",
                         full.names = T,
                         pattern = ".tif") %>% 
  str_subset(pattern = "aux",negate = T) %>%
  rast()

# Southeast
covs_southeast <- list.files(path = "1.DataManagement/CovRasters/cov_layers_final/southeast",
                             full.names = T,
                             pattern = ".tif") %>% 
  str_subset(pattern = "aux",negate = T) %>%
  rast()

# Fixed Layer Names
names(covs_southeast[[1]])<- "southeast_contag"
names(covs_southeast[[13]])<- "southeast_shdi"

###############################################################################
#   [North]                                                                 ####

for (i in 1:length(locs_north)) {
  
  # Choosing layer
  loc_data <- locs_north[[i]]
  
  # Converting data to dataframe
  choice_values <- loc_data %>% as.data.frame() %>% 
    select(-geometry) %>% 
    mutate(ObservationID = 1:nrow(.),.before = 1)
  
  # Extracting the data
  cov_values <- extract(covs_north, loc_data, ID = T) %>% 
    rename(ObservationID = ID)
  
  # Joining datasets
  final <- inner_join(choice_values, cov_values, by = "ObservationID")   
  
  # Exporting Data
  write_csv(final,
            file = paste0("1.DataManagement/used_available/north/final_data/",names_north_dfs[i],"_final.csv"),
            append = F)
}

###############################################################################
#   [South]                                                                 ####

for (i in 1:length(locs_south)) {
  
  # Choosing layer
  loc_data <- locs_south[[i]]
  
  # Converting data to dataframe
  choice_values <- loc_data %>% as.data.frame() %>% 
    select(-geometry) %>% 
    mutate(ObservationID = 1:nrow(.),.before = 1)
  
  # Extracting the data
  cov_values <- extract(covs_south, loc_data, ID = T) %>% 
    rename(ObservationID = ID)
  
  # Joining datasets
  final <- inner_join(choice_values, cov_values, by = "ObservationID")   
  
  # Exporting Data
  write_csv(final,
            file = paste0("1.DataManagement/used_available/south/final_data/",names_south_dfs[i],"_final.csv"),
            append = F)
}

###############################################################################
#   [Southeast]                                                             ####

for (i in 1:length(locs_southeast)) {
  
  # Choosing layer
  loc_data <- locs_southeast[[i]]
  
  # Converting data to dataframe
  choice_values <- loc_data %>% as.data.frame() %>% 
    select(-geometry) %>% 
    mutate(ObservationID = 1:nrow(.),.before = 1)
  
  # Extracting the data
  cov_values <- extract(covs_southeast, loc_data, ID = T) %>% 
    rename(ObservationID = ID)
  
  # Joining datasets
  final <- inner_join(choice_values, cov_values, by = "ObservationID")   
  
  # Exporting Data
  write_csv(final,
            file = paste0("1.DataManagement/used_available/southeast/final_data/",names_southeast_dfs[i],"_final.csv"),
            append = F)
}

###############################################################################