#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-02-02 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(ctmm)
library(tidyverse)
library(sf)
library(terra)
library(move)
library(landscapemetrics)
library(foreach)

#      Functions                                                            ####
reclass_matrixNorth<- matrix(data = c(1,"Water",
                                      2,"Developed",
                                      3,"Barren",
                                      4,"Forest",
                                      5,"Shrub",
                                      6,"Grassland",
                                      7,"Cropland",
                                      8,"Wetland"
),ncol = 2,byrow = TRUE) %>% 
  as.data.frame() %>% 
  rename("class" = V1,
         "class_name" = V2)


reclass_matrixSouth<- matrix(data = c(1, "Water",
                                      2, "Developed",
                                      3, "Barren",
                                      4, "Deciduous",
                                      5, "Evergreen",
                                      6, "Mixed",
                                      7, "Shrub",
                                      8, "Grassland",
                                      9, "Cropland",
                                      10, "Wetland"),ncol = 2,byrow = TRUE) %>% 
  as.data.frame() %>% 
  rename("class" = V1,
         "class_name" = V2)

#      Data                                                                 ####

# Deer Locations
locs_deer <- read_csv("1.DataManagement/ch2_data/clean/locs/deer_mortalitylocs.csv")

# Environmental Layers 
covlayers_north <- list.files("1.DataManagement/CovRasters/base_layers", 
                              pattern = "north",
                              full.names = T) %>% str_subset(".xml", negate = T)


covlayers_south <- list.files("1.DataManagement/CovRasters/base_layers", 
                              pattern = "south",
                              full.names = T) %>% str_subset(".xml", negate = T) %>% str_subset("southeast", negate = T)

###############################################################################
#   North                                                                   ####
#      Data Import                                                          ####

# Deer Locations
locs <- locs_deer %>% filter(site == "North")

# Covariate Layers/Stack 
cov_layers <- rast(covlayers_north)

#      Home Range Calculation                                               ####
#        Filtering ids and periods with greater than 20 locs                ####

valid_homeranges <- locs %>% 
  group_by(individual.local.identifier,ordered_int_id) %>% 
  summarize(n_locs = n()) %>% 
  filter(n_locs > 20)

#        Home Range Polygon calculation and export                          ####
#nrow(valid_homeranges)
hr_log <- foreach(i = 1:20,
        .combine = rbind) %do% {
          
          # HR metadata 
          indiv_id <- valid_homeranges[[i,1]]
          interval_id <- valid_homeranges[[i,2]]
          
          # Filtering locations
          locs_hr <- locs %>% 
            filter(individual.local.identifier == indiv_id & ordered_int_id == interval_id) %>% 
            mutate(HR_id = paste0(indiv_id,"_",interval_id),.after = 1)
          
          telemetry_object <- move(x=locs_hr$location.long, 
                                   y=locs_hr$location.lat, time=as.POSIXct(locs_hr$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), 
                                   proj=CRS("+init=epsg:5070"),
                                   data=locs_hr, 
                                   animal=locs_hr$HR_id) %>% 
            as.telemetry()
          
          # Creating CTMM fit objects for the smoothing
          M.IID <- ctmm.fit(telemetry_object) 
          GUESS <- ctmm.guess(telemetry_object,interactive=FALSE) 
          M.OUF <- ctmm.fit(telemetry_object,GUESS) 
          
          # Home Range Polygons
          KDE_95 <- akde(telemetry_object,M.IID) %>% 
            as.sf(level.UD = 0.95) %>% 
            mutate(id = indiv_id,.before = 1) %>% 
            mutate(int_id = interval_id,.after = id) %>% 
            mutate(hr_type = c("kde_95"),.after = int_id) %>% 
            slice(2)
          
          AKDE_95 <- akde(telemetry_object,M.OUF) %>% 
            as.sf(level.UD = 0.95) %>% 
            mutate(id = indiv_id,.before = 1) %>% 
            mutate(int_id = interval_id,.after = id) %>% 
            mutate(hr_type = c("akde_95"),.after = int_id) %>% 
            slice(2)
  
          # Export
          sf_export <- bind_rows(KDE_95,AKDE_95) %>% 
            st_write(paste0("1.DataManagement/ch2_data/clean/homerange_polygons/north/",
                            indiv_id,"_",interval_id,".shp"),
                     append = F,
                     quiet = T)
          
          return(i)
        }

#      Covariate Extraction                                                 ####

# List of Home Ranges
polygon_filepaths <- list.files("1.DataManagement/ch2_data/clean/homerange_polygons/north",
                                pattern = ".shp",
                                full.names = T)

# Covariate Extraction
HR_covs <- foreach(i = 1:length(polygon_filepaths),
                   .combine = bind_rows) %do% {
                     
                     HR_estimates <- st_read(polygon_filepaths[[i]],
                                             quiet = T)
                     
                     covs <- foreach(j = 1:nrow(HR_estimates),
                                     .combine = bind_rows) %do% {
                                       
                                       HR_estimate <- HR_estimates %>% slice(j)
                                       HR_estimate_vect <- vect(HR_estimate)
                                       
                                       # Landscape Metrics  
                                       
                                       cropped_landscape <- crop(cov_layers,HR_estimate_vect) %>% mask(vect(HR_estimate))
                                       
                                       hr_size <- tibble(hr_size_ha = expanse(HR_estimate_vect, unit="ha", transform=TRUE))
                                       
                                       # Landscape Metrics  
                                       
                                       cropped_landscape <- crop(cov_layers,HR_estimate_vect) %>% mask(vect(HR_estimate))
                                       
                                       hr_size <- tibble(hr_size_ha = expanse(HR_estimate_vect, unit="ha", transform=TRUE))
                                       
                                       mean_vals <- global(cropped_landscape[[c("north_dem",
                                                                                "slope",
                                                                                "aspect",
                                                                                "TRI")]],"mean", na.rm = T) %>% 
                                         rownames_to_column(var = "cov_name") %>% as_tibble %>% 
                                         pivot_wider(names_from = cov_name, 
                                                     values_from = mean) %>% 
                                         rename(dem = north_dem)
                                       
                                       contag <- lsm_l_contag(cropped_landscape[["north_nlcd"]]) %>% 
                                         dplyr::select(c(metric,value)) %>% 
                                         pivot_wider(names_from = metric, 
                                                     values_from = value)
                                       
                                       lsi <- lsm_l_lsi(cropped_landscape[["north_nlcd"]]) %>% 
                                         dplyr::select(c(metric,value)) %>% 
                                         pivot_wider(names_from = metric, 
                                                     values_from = value)
                                       
                                       shdi <- lsm_l_shdi(cropped_landscape[["north_nlcd"]]) %>% 
                                         dplyr::select(c(metric,value)) %>% 
                                         pivot_wider(names_from = metric, 
                                                     values_from = value)
                                       
                                       mean_patcharea <- lsm_c_area_mn(cropped_landscape[["north_nlcd"]]) %>% 
                                         mutate(class = as.character(class)) %>% 
                                         left_join(reclass_matrixNorth) %>% 
                                         dplyr::select(c(class_name, value)) %>% 
                                         pivot_wider(names_from = class_name,
                                                     values_from = value,
                                                     names_prefix = "meanpatcharea_")
                                       
                                       patch_density <- lsm_c_pd(cropped_landscape[["north_nlcd"]]) %>% 
                                         mutate(class = as.character(class)) %>% 
                                         left_join(reclass_matrixNorth) %>% 
                                         dplyr::select(c(class_name, value)) %>% 
                                         pivot_wider(names_from = class_name,
                                                     values_from = value,
                                                     names_prefix = "patchdensity_")
                                       
                                       landcover_proportions <- lsm_c_ca(cropped_landscape[["north_nlcd"]])%>% 
                                         mutate(class = as.character(class)) %>% 
                                         left_join(reclass_matrixNorth) %>% 
                                         dplyr::select(c(class_name, value)) %>% 
                                         mutate(value = value/hr_size$hr_size_ha) %>% 
                                         pivot_wider(names_from = class_name,
                                                     values_from = value,
                                                     names_prefix = "proportion_")
                                       
                                       covs <- bind_cols(hr_size,
                                                         mean_vals,
                                                         contag,
                                                         lsi,
                                                         shdi,
                                                         mean_patcharea,
                                                         patch_density,
                                                         landcover_proportions)
                                       
                                       return(covs)
                                     }
                     
                     final <- bind_cols(HR_estimates,covs) %>% st_drop_geometry() %>% 
                       dplyr::select(-c(name))
                     
                     return(final)
                   }

###############################################################################
#   South                                                                   ####
###############################################################################