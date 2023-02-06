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

# Class Legends
class_legend <- data.frame(class = unique(cov_layers$north_nlcd),
                           landcover_type = c("water",
                                              "developed",
                                              "barren",
                                              "forest",
                                              "shrub",
                                              "grassland",
                                              "cropland",
                                              "wetland")) %>% rename(class = north_nlcd)

#      Home Range Calculation                                               ####
#        Filtering ids and periods with greater than 20 locs                ####

valid_homeranges <- locs %>% 
  group_by(individual.local.identifier,ordered_int_id) %>% 
  summarize(n_locs = n()) %>% 
  filter(n_locs > 20)

#        Home Range Polygon calculation and export                          ####

hr_log <- foreach(i = 1:nrow(valid_homeranges),
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
          
          KDE_50 <- akde(telemetry_object,M.IID) %>% 
            as.sf(level.UD = 0.50) %>% 
            mutate(id = indiv_id,.before = 1) %>% 
            mutate(int_id = interval_id,.after = id) %>% 
            mutate(hr_type = c("kde_50"),.after = int_id) %>% 
            slice(2)
          
          AKDE_95 <- akde(telemetry_object,M.OUF) %>% 
            as.sf(level.UD = 0.95) %>% 
            mutate(id = indiv_id,.before = 1) %>% 
            mutate(int_id = interval_id,.after = id) %>% 
            mutate(hr_type = c("akde_95"),.after = int_id) %>% 
            slice(2)
          
          AKDE_50 <- akde(telemetry_object,M.OUF) %>% 
            as.sf(level.UD = 0.50) %>% 
            mutate(id = indiv_id,.before = 1) %>% 
            mutate(int_id = interval_id,.after = id) %>% 
            mutate(hr_type = c("akde_50"),.after = int_id) %>% 
            slice(2)
          
          # Export
          sf_export <- bind_rows(KDE_95,KDE_50,AKDE_95,AKDE_50) %>% 
            st_write(paste0("1.DataManagement/ch2_data/clean/homerange_polygons/north/",
                            indiv_id,"_",interval_id,".shp"),
                     append = F)
          
          return(i)
        }

#      Covariate Extraction [DEV]                                           ####
#        Polygon Import                                                     ####

polygon_filepaths <- list.files("1.DataManagement/ch2_data/clean/homerange_polygons/north",
                                pattern = ".shp",
                                full.names = T)

# Start of the for loop 

HR_estimates <- st_read(polygon_filepaths[[1]])

# Start of second loop [ each HR estimate ]

HR_estimate <- HR_estimates %>% slice(1)

# Landscape Metrics  

cropped_landscape <- crop(cov_layers,vect(HR_estimate)) %>% mask(vect(HR_estimate))

# Start of next for loop

# Home Range Area
hr_area <- as.numeric(st_area(HR_estimate))/10000 %>% 
  data.frame(hr_area = .)

# Mean of all topographic covariates
mean_vals <- global(cropped_landscape[[c("north_dem",
                                         "slope",
                                         "aspect",
                                         "TRI")]],"mean", na.rm = T) %>% 
  rownames_to_column(var = "cov_name") %>% as_tibble %>% 
  pivot_wider(names_from = cov_name, 
              values_from = mean) 

# Landscape config metrics

landcover_lsi <- lsm_l_lsi(cropped_landscape[["north_nlcd"]]) %>% 
  .[c("metric","value")] %>% 
  pivot_wider(names_from = metric, 
              values_from = value) 

landcover_shdi <- lsm_l_shdi(cropped_landscape[["north_nlcd"]]) %>% 
  .[c("metric","value")] %>% 
  pivot_wider(names_from = metric, 
              values_from = value) 

landcover_contag <- lsm_l_contag(cropped_landscape[["north_nlcd"]]) %>% 
  .[c("metric","value")] %>% 
  pivot_wider(names_from = metric, 
              values_from = value) 

# Mean patch size
landcover_meanarea <- lsm_c_area_mn(cropped_landscape[["north_nlcd"]]) %>%
  left_join(class_legend, by = "class") %>% 
  .[c("landcover_type","value")] %>% 
  left_join(class_legend,., by = "landcover_type") %>% 
  .[c("landcover_type","value")] %>% 
  pivot_wider(names_from = landcover_type, 
              values_from = value) 

colnames(landcover_meanarea) <- paste("meanarea",colnames(landcover_meanarea),sep="_")
  
# Total patch area

landcover_totalarea <- lsm_c_ca(cropped_landscape[["north_nlcd"]]) %>%
  left_join(class_legend, by = "class") %>% 
  .[c("landcover_type","value")] %>% 
  left_join(class_legend,., by = "landcover_type") %>% 
  .[c("landcover_type","value")] %>% 
  pivot_wider(names_from = landcover_type, 
              values_from = value) 

colnames(landcover_totalarea) <- paste("totalarea",colnames(landcover_totalarea),sep="_")

# Percent landcover

landcover_percent <- landcover_totalarea/as.numeric(hr_area)

colnames(landcover_percent) <- colnames(landcover_totalarea) %>% 
  str_replace(pattern = "meanarea", replacement = "percent")

# Patch Density

landcover_patchdensity <- lsm_c_pd(cropped_landscape[["north_nlcd"]]) %>%
  left_join(class_legend, by = "class") %>% 
  .[c("landcover_type","value")] %>% 
  left_join(class_legend,., by = "landcover_type") %>% 
  .[c("landcover_type","value")] %>% 
  pivot_wider(names_from = landcover_type, 
              values_from = value) 

colnames(landcover_patchdensity) <- paste("patchdensity",colnames(landcover_patchdensity),sep="_")

# Joining Everything

covs <- bind_cols(hr_area,
          landcover_contag,
          landcover_lsi,
          landcover_shdi,
          landcover_meanarea,
          landcover_totalarea,
          landcover_patchdensity,
          landcover_percent) %>% 
  mutate(id = HR_estimate$id,
         int_id = HR_estimate$int_id,
         hr_type = HR_estimate$hr_type,.before = 1)


###############################################################################
#   South                                                                   ####
###############################################################################