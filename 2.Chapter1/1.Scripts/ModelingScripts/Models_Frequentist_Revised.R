#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-08-27 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(lares)
library(amt)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(gridExtra)
library(TwoStepCLogit)

#      Functions                                                            ####

#      Data                                                                 ####

#        [Import data]                                                      ####

data_north <- read_csv("1.DataManagement/CleanData/Chapter1_FinalData/north_final.csv") %>% 
  rename(choice = location_type,
         proportion_water = north_proportion_1_buffer600m,
         proportion_developed = north_proportion_2_buffer600m,
         proportion_barren = north_proportion_3_buffer600m,
         proportion_forest = north_proportion_4_buffer600m,
         proportion_shrub = north_proportion_5_buffer600m,
         proportion_grassland = north_proportion_6_buffer600m,
         proportion_cropland = north_proportion_7_buffer600m,
         proportion_wetland = north_proportion_8_buffer600m,
         contagion = north_contag,
         landscapeshapeindex = north_lsi,
         meanshapeindex = north_meanshape,
         ShannonDiversityIndex = north_shdi)

data_south <- read_csv("1.DataManagement/CleanData/Chapter1_FinalData/south_final.csv") %>% 
  rename(choice = location_type,
         proportion_water = south_proportion_1_buffer600m,
         proportion_developed = south_proportion_2_buffer600m,
         proportion_barren = south_proportion_3_buffer600m,
         proportion_decidousforest = south_proportion_4_buffer600m,
         proportion_evergreenforest = south_proportion_5_buffer600m,
         proportion_mixedforest = south_proportion_6_buffer600m,
         proportion_shrub = south_proportion_7_buffer600m,
         proportion_grassland = south_proportion_8_buffer600m,
         proportion_cropland = south_proportion_9_buffer600m,
         proportion_wetland = south_proportion_10_buffer600m,
         contagion = south_contag,
         landscapeshapeindex = south_lsi,
         meanshapeindex = south_meanshape,
         ShannonDiversityIndex = south_shdi)

data_southeast <- read_csv("1.DataManagement/CleanData/Chapter1_FinalData/southeast_final.csv") %>% 
  rename(choice = location_type,
         proportion_water = Southeast_proportion_1_buffer600m,
         proportion_developed = Southeast_proportion_2_buffer600m,
         proportion_barren = Southeast_proportion_3_buffer600m,
         proportion_decidousforest = Southeast_proportion_4_buffer600m,
         proportion_evergreenforest = Southeast_proportion_5_buffer600m,
         proportion_mixedforest = Southeast_proportion_6_buffer600m,
         proportion_shrub = Southeast_proportion_7_buffer600m,
         proportion_grassland = Southeast_proportion_8_buffer600m,
         proportion_cropland = Southeast_proportion_9_buffer600m,
         proportion_wetland = Southeast_proportion_10_buffer600m,
         contagion = Southeast_contag,
         landscapeshapeindex = Southeast_lsi,
         meanshapeindex = Southeast_meanshape,
         ShannonDiversityIndex = Southeast_shdi)

#        [Changing the format of choice variable]                           ####

data_north$choice <- ifelse(data_north$choice == "used",0,1)
data_south$choice <- ifelse(data_south$choice == "used",0,1)
data_southeast$choice <- ifelse(data_southeast$choice == "used",0,1)
###############################################################################
#   [Global Models]                                                         ####

#      [North]                                                              ####
#        [Global Model: No Random Effects]                                  ####

north_global <-  data_north %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex + 
               proportion_water + 
               proportion_developed + 
               proportion_forest + 
               proportion_shrub + 
               proportion_grassland +
               proportion_cropland  +
               strata(observation_id))

#        [Global Model: Random Effects]                                     ####

north_global_MixedEffects <-Ts.estim(formula = choice ~ contagion + 
                    landscapeshapeindex +
                    meanshapeindex + 
                    proportion_water + 
                    proportion_developed + 
                    proportion_forest + 
                    proportion_shrub + 
                    proportion_grassland +
                    proportion_cropland  +
                    strata(observation_id) + cluster(id),
                  data = data_north, 
                  random = ~ contagion + 
                    landscapeshapeindex +
                    meanshapeindex + 
                    proportion_water + 
                    proportion_developed + 
                    proportion_forest + 
                    proportion_shrub + 
                    proportion_grassland +
                    proportion_cropland, all.m.1=F)

#      [South]                                                              ####

#        [Global Model: No Random Effects]                                  ####

south_global <-  data_south %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex + 
               proportion_water + 
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest +
               proportion_mixedforest + 
               proportion_shrub + 
               proportion_grassland +
               proportion_cropland  +
               strata(observation_id))

#        [Global Model: Random Effects]                                     ####

south_global_MixedEffects <-Ts.estim(formula = choice ~ contagion + 
                                       landscapeshapeindex +
                                       meanshapeindex + 
                                       proportion_water + 
                                       proportion_developed + 
                                       proportion_decidousforest +
                                       proportion_evergreenforest +
                                       proportion_mixedforest + 
                                       proportion_shrub + 
                                       proportion_grassland +
                                       proportion_cropland  +
                                       strata(observation_id) + cluster(id),
                                     data = data_south, 
                                     random = ~ contagion + 
                                       landscapeshapeindex +
                                       meanshapeindex + 
                                       proportion_water + 
                                       proportion_developed + 
                                       proportion_decidousforest +
                                       proportion_evergreenforest +
                                       proportion_mixedforest + 
                                       proportion_shrub + 
                                       proportion_grassland +
                                       proportion_cropland, all.m.1=F)

#      [Southeast]                                                          ####

twostep<-Ts.estim(formula = choice ~  contagion + 
                    landscapeshapeindex +
                    meanshapeindex + 
                    proportion_water + 
                    proportion_developed + 
                    proportion_decidousforest +
                    proportion_evergreenforest + 
                    proportion_mixedforest + 
                    proportion_shrub + 
                    proportion_grassland +
                    proportion_cropland  +
                    strata(observation_id) + cluster(id),
                  data = data_south[1:20000,], 
                  random = ~ contagion + 
                    landscapeshapeindex +
                    meanshapeindex + 
                    proportion_water + 
                    proportion_developed + 
                    proportion_decidousforest +
                    proportion_evergreenforest + 
                    proportion_mixedforest + 
                    proportion_shrub + 
                    proportion_grassland +
                    proportion_cropland , all.m.1=F)

southeast_model_01 <- data_south %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex + 
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_barren + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               proportion_grassland +
               proportion_cropland  +
               strata(observation_id))

#        [Global Model: No Random Effects]                                  ####

southeast_global <-  data_southeast[1:20000,] %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex + 
               proportion_water + 
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest +
               proportion_mixedforest + 
               proportion_shrub + 
               proportion_grassland +
               proportion_cropland  +
               strata(observation_id))

#        [Global Model: Random Effects]                                     ####

southeast_global_MixedEffects <-Ts.estim(formula = choice ~ contagion + 
                                       landscapeshapeindex +
                                       meanshapeindex + 
                                       proportion_water + 
                                       proportion_developed + 
                                       proportion_decidousforest +
                                       proportion_evergreenforest +
                                       proportion_mixedforest + 
                                       proportion_shrub + 
                                       proportion_grassland +
                                       proportion_cropland  +
                                       strata(observation_id) + cluster(id),
                                     data = data_southeast[1:20000,], 
                                     random = ~ contagion + 
                                       landscapeshapeindex +
                                       meanshapeindex + 
                                       proportion_water + 
                                       proportion_developed + 
                                       proportion_decidousforest +
                                       proportion_evergreenforest +
                                       proportion_mixedforest + 
                                       proportion_shrub + 
                                       proportion_grassland +
                                       proportion_cropland, all.m.1=F)

###############################################################################