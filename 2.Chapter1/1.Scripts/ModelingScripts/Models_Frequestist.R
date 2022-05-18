#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-05-17 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(survival)
library(tidyverse)
library(lares)

#      Functions                                                            ####

#      Data                                                                 ####

#        [Deer Data]                                                        ####

data_southeast <- read_csv("1.DataManagement/CleanData/Chapter1_FinalData/Southeast_final.csv") %>% 
  rename(proportion_water = Southeast_proportion_1_buffer600m,
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
         meanshapeindex = Southeast_meanshapeindex,
         ShannonDiversityIndex = Southeast_shdi)

#        [Changes to Data]                                                   ####

# Making Choice a factor
data_north$choice <- factor(data_north$choice, levels = c("0","1"))
data_south$choice <- factor(data_south$choice, levels = c("0","1"))
data_southeast$choice <- factor(data_southeast$choice, levels = c("0","1"))

###############################################################################



