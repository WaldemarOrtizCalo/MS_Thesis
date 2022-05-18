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

data_north <- read_csv("1.DataManagement/CleanData/Chapter1_FinalData/North_final.csv") %>% 
  rename(choice = location_type,
         proportion_water = South_proportion_1_buffer600m,
         proportion_developed = South_proportion_2_buffer600m,
         proportion_barren = South_proportion_3_buffer600m,
         proportion_forest = South_proportion_4_buffer600m,
         proportion_shrub = South_proportion_5_buffer600m,
         proportion_grassland = South_proportion_6_buffer600m,
         proportion_cropland = South_proportion_7_buffer600m,
         proportion_wetland = South_proportion_8_buffer600m,
         contagion = South_contag,
         landscapeshapeindex = South_lsi,
         meanshapeindex = South_meanshapeindex,
         ShannonDiversityIndex = South_shdi)

data_south <- read_csv("1.DataManagement/CleanData/Chapter1_FinalData/South_final.csv") %>% 
  rename(choice = location_type,
         proportion_water = South_proportion_1_buffer600m,
         proportion_developed = South_proportion_2_buffer600m,
         proportion_barren = South_proportion_3_buffer600m,
         proportion_decidousforest = South_proportion_4_buffer600m,
         proportion_evergreenforest = South_proportion_5_buffer600m,
         proportion_mixedforest = South_proportion_6_buffer600m,
         proportion_shrub = South_proportion_7_buffer600m,
         proportion_grassland = South_proportion_8_buffer600m,
         proportion_cropland = South_proportion_9_buffer600m,
         proportion_wetland = South_proportion_10_buffer600m,
         contagion = South_contag,
         landscapeshapeindex = South_lsi,
         meanshapeindex = South_meanshapeindex,
         ShannonDiversityIndex = South_shdi)

data_southeast <- read_csv("1.DataManagement/CleanData/Chapter1_FinalData/Southeast_final.csv") %>% 
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
         meanshapeindex = Southeast_meanshapeindex,
         ShannonDiversityIndex = Southeast_shdi)

#        [Changes to Data]                                                  ####

# Making sure sex is formatted correctly 
data_southeast$sex <- ifelse(data_southeast$sex == F, "F", "M")


# Changing Used/Available data to right format 
data_north$choice <- ifelse(data_north$choice$choice == "used",1,0)
data_south$choice <- ifelse(data_south$choice$choice == "used",1,0)
data_southeast$choice <- ifelse(data_southeast$choice == "used",1,0)

# Making Choice a factor
data_north$choice <- factor(data_north$choice, levels = c("0","1"))
data_south$choice <- factor(data_south$choice, levels = c("0","1"))
data_southeast$choice <- factor(data_southeast$choice, levels = c("0","1"))

###############################################################################
#   [Covariate Exploration]                                                 ####
#      [North]                                                              ####
#        [Correlation Plots]                                                ####

north_cor_bargraph <-corr_cross(data_north[13:ncol(data_north)], rm.na = T, max_pvalue = 0.05, 
                                    top = 10, grid = T)
north_cor_bargraph

ggsave(filename = "north_corplot.png",
       plot = north_cor_bargraph,
       device = "png",
       path = "2.Chapter1/3.Output/CovariateExploration")

#        [Box-Whisker Plots]                                                ####

boxwhisk_data <- pivot_longer(data_north,cols = 13:ncol(data_north))

north_boxwhisk <- ggplot(boxwhisk_data, aes(x = name,y = value, fill = choice))+
  geom_boxplot() +
  labs(x = "covariates")

ggsave(filename = "north_boxwhisk.png",
       plot = north_boxwhisk,
       device = "png",
       path = "2.Chapter1/3.Output/CovariateExploration",
       width = 16,
       height = 6,
       units = "in")

#      [South]                                                              ####
#        [Correlation Plots]                                                ####

south_cor_bargraph <-corr_cross(data_south[13:ncol(data_south)], rm.na = T, max_pvalue = 0.05, 
                                    top = 10, grid = T)
south_cor_bargraph

ggsave(filename = "south_corplot.png",
       plot = south_cor_bargraph,
       device = "png",
       path = "2.Chapter1/3.Output/CovariateExploration")

#        [Box-Whisker Plots]                                                ####

boxwhisk_data <- pivot_longer(data_south,cols = 13:ncol(data_south))

south_boxwhisk <- ggplot(boxwhisk_data, aes(x = name,y = value, fill = choice))+
  geom_boxplot() +
  labs(x = "covariates")

ggsave(filename = "south_boxwhisk.png",
       plot = south_boxwhisk,
       device = "png",
       path = "2.Chapter1/3.Output/CovariateExploration",
       width = 16,
       height = 6,
       units = "in")

#      [Southeast]                                                          ####
#        [Correlation Plots]                                                ####

southeast_cor_bargraph <-corr_cross(data_southeast[13:ncol(data_southeast)], rm.na = T, max_pvalue = 0.05, 
                                top = 10, grid = T)
southeast_cor_bargraph

ggsave(filename = "southeast_corplot.png",
       plot = southeast_cor_bargraph,
       device = "png",
       path = "2.Chapter1/3.Output/CovariateExploration")

#        [Box-Whisker Plots]                                                ####

boxwhisk_data <- pivot_longer(data_southeast,cols = 13:ncol(data_southeast))

southeast_boxwhisk <- ggplot(boxwhisk_data, aes(x = name,y = value, fill = choice))+
  geom_boxplot() +
  labs(x = "covariates")

ggsave(filename = "southeast_boxwhisk.png",
       plot = southeast_boxwhisk,
       device = "png",
       path = "2.Chapter1/3.Output/CovariateExploration",
       width = 16,
       height = 6,
       units = "in")





###############################################################################
