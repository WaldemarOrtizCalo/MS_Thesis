#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-05-17 

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
         meanpatcharea_decidousforest = Southeast_class4_MeanPatchArea,
         meanpatcharea_evergreenforest = Southeast_class5_MeanPatchArea,
         meanpatcharea_mixedforest = Southeast_class6_MeanPatchArea,
         meanpatcharea_grassland = Southeast_class8_MeanPatchArea,
         meanpatcharea_cropland = Southeast_class9_MeanPatchArea
         #ShannonDiversityIndex = Southeast_shdi # Leaving this here while I fix layer
    )

#        [Changes to Data]                                                  ####

# Making sure sex is formatted correctly 
data_southeast$sex <- ifelse(data_southeast$sex == F, "F", "M")


# Changing Used/Available data to right format 
data_north$choice <- ifelse(data_north$choice$choice == "used",1,0)
data_south$choice <- ifelse(data_south$choice$choice == "used",1,0)
data_southeast$choice <- ifelse(data_southeast$choice == "used",1,0)

# Changing NAs to zero
data_southeast[is.na(data_southeast)] = 0

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
       width = 12,
       height = 8,
       units = "in",
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
#   [Models]                                                                ####
#      [Southeast - Non-Scaled]                                             ####
#        [Global]                                                           ####
#           [Backwards Step Selection]                                      ####
#               [Model 1]                                                   ####

# Global Model with all covariates included 
southeast_model_01 <- data_southeast %>% 
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
               proportion_cropland + 
               meanpatcharea_decidousforest +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_grassland +
               meanpatcharea_cropland +
               strata(observation_id))

summary(southeast_model_01)

saveRDS(southeast_model_01, file = "2.Chapter1/3.Output/Models/Southeast_model_01.rda")

#               [Model 2]                                                   ####

# Removing Proportion Cropland. It is highly correlated and NA in results  
southeast_model_02 <- data_southeast %>% 
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
               meanpatcharea_decidousforest +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_grassland +
               meanpatcharea_cropland +
               strata(observation_id))

summary(southeast_model_02)

saveRDS(southeast_model_02, file = "2.Chapter1/3.Output/Models/Southeast_model_02.rda")



#               [Model 3]                                                   ####

# Removed mean patch grassland. Highly correlated with proportion grassland. 

southeast_model_03 <- data_southeast %>% 
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
               meanpatcharea_decidousforest +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_grassland +
               meanpatcharea_cropland +
               strata(observation_id))

summary(southeast_model_03)

saveRDS(southeast_model_03, file = "2.Chapter1/3.Output/Models/Southeast_model_03.rda")

#               [Model 4]                                                   ####

# Removed meanshapeindex It had a p-value of 0.81439
southeast_model_04 <- data_southeast %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_barren + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               meanpatcharea_decidousforest +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_grassland +
               meanpatcharea_cropland +
               strata(observation_id))


summary(southeast_model_04)

saveRDS(southeast_model_04, file = "2.Chapter1/3.Output/Models/Southeast_model_04.rda")

#               [Model 5]                                                   ####

# Removed meanpatcharea decidousforest. It had a p-value of 0.81821
southeast_model_05 <- data_southeast %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_barren + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_grassland +
               meanpatcharea_cropland +
               strata(observation_id))

summary(southeast_model_05)

saveRDS(southeast_model_05, file = "2.Chapter1/3.Output/Models/Southeast_model_05.rda")

#               [Model 6]                                                   ####

# Removed meanpatcharea cropland It had a p-value of 0.642012
southeast_model_06 <- data_southeast %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_barren + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_grassland +
               strata(observation_id))

summary(southeast_model_06)

saveRDS(southeast_model_06, file = "2.Chapter1/3.Output/Models/Southeast_model_06.rda")

#               [Model 7]                                                   ####

# Removed proportion barren. It had a p-value of 0.1230

southeast_model_07 <- data_southeast %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_grassland +
               strata(observation_id))

summary(southeast_model_07)

saveRDS(southeast_model_07, file = "2.Chapter1/3.Output/Models/Southeast_model_07.rda")

#               [Model Final]                                               ####


southeast_model_final <- data_southeast %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_grassland +
               strata(observation_id))

summary(southeast_model_final)

saveRDS(southeast_model_final, file = "2.Chapter1/3.Output/Models/Southeast_model_final.rda")

#      [Southeast - Scaled]                                                 ####

data_southeast_scaled <- data_southeast

for (i in 13:ncol(data_southeast)) {
  data_southeast_scaled[ ,i] <- scale(data_southeast_scaled[ ,i])
  print(i)
}

#           [Backwards Step Selection]                                      ####
#               [Model 1]                                                   ####

# Global Model with all covariates included 
southeast_model_01_scaled <- data_southeast_scaled %>% 
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
               proportion_cropland + 
               meanpatcharea_decidousforest +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_grassland +
               meanpatcharea_cropland +
               strata(observation_id))

summary(southeast_model_01_scaled)

saveRDS(southeast_model_01_scaled, file = "2.Chapter1/3.Output/Models/Southeast_model_01_scaled.rda")

#               [Model 2]                                                   ####

# Eliminated Proportion of Cropland. Produced NAs and highly correlated. 
southeast_model_02_scaled <- data_southeast_scaled %>% 
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
               meanpatcharea_decidousforest +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_grassland +
               meanpatcharea_cropland +
               strata(observation_id))

summary(southeast_model_02_scaled)

saveRDS(southeast_model_02_scaled, file = "2.Chapter1/3.Output/Models/Southeast_model_02_scaled.rda")

#               [Model 3]                                                   ####

# Eliminated Mean Shape Index. P-value of 0.97053

southeast_model_03_scaled <- data_southeast_scaled %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_barren + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               proportion_grassland +
               meanpatcharea_decidousforest +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_grassland +
               meanpatcharea_cropland +
               strata(observation_id))

summary(southeast_model_03_scaled)

saveRDS(southeast_model_03_scaled, file = "2.Chapter1/3.Output/Models/Southeast_model_03_scaled.rda")

#               [Model 4]                                                   ####

# Eliminated Mean Patch Area - Grassland. P-value of 0.92337

southeast_model_04_scaled <- data_southeast_scaled %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_barren + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               proportion_grassland +
               meanpatcharea_decidousforest +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_cropland +
               strata(observation_id))

summary(southeast_model_04_scaled)

saveRDS(southeast_model_04_scaled, file = "2.Chapter1/3.Output/Models/Southeast_model_04_scaled.rda")

#               [Model 5]                                                   ####

# Eliminated Mean Patch Area - Decidous Forest.  P-value of 0.8756

southeast_model_05_scaled <- data_southeast_scaled %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_barren + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               proportion_grassland +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               meanpatcharea_cropland +
               strata(observation_id))

summary(southeast_model_05_scaled)

saveRDS(southeast_model_05_scaled, file = "2.Chapter1/3.Output/Models/Southeast_model_05_scaled.rda")

#               [Model 6]                                                   ####

# Eliminated Mean Patch Area - Cropland.  P-value of 0.78984

southeast_model_06_scaled <- data_southeast_scaled %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_barren + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               proportion_grassland +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               strata(observation_id))

summary(southeast_model_06_scaled)

saveRDS(southeast_model_06_scaled, file = "2.Chapter1/3.Output/Models/Southeast_model_06_scaled.rda")

#               [Model 7]                                                   ####

# Eliminated proportion_barren.  P-value of 0.1028

southeast_model_07_scaled <- data_southeast_scaled %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               proportion_grassland +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               strata(observation_id))

summary(southeast_model_07_scaled)

saveRDS(southeast_model_07_scaled, file = "2.Chapter1/3.Output/Models/Southeast_model_07_scaled.rda")

#               [Model Final]                                               ####


southeast_model_final_scaled <- data_southeast_scaled %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_water + 
               proportion_wetland + 
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_shrub + 
               proportion_grassland +
               meanpatcharea_evergreenforest +
               meanpatcharea_mixedforest +
               strata(observation_id))


summary(southeast_model_final_scaled)

saveRDS(southeast_model_final_scaled, file = "2.Chapter1/3.Output/Models/Southeast_model_final_scaled.rda")

###############################################################################
#   [Export All Model Data as tables]                                       ####

file_list <- list.files("2.Chapter1/3.Output/Models",
                        pattern = ".rda",
                        full.names = T)

file_names <- list.files("2.Chapter1/3.Output/Models",
                        pattern = ".rda",
                        full.names = F) %>% str_remove(".rda")

for (i in 1:length(file_list)) {
  
  l <- readRDS(file_list[i])
  
  model <- broom::tidy(l$model)
  
  write_csv(model,
            file = paste0("2.Chapter1/3.Output/Models/",file_names[i],".csv"))
  
  print(i)
}

###############################################################################
#   [Model Validation and Inspection]                                       ####
#      [Southeast]                                                          ####
#        [Model import]                                                     ####

Southeast_model_final <- readRDS("2.Chapter1/3.Output/Models/Southeast_model_final.rda") 

#        [Effect Plots]                                                     ####

data_southeast_model <- broom::tidy(Southeast_model_final$model) %>% 
  add_column("plot_index"= rep(x = c(1,2,3),each = nrow(.)/3))

p1 <- ggplot(subset(data_southeast_model, plot_index == 1),
       aes( x = term , y = estimate))+
  geom_point()

p2 <- ggplot(subset(data_southeast_model, plot_index == 2),
       aes( x = term , y = estimate))+
  geom_point()

p3 <- ggplot(subset(data_southeast_model, plot_index == 3),
       aes( x = term , y = estimate))+
  geom_point()


grid.arrange(p1,p2,p3,nrow = 3)


p4 <- ggplot(subset(data_southeast_model, plot_index == 1),
             aes( x = term , y = exp(estimate)))+
  geom_point()+
  theme(axis.title.x = element_blank())

p5 <- ggplot(subset(data_southeast_model, plot_index == 2),
             aes( x = term , y = exp(estimate)))+
  geom_point()+
  theme(axis.title.x = element_blank())

p6 <- ggplot(subset(data_southeast_model, plot_index == 3),
             aes( x = term , y = exp(estimate)))+
  geom_point()+
  labs(x = "Covariates")


grid.arrange(p4,p5,p6,nrow = 3)



