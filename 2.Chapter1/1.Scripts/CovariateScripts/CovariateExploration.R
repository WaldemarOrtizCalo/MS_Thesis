#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-06-29 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####
#      Library                                                              ####
library(tidyverse)
library(survival)
library(amt)
library(lares)
library(amt)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(gridExtra)

#      Functions                                                            ####

#      Data                                                                 ####
#        [Deer Data]                                                        ####

# Southeast
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
#   [Boxplot]                                                               ####


boxwhisk_data <- pivot_longer(data_southeast,cols = 13:ncol(data_southeast))

boxwhisk_data$value <- ifelse(is.na(boxwhisk_data$value), 0, boxwhisk_data$value)


cov_name <- unique(boxwhisk_data$name)

for (i in 1:length(cov_name)) {
  print(i)
  
    boxwhisk <- ggplot(subset(boxwhisk_data, name == cov_name[[i]]), 
                               aes(x = name,y = value, fill = choice))+
    geom_boxplot() +
    labs(x = "Covariate")
  
  ggsave(filename = paste0("southeast_",cov_name[[i]],".png"),
         plot = boxwhisk,
         device = "png",
         path = "2.Chapter1/3.Output/CovariateExploration",
         width = 6,
         height = 6,
         units = "in")
}
