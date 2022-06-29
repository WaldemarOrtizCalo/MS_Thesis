#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-06-28 

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

#      Data                                                                 ####

#        [Deer Data]                                                        ####


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

#               [Model Final]                                               ####

southeast_model_clogit <- readRDS("2.Chapter1/3.Output/Models/Southeast_model_final_clogit.rda")

###############################################################################
#   [Southeast]                                                             ####
#      [Prediction Plots]                                                   ####

# Model Probabilities
probabilities <- southeast_model_clogit %>% predict(type = "expected")

# Figuresx
cov_names <- c("Contagion",
               "Landscape Shape Index",
               "Proportion of Water",
               "Proportion of Wetland",
               "Proportion of Developed",
               "Proportion of Deciduous Forest",
               "Proportion of Evergreen Forest",
               "Proportion of Mixed Forest",
               "Proportion of Shrub",
               "Mean Patch Area (Evergreen Forest)",
               "Mean Patch Area (Mixed Forest)",
               "Mean Patch Area (Grassland)")

cov_chosen <- c(18,19,21,22,23,25,26,27,28,14,15,16)

prediction <- data_southeast[c(1:12,cov_chosen)]

pred <- predict(southeast_model_clogit,newdata = prediction)

prediction$prob <- exp(pred)/(1+exp(pred))

pred_df <- data.frame(data_southeast[cov_chosen[i]],prediction$prob)


for (i in 1:length(cov_chosen)) {
  print(i)
  
  pred_df <- data.frame(data_southeast[cov_chosen[i]],prediction$prob)
  
  plot <- ggplot(pred_df, aes(x =  pred_df[,1],y = pred_df[,2])) +
    stat_smooth(method="glm", method.args = list(family="binomial"))+
    labs(x = cov_names[i], y = "Probability of Use")
  
  ggsave(filename = paste0("Southeast_PredictiveProbability_",cov_names[i],".png"),
         plot = plot,
         device = "png",
         path = "2.Chapter1/3.Output/PredictivePlots",
         height = 6,
         width = 8)
}

