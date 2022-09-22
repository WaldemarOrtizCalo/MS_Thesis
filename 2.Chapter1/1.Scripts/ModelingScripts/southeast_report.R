#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-09-21 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####
#      Library                                                              ####

library(tidyverse)
library(MuMIn)
library(foreach)
library(doParallel)
library(amt)

#      Functions                                                            ####

#      Data                                                                 ####
#        [Logistic Regression Data]                                         ####

# Data 
data_southeast_logreg <- list.files("1.DataManagement/used_available/southeast/final_data",
                             full.names = T,
                             pattern = ".csv") %>% lapply(read_csv)

names_southeast <- list.files("1.DataManagement/used_available/southeast/final_data",
                              full.names = F,
                              pattern = ".csv") %>% 
  str_remove(".csv") %>% 
  str_remove("_final")

# Scaling
data_southeast_logreg_scaled <- data_southeast_logreg

for (i in 1:length(data_southeast_logreg_scaled)) {
  for (j in 3:ncol(data_southeast_logreg[[i]])) {
    data_southeast_logreg_scaled[[i]][,j] <- scale(data_southeast_logreg_scaled[[i]][,j])
  }
}


#        [Discrete Choice Data]                                             ####

# Data
data_southeast_dc <- read_csv("1.DataManagement/CleanData/Chapter1_FinalData/Southeast_final.csv") %>% 
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

#        [Changes to Data]                                                  

# Making sure sex is formatted correctly 
data_southeast_dc$sex <- ifelse(data_southeast_dc$sex == F, "F", "M")


# Changing Used/Available data to right format 
data_southeast_dc$choice <- ifelse(data_southeast_dc$choice == "used",1,0)

# Changing NAs to zero
data_southeast_dc[is.na(data_southeast_dc)] = 0

# Making Choice a factor 
data_southeast_dc$choice <- as.factor(data_southeast_dc$choice)

# Scaling
data_southeast_dc_scaled <- data_southeast_dc

for (i in 13:ncol(data_southeast_dc)) {
  data_southeast_dc_scaled[ ,i] <- scale(data_southeast_dc_scaled[ ,i])
}

###############################################################################
#   [Logistic Regression]                                                   ####
#      [Setting up Cluster]                                                 ####

# Cluster Number
cl <- makeCluster(4)
registerDoParallel(cl)

# Exporting Packages
clusterEvalQ(cl,
             {
               library(tidyverse)
               library(MuMIn)
               library(foreach)
               library(tidyverse)
             })

# Exporting data to clusters
clusterExport(cl=cl, varlist=c("data_southeast","names_southeast"), envir=environment())

#      [Modeling]                                                           ####

# Modelling Protocol

foreach(i = 1:length(data_southeast)) %dopar% {
  
  # Separating specific dataframe
  df <- data_southeast[[i]] 
  
  # Model
  model <- glm(choice ~ southeast_lsi + 
                 southeast_contag +
                 southeast_shdi +
                 southeast_proportion_2_buffer600m +
                 southeast_proportion_4_buffer600m +
                 southeast_proportion_5_buffer600m +
                 southeast_proportion_6_buffer600m +
                 southeast_proportion_8_buffer600m +
                 southeast_proportion_9_buffer600m, 
               data = df, 
               family = "binomial",na.action = "na.fail")
  
  # Model Dredging
  dredge <- dredge(model)
  
  # Dredge Export
  write_csv(x = dredge,
            file = paste0("2.Chapter1/3.Output/models_dredge/southeast/",names_southeast[i],"_dredge.csv"),
            append = F)
  
  # Return of the for loop
  print(i)
}

#      [Stopping Cluster]                                                   ####

stopCluster(cl)

###############################################################################
#   [Discrete Choice]                                                       ####

southeast_model_01_scaled <- data_southeast_dc_scaled %>% 
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
               strata(observation_id))

###############################################################################
