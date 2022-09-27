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
library(lubridate)
library(broom)
library(jtools)

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

# Changing timestamp column
data_southeast_dc$t <-  as_datetime(data_southeast_dc$t)

data_southeast_dc <- data_southeast_dc %>% rename(timestamp = t)

#           [Adding Seasonality]                                            ####

# Spring:  1 March – 31 May (3 mo.)
# Summer:  1 June – 30 August (3 mo.)
# Fall:  1 September – 30 November (3 mo.)
# Winter:  1 December – 28 February (3 mo.)

# Start
time_partition_start <- expand_grid(year = (data_southeast_dc$timestamp %>% year() %>% unique() %>% .[1]-1) : 
                                      (data_southeast_dc$timestamp %>% year() %>% unique() %>% .[length(.)]+1),
                                    month = c("03","06","09","12"),
                                    day = "01") %>% 
  mutate(season = ifelse(month == "03", "spring",
                         ifelse(month == "06", "summer",
                                ifelse(month == "09", "fall",
                                       ifelse(month == "12", "winter",NA))))) %>% 
  mutate(date = paste(year,month,day,sep = "-"),.keep = "unused",.before = 1) %>% 
  mutate(date = paste0(date, " 00:00:00"))

# End
time_partition_end <- expand_grid(year = (data_southeast_dc$timestamp %>% year() %>% unique() %>% .[1]-1) : 
                                    (data_southeast_dc$timestamp %>% year() %>% unique() %>% .[length(.)]+1),
                                  month = c("05","08","11","02"))

time_partition_end <- time_partition_end %>% 
  mutate(day = ifelse(time_partition_end$month == "05","31",
                      ifelse(time_partition_end$month == "08","31",
                             ifelse(time_partition_end$month == "11","30",
                                    ifelse(time_partition_end$month == "02","28",NA))))) %>% 
  mutate(year = ifelse(.$month == "02",year + 1, year)) %>% 
  mutate(date = paste(year,month,day,sep = "-"),.keep = "unused") %>% 
  mutate(date = paste0(date, " 23:59:59"))

# Creating Time Partition dataframe
time_partitions <- data.frame(start = time_partition_start$date,
                              end = time_partition_end$date,
                              season = time_partition_start$season)

ints <- list()

for (i in 1:nrow(time_partitions)) {
  ints[[i]] <- interval(time_partitions[i,1], time_partitions[i,2])
}

# Mutating dataframe and adding season
data_southeast_dc <- data_southeast_dc %>% 
  mutate(season = case_when(
    timestamp %within% ints[[1]] ~ time_partitions$season %>% unique() %>% .[1],
    timestamp %within% ints[[2]] ~ time_partitions$season %>% unique() %>% .[2],
    timestamp %within% ints[[3]] ~ time_partitions$season %>% unique() %>% .[3],
    timestamp %within% ints[[4]] ~ time_partitions$season %>% unique() %>% .[4],
    timestamp %within% ints[[5]] ~ time_partitions$season %>% unique() %>% .[1],
    timestamp %within% ints[[6]] ~ time_partitions$season %>% unique() %>% .[2],
    timestamp %within% ints[[7]] ~ time_partitions$season %>% unique() %>% .[3],
    timestamp %within% ints[[8]] ~ time_partitions$season %>% unique() %>% .[4],
    timestamp %within% ints[[9]] ~ time_partitions$season %>% unique() %>% .[1],
    timestamp %within% ints[[10]] ~ time_partitions$season %>% unique() %>% .[2],
    timestamp %within% ints[[11]] ~ time_partitions$season %>% unique() %>% .[3],
    timestamp %within% ints[[12]] ~ time_partitions$season %>% unique() %>% .[4],
    timestamp %within% ints[[13]] ~ time_partitions$season %>% unique() %>% .[1],
    timestamp %within% ints[[14]] ~ time_partitions$season %>% unique() %>% .[2],
    timestamp %within% ints[[15]] ~ time_partitions$season %>% unique() %>% .[3],
    timestamp %within% ints[[16]] ~ time_partitions$season %>% unique() %>% .[4],
    timestamp %within% ints[[17]] ~ time_partitions$season %>% unique() %>% .[1],
    timestamp %within% ints[[18]] ~ time_partitions$season %>% unique() %>% .[2],
    timestamp %within% ints[[19]] ~ time_partitions$season %>% unique() %>% .[3],
    timestamp %within% ints[[20]] ~ time_partitions$season %>% unique() %>% .[4],
    timestamp %within% ints[[21]] ~ time_partitions$season %>% unique() %>% .[1],
    timestamp %within% ints[[22]] ~ time_partitions$season %>% unique() %>% .[2],
    timestamp %within% ints[[23]] ~ time_partitions$season %>% unique() %>% .[3],
    timestamp %within% ints[[24]] ~ time_partitions$season %>% unique() %>% .[4],
    timestamp %within% ints[[25]] ~ time_partitions$season %>% unique() %>% .[1],
    timestamp %within% ints[[26]] ~ time_partitions$season %>% unique() %>% .[2],
    timestamp %within% ints[[27]] ~ time_partitions$season %>% unique() %>% .[3],
    timestamp %within% ints[[28]] ~ time_partitions$season %>% unique() %>% .[4],
    TRUE ~ "NA"),.after = timestamp)

# Fixing leap years
data_southeast_dc$season <- ifelse(month(data_southeast_dc$timestamp) == 2 & day(data_southeast_dc$timestamp) == 29, "winter",data_southeast_dc$season)

#           [Scaling]                                                       ####

# Scaling
data_southeast_dc_scaled <- data_southeast_dc

for (i in 14:ncol(data_southeast_dc)) {
  data_southeast_dc_scaled[ ,i] <- scale(data_southeast_dc_scaled[ ,i])
}

###############################################################################
#   [Logistic Regression]                                                   ####
#      [Fall]                                                               ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_logreg[[1]] 

#        [Global Model]                                                     ####

# Model
model_fall <- glm(choice ~ southeast_lsi + 
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

# Summary 

summary(model_fall)

#      [Winter]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_logreg[[4]] 

#        [Global Model]                                                     ####

# Model
model_winter <- glm(choice ~ southeast_lsi + 
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

# Summary
summary(model_winter)

#      [Spring]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_logreg[[2]] 

#        [Global Model]                                                     ####

# Model
model_spring <- glm(choice ~ southeast_lsi + 
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

# Summary 
summary(model_spring)

#        [model_v2]                                                         ####

# Global model had one covariate that did not cross the 0.05 threshold
# Covariate: southeast_proportion_5_buffer600m (p-value: 0.433)

# Model
model_spring <- glm(choice ~ southeast_lsi + 
                      southeast_contag +
                      southeast_shdi +
                      southeast_proportion_2_buffer600m +
                      southeast_proportion_4_buffer600m +
                      southeast_proportion_6_buffer600m +
                      southeast_proportion_8_buffer600m +
                      southeast_proportion_9_buffer600m, 
                    data = df, 
                    family = "binomial",na.action = "na.fail")

# Summary 
summary(model_spring)
#      [Summer]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_logreg[[3]] 

#        [Global Model]                                                     ####

# Model
model_summer <- glm(choice ~ southeast_lsi + 
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

# Summary
summary(model_summer)

###############################################################################
#   [Logistic Regression - Scaled]                                          ####
#      [Fall]                                                               ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_logreg_scaled[[1]] 

#        [Global Model]                                                     ####

# Model
model_fall_scaled <- glm(choice ~ southeast_lsi + 
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

# Summary 

summary(model_fall_scaled)

#      [Winter]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_logreg_scaled[[4]] 

#        [Global Model]                                                     ####

# Model
model_winter_scaled <- glm(choice ~ southeast_lsi + 
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

# Summary
summary(model_winter_scaled)

#      [Spring]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_logreg_scaled[[2]] 

#        [Global Model]                                                     ####

# Model
model_spring_scaled <- glm(choice ~ southeast_lsi + 
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

# Summary 
summary(model_spring_scaled)

#        [model_v2]                                                         ####

# Global model had one covariate that did not cross the 0.05 threshold
# Covariate: southeast_proportion_5_buffer600m (p-value: 0.433)

# Model
model_spring_scaled <- glm(choice ~ southeast_lsi + 
                      southeast_contag +
                      southeast_shdi +
                      southeast_proportion_2_buffer600m +
                      southeast_proportion_4_buffer600m +
                      southeast_proportion_6_buffer600m +
                      southeast_proportion_8_buffer600m +
                      southeast_proportion_9_buffer600m, 
                    data = df, 
                    family = "binomial",na.action = "na.fail")

# Summary 
summary(model_spring_scaled)

#      [Summer]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_logreg_scaled[[3]] 

#        [Global Model]                                                     ####

# Model
model_summer_scaled <- glm(choice ~ southeast_lsi + 
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

# Summary
summary(model_summer_scaled)

###############################################################################
#   [Discrete Choice]                                                       ####
#      [Fall]                                                               ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_dc %>% filter(season == "fall")

#        [Global Model]                                                     ####

# Model
model_dc_fall <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_fall)

#        [model_v2]                                                         ####

# Eliminated proportion_evergreenforest (p-value of 0.541)

# Model
model_dc_fall <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_fall)


#        [model_v3]                                                         ####

# Eliminated contagion (p-value of 0.392)

# Model
model_dc_fall <- df %>% 
  fit_clogit(choice ~ landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_fall)

#        [model_v4]                                                         ####

# Eliminated proportion_mixedforest (p-value of 0.2098)

# Model
model_dc_fall <- df %>% 
  fit_clogit(choice ~ landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_fall)

#        [model_v5]                                                         ####

# Eliminated meanshapeindex (p-value of 0.0573)

# Model
model_dc_fall <- df %>% 
  fit_clogit(choice ~ landscapeshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_fall)

#      [Winter]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_dc %>% filter(season == "winter")

#        [Global Model]                                                     ####

# Model
model_dc_winter <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_winter)

#        [model_v2]                                                         ####

# Eliminated meanshapeindex (p-value of 0.06278)

# Model
model_dc_winter <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_winter)

#      [Spring]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_dc %>% filter(season == "spring")

#        [Global Model]                                                     ####

# Model
model_dc_spring <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_spring)

#        [model_v2]                                                         ####

# Eliminated proportion_mixedforest (p-value of 0.3694)

# Model
model_dc_spring <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_spring)

#        [model_v3]                                                         ####

# Eliminated proportion_evergreenforest (p-value of 0.0730)

# Model
model_dc_spring <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_spring)

#      [Summer]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_dc %>% filter(season == "summer")

#        [Global Model]                                                     ####

# Model
model_dc_summer <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_summer)

#        [model_v2]                                                         ####

# Eliminated proportion_developed (p-value of 0.62213)

# Model
model_dc_summer <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_summer)

#        [model_v3]                                                         ####

# Eliminated proportion_mixedforest (p-value of 0.3659)

# Model
model_dc_summer <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_summer)

###############################################################################
#   [Discrete Choice - Scaled]                                              ####
#      [Fall]                                                               ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_dc_scaled %>% filter(season == "fall")

#        [Global Model]                                                     ####

# Model
model_dc_scaled_fall <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_fall)

#        [model_v2]                                                         ####

# Eliminated proportion_evergreenforest (p-value of 0.541)

# Model
model_dc_scaled_fall <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_fall)

#        [model_v3]                                                         ####

# Eliminated contagion (p-value of 0.392)

# Model
model_dc_scaled_fall <- df %>% 
  fit_clogit(choice ~ landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_fall)

#        [model_v4]                                                         ####

# Eliminated proportion_mixedforest (p-value of 0.2098)

# Model
model_dc_scaled_fall <- df %>% 
  fit_clogit(choice ~ landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_fall)

#        [model_v5]                                                         ####

# Eliminated meanshapeindex (p-value of 0.0573)

# Model
model_dc_scaled_fall <- df %>% 
  fit_clogit(choice ~ landscapeshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_fall)

#      [Winter]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_dc_scaled %>% filter(season == "winter")

#        [Global Model]                                                     ####

# Model
model_dc_scaled_winter <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_winter)

#        [model_v2]                                                         ####

# Eliminated meanshapeindex (p-value of 0.06278)

# Model
model_dc_scaled_winter <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_winter)

#      [Spring]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_dc_scaled %>% filter(season == "spring")

#        [Global Model]                                                     ####

# Model
model_dc_scaled_spring <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_spring)

#        [model_v2]                                                         ####

# Eliminated proportion_mixedforest (p-value of 0.3694)

# Model
model_dc_scaled_spring <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_spring)

#        [model_v3]                                                         ####

# Eliminated proportion_evergreenforest (p-value of 0.0730)

# Model
model_dc_scaled_spring <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_spring)


#      [Summer]                                                             ####
#        [Data]                                                             ####

# Separating specific dataframe
df <- data_southeast_dc_scaled %>% filter(season == "summer")

#        [Global Model]                                                     ####

# Model
model_dc_scaled_summer <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_developed + 
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_summer)

#        [model_v2]                                                         ####

# Eliminated proportion_developed (p-value of 0.62213)

# Model
model_dc_scaled_summer <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_mixedforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_summer)

#        [model_v3]                                                         ####

# Eliminated proportion_mixedforest (p-value of 0.3659)

# Model
model_dc_scaled_summer <- df %>% 
  fit_clogit(choice ~ contagion + 
               landscapeshapeindex +
               meanshapeindex +
               proportion_decidousforest +
               proportion_evergreenforest + 
               proportion_grassland +
               proportion_cropland + 
               strata(observation_id))

# Summary 

summary(model_dc_scaled_summer)

###############################################################################
#   [Model_results table]                                                   ####
model_list_logreg <- list(model_fall,
                          model_fall_scaled,
                          model_winter,
                          model_winter_scaled,
                          model_spring,
                          model_spring_scaled,
                          model_summer,
                          model_summer_scaled)

model_list_dc <- list(model_dc_fall,
                      model_dc_scaled_fall,
                      model_dc_winter,
                      model_dc_scaled_winter,
                      model_dc_spring,
                      model_dc_scaled_spring,
                      model_dc_summer,
                      model_dc_scaled_summer)

broom::tidy(model_list_dc[[1]]$model)

broom::tidy(model_fall)

###############################################################################
#   [Predictive Plots - Log Reg]                                            ####
#      [Fall]                                                               ####

# Data
df <- data_southeast_logreg[[1]] 

# Model Object
summary(model_fall)

#        [Cov 1]                                                            ####

 # Prediction dataset
pred_data <- with(data = df, 
                  data.frame(southeast_lsi = seq(from = 0, to = max(df$southeast_lsi),by = 0.1),
                             southeast_contag = mean(southeast_contag,na.rm = T),
                             southeast_shdi = mean(southeast_shdi,na.rm = T),
                             southeast_proportion_2_buffer600m = mean(southeast_proportion_2_buffer600m, na.rm=TRUE),
                             southeast_proportion_4_buffer600m = mean(southeast_proportion_4_buffer600m, na.rm=TRUE),
                             southeast_proportion_5_buffer600m = mean(southeast_proportion_5_buffer600m, na.rm=TRUE),
                             southeast_proportion_6_buffer600m = mean(southeast_proportion_6_buffer600m, na.rm=TRUE),
                             southeast_proportion_8_buffer600m = mean(southeast_proportion_8_buffer600m, na.rm=TRUE),
                             southeast_proportion_9_buffer600m = mean(southeast_proportion_9_buffer600m, na.rm=TRUE)))

preds <- predict(m, pred_data, type="response", se.fit=TRUE)

predf <- preds$fit # predicted
lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
upper <- preds$fit + (1.96*preds$se.fit) # upper bounds

plotdata <- data.frame("x" = pred_data[,1],
                       "predicted" = predf,
                       "lower" = lower,
                       "upper" = upper)

plot <- ggplot(data = plotdata, aes(x = x, y = predicted)) + 
  geom_line(size = 1)+
  geom_line(aes(x = x, y = lower), color = "blue")+
  geom_line(aes(x = x, y = upper), color = "blue")+
  labs(x = "Landscape Shape Index", y = "Probability of Use")+
  theme_bw()+
  theme(axis.text = element_text(size = 10))

name_file <-  "2.Chapter1/3.Output/predicticed_probabilityuse/logreg_fall_lsi"

ggsave(filename = paste0(name_file,".png"),
       plot = plot,
       device = "png",
       width = 6,
       height = 4,
       units = "in")

saveRDS(plot,
        file = paste0(name_file,".rds"))

#        [Cov 2]                                                            ####

# Prediction dataset
pred_data <- with(data = df, 
                  data.frame(southeast_lsi = mean(southeast_lsi,na.rm = T),
                             southeast_contag = seq(from = 0, to = 1, by = 0.1),
                             southeast_shdi = mean(southeast_shdi,na.rm = T),
                             southeast_proportion_2_buffer600m = mean(southeast_proportion_2_buffer600m, na.rm=TRUE),
                             southeast_proportion_4_buffer600m = mean(southeast_proportion_4_buffer600m, na.rm=TRUE),
                             southeast_proportion_5_buffer600m = mean(southeast_proportion_5_buffer600m, na.rm=TRUE),
                             southeast_proportion_6_buffer600m = mean(southeast_proportion_6_buffer600m, na.rm=TRUE),
                             southeast_proportion_8_buffer600m = mean(southeast_proportion_8_buffer600m, na.rm=TRUE),
                             southeast_proportion_9_buffer600m = mean(southeast_proportion_9_buffer600m, na.rm=TRUE)))

preds <- predict(m, pred_data, type="response", se.fit=TRUE)

predf <- preds$fit # predicted
lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
upper <- preds$fit + (1.96*preds$se.fit) # upper bounds

plotdata <- data.frame("x" = pred_data[,1],
                       "predicted" = predf,
                       "lower" = lower,
                       "upper" = upper)

plot <- ggplot(data = plotdata, aes(x = x, y = predicted)) + 
  geom_line(size = 1)+
  geom_line(aes(x = x, y = lower), color = "blue")+
  geom_line(aes(x = x, y = upper), color = "blue")+
  labs(x = "Landscape Shape Index", y = "Probability of Use")+
  theme_bw()+
  theme(axis.text = element_text(size = 10))

name_file <-  "2.Chapter1/3.Output/predicticed_probabilityuse/logreg_fall_lsi"

ggsave(filename = paste0(name_file,".png"),
       plot = plot,
       device = "png",
       width = 6,
       height = 4,
       units = "in")

saveRDS(plot,
        file = paste0(name_file,".rds"))


#      [Winter]                                                             ####
#      [Spring]                                                             ####
#      [Summer]                                                             ####

#   [Predictive Plots - Log Reg Scaled]                                     ####
#   [Predictive Plots - Discrete Choice]                                    ####
#   [Predictive Plots - Discrete Choice Scaled]                             ####
###############################################################################

library(jtools)

effect_plot(model_fall, pred = southeast_proportion_5_buffer600m, interval = TRUE, plot.points = F)



names(m$coefficients)

covnames_logreg <- data.frame(cov_names = names(data_southeast_logreg[[1]])[3:length(names(data_southeast_logreg[[1]]))]) %>% 
  mutate(cov_label = case_when(cov_names == "southeast_contag" ~ "Contagion",
                               cov_names == "southeast_lsi" ~ "Landscape Shape Index",
                               cov_names == "southeast_shdi" ~ "Shannon's Diversity Index",
                               cov_names == "southeast_proportion_1_buffer600m"  ~ "Proportion of Water",
                               cov_names == "southeast_proportion_2_buffer600m"  ~ "Proportion of Developed",
                               cov_names == "southeast_proportion_3_buffer600m"  ~ "Proportion of Barren",
                               cov_names == "southeast_proportion_4_buffer600m"  ~ "Proportion of Deciduous Forest",
                               cov_names == "southeast_proportion_5_buffer600m"  ~ "Proportion of Evergreen Forest",
                               cov_names == "southeast_proportion_6_buffer600m"  ~ "Proportion of Mixed Forest",
                               cov_names == "southeast_proportion_7_buffer600m"  ~ "Proportion of Shrub",
                               cov_names == "southeast_proportion_8_buffer600m"  ~ "Proportion of Grassland",
                               cov_names == "southeast_proportion_9_buffer600m"  ~ "Proportion of Cropland",
                               cov_names == "southeast_proportion_10_buffer600m" ~ "Proportion of Wetland"
  ))

covnames_dc<- data.frame(cov_names = names(data_southeast_dc)[14:length(names(data_southeast_dc))]) %>% 
  mutate(cov_label = case_when(cov_names == "contagion" ~ "Contagion",
                               cov_names == "landscapeshapeindex" ~ "Landscape Shape Index",
                               cov_names == "southeast_shdi" ~ "Shannon's Diversity Index",
                               cov_names == "proportion_water"  ~ "Proportion of Water",
                               cov_names == "proportion_developed"  ~ "Proportion of Developed",
                               cov_names == "proportion_barren"  ~ "Proportion of Barren",
                               cov_names == "proportion_decidousforest"  ~ "Proportion of Deciduous Forest",
                               cov_names == "proportion_evergreenforest"  ~ "Proportion of Evergreen Forest",
                               cov_names == "proportion_mixedforest"  ~ "Proportion of Mixed Forest",
                               cov_names == "proportion_shrub"  ~ "Proportion of Shrub",
                               cov_names == "proportion_grassland"  ~ "Proportion of Grassland",
                               cov_names == "proportion_cropland"  ~ "Proportion of Cropland",
                               cov_names == "proportion_wetland" ~ "Proportion of Wetland"))


logreg_model_list <- list(model_fall,
                          model_winter,
                          model_spring,
                          model_summer,
                          model_fall_scaled,
                          model_winter_scaled,
                          model_spring_scaled,
                          model_summer_scaled)

logreg_model_list_names <- list('model_fall',
                                "model_winter",
                                "model_spring",
                                "model_summer",
                                "model_fall_scaled",
                                "model_winter_scaled",
                                "model_spring_scaled",
                                "model_summer_scaled")

dc_model_list <- list(model_dc_fall,
                      model_dc_winter,
                      model_dc_spring,
                      model_dc_summer,
                      model_dc_scaled_fall,
                      model_dc_scaled_winter,
                      model_dc_scaled_spring,
                      model_dc_scaled_summer)

dc_model_list_names <- list('model_dc_fall',
                            "model_dc_winter",
                            "model_dc_spring",
                            "model_dc_summer",
                            "model_dc_scaled_fall",
                            "model_dc_scaled_winter",
                            "model_dc_scaled_spring",
                            "model_dc_scaled_summer")


model <- logreg_model_list[[1]]

model$coefficients %>% names()
p<-effect_plot(model, pred = 'southeast_proportion_8_buffer600m', interval = TRUE, plot.points = F)


ggsave(filename = paste0("2.Chapter1/3.Output/southeast_predictedprob/",logreg_model_list_names[1],".png"),
       plot = p,
       device = "png",
       width = 6,
       height = 4,
       units = "in")

"G:\My Drive\Research\UMontana\2.INPROGRESS\1.Masters\MS_Thesis_Analysis\2.Chapter1\3.Output\southeast_predictedprob"





