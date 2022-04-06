#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-03-23 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(survival)
library(corrplot)
library(car)
library(stringr)
library(lares)

#      Functions                                                            ####

#      Data                                                                 ####
#        [North]                                                            ####

# Importing
deer_north_raw <- list.files("2.Chapter1/3.Output/CovariateExtraction/Covariates",
                         pattern = "N",
                         full.names = T) %>% lapply(read_csv)

# Fixing Age, Sex, and Location Type 

for (i in 1:length(deer_north_raw)) {
  deer_north_raw[[i]]$age <- ifelse(deer_north_raw[[i]]$age == F, "F",deer_north_raw[[i]]$age)
  print(i)
}

for (i in 1:length(deer_north_raw)) {
  deer_north_raw[[i]]$sex <- ifelse(deer_north_raw[[i]]$sex == F, "F",deer_north_raw[[i]]$sex)
  print(i)
}

for (i in 1:length(deer_north_raw)) {
  deer_north_raw[[i]]$location_type <- ifelse(deer_north_raw[[i]]$location_type == "used", 1,0)
  print(i)
}

for (i in 1:length(deer_north_raw)) {
  deer_north_raw[[i]]$point <- ifelse(deer_north_raw[[i]]$location_type == 1, 1,2)
  print(i)
}

# Binding Rows
deer_north <- bind_rows(deer_north_raw)

deer_north$observation_id<- rep(1:(nrow(deer_north)/6), each = 6)

###############################################################################
#   [Data Cleaning/Pre-processing]                                          ####
#      [Changing NAs to Zeros]                                              ####
deer_north[is.na(deer_north)] <- 0

#      [Correlation Assessments]                                            ####
#        [Correlation Matrix]                                               ####

corplot_north <-cor(deer_north[13:52])

#        [Correlation Barplot]                                              ####

cor_bargraph <-corr_cross(deer_north[13:52], rm.na = T, max_pvalue = 0.05, 
                          top = 35, grid = T)
cor_bargraph


###############################################################################
#   [Modeling]                                                              ####
#      [Proportion Model]                                                   ####

names(deer_north[str_which(names(deer_north),"proportion")])

cor_bargraph <-corr_cross(deer_north[str_which(names(deer_north),"proportion")], rm.na = T, max_pvalue = 0.05, 
                          top = 35, grid = T)

model_proportion_global<- clogit(location_type ~ strata(observation_id) +
                                   proportion_1 +
                                   proportion_2 +
                                   proportion_3+
                                   cluster(id),
                                 method = 'approximate', data = deer_north)

summary(model_proportion_global)


