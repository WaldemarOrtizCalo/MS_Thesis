#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-03-23 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(mlogit)
library(survival)

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


# Binding Rows
deer_north <- bind_rows(deer_north_raw[1])


###############################################################################

#   [Trying with one indiv]                                              ####

library(survival)

tictoc::tic()
summary(clogit(location_type ~ strata(observation_id) +
         contagion +
         cluster(id),
       method = 'approximate', data = deer_north))

summary(clogit(location_type ~ strata(observation_id) +
                 contagion,
               method = 'approximate', data = deer_north))


tictoc::toc()

