#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-03-23 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(mlogit)

#      Functions                                                            ####

#      Data                                                                 ####
#        [North]                                                            ####

# Importing
deer_north <- list.files("2.Chapter1/3.Output/CovariateExtraction/Covariates",
                         pattern = "N",
                         full.names = T) %>% lapply(read_csv)

# Fixing Age and Sex Covariate 

for (i in 1:length(deer_north)) {
  deer_north[[i]]$age <- ifelse(deer_north[[i]]$age == F, "F",deer_north[[i]]$age)
}

for (i in 1:length(deer_north)) {
  deer_north[[i]]$sex <- ifelse(deer_north[[i]]$sex == F, "F",deer_north[[i]]$sex)
}

for (i in 1:length(deer_north)) {
  deer_north[[i]]$location_type <- ifelse(deer_north[[i]]$location_type == "used", T,F)
}

# Binding Rows
deer_north <- bind_rows(deer_north)

###############################################################################

#   [Trying with one indiv]                                              ####

deer_north_mlogitformat <- dfidx(deer_north,
                                 idx = list(c("id","observation_id")),
                                 choice = "location_type")
