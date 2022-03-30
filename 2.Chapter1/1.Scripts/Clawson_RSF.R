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

for (i in 1:length(deer_north_raw)) {
  deer_north_raw[[i]]$point <- ifelse(deer_north_raw[[i]]$location_type == 1, 1,2)
  print(i)
}

# Binding Rows
deer_north <- bind_rows(deer_north_raw[1])

deer_north$observation_id<- rep(1:(nrow(deer_north)/6), each = 6)

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

summary(Rchoice(location_type ~ contagion , family = binomial("logit"),
                data = deer_north))

summary(mlogit(location_type ~ contagion | 0, 
               data = deer_north,
               shape = "long",
               chid.var = "observation_id",
               choice = "location_type",
               alt.var = 'point'))

summary(mlogit(formula = case ~ dist | 0, 
               data = hellbender, 
               shape = 'long',
               chid.var = 'set', 
               choice = 'case', 
               alt.var = 'point'))


test <- data.frame(id = deer_north$id,
                   set = deer_north$observation_id,
                   case = deer_north$location_type,
                   point = deer_north$point,
                   var1 = deer_north$proportion_4)

summary(mlogit(formula = case ~ var1 | 0, 
               data = test, 
               shape = 'long',
               chid.var = 'set', 
               choice = 'case', 
               alt.var = 'point'))

dfidx(data = test,
      idx = c("id","set", "point"),
      choice = "case")



summary(clogit(formula = case ~ dist + strata(set), 
               data = hellbender))


tictoc::toc()

