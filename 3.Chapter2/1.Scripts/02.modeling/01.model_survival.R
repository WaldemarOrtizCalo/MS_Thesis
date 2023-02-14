#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-02-06 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####
#      Library                                                              ####
library(tidyverse)
library(survival)
library(ggsurvfit)
#      Functions                                                            ####

#      Data                                                                 ####
#        [Locs]                                                             ####

# Deer Locs
locs_deer <- read_csv("1.DataManagement/ch2_data/clean/locs/deer_mortalitylocs.csv") %>% 
  mutate(age = factor(age,levels = c("A","Y","F")))

# Deer metadata
locs_metadata <- locs_deer %>% 
  dplyr::select(c(individual.local.identifier,ordered_int_id,sex,site,age,t_start,t_end,event)) %>% 
  unique()

# Identifying problem cases
problem_cases <- locs_metadata %>% 
  group_by(individual.local.identifier,ordered_int_id) %>% 
  summarize(count = n()) %>% 
  filter(count > 1)

# Cleaning
for (i in 1:nrow(problem_cases)) {
  
  sub <- locs_metadata[locs_metadata$individual.local.identifier == problem_cases[[i,1]] & 
                         locs_metadata$ordered_int_id == problem_cases[[i,2]],] 
  
  age_class <- sub$age %>% .[which.min(.)]
  
  locs_metadata[locs_metadata$individual.local.identifier == problem_cases[[i,1]] & 
                  locs_metadata$ordered_int_id == problem_cases[[i,2]], "age"] <- age_class
  
  print(i)
}

# Identifying clean ones
locs_metadata_clean <- locs_metadata %>% unique()

#        [Covs]                                                             ####

# This will be replaced with the join cov dataframe
covs <- read_csv("1.DataManagement/ch2_data/clean/cov_extractions/north_final_covs.csv")

#        [Joining Covs and Loc data]                                        ####

data_final <- covs %>% 
  left_join(locs_metadata_clean)


###############################################################################
#   Kaplan-Meier Models                                                     ####
#      North                                                                ####
#        All individuals                                                    ####

# Creating Survival Object
surv_object <- Surv(time = data_final$t_start,
                 time2 = data_final$t_end,
                 event = data_final$event,
                 type = "counting")

# Fitting Kaplan-Meier Model 
model_kapmier_all <- survfit(surv_object ~ 1, data=data_final)

# Plot
model_kapmier_all %>% ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + add_confidence_interval() +
  add_risktable()

#        Sex-Based                                                          ####

# Creating Survival Object
surv_object <- Surv(time = data_final$t_start,
                    time2 = data_final$t_end,
                    event = data_final$event,
                    type = "counting")

# Fitting Kaplan-Meier Model 
model_kapmier_sex <- survfit(surv_object ~ sex, data=data_final)

# Plot
model_kapmier_sex %>% ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + add_confidence_interval() +
  add_risktable()

###############################################################################