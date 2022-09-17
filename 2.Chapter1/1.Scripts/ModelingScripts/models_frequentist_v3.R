#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-09-16 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(MuMIn)

#      Functions                                                            ####

#      Data                                                                 ####

data_north <- list.files("1.DataManagement/used_available/north/final_data",
                         full.names = T,
                         pattern = ".csv") %>% lapply(read_csv)

###############################################################################


df <- data_north[[1]] 

summary(df$choice)

model <- glm(choice ~ north_lsi + 
               north_contag +
               north_proportion_5_buffer600m, 
    data = df, 
    family = "binomial",na.action = "na.fail")

dredge <- dredge(model)
