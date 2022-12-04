#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-12-03 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(lares)

#      Functions                                                            ####

#      Data                                                                 ####
#        North                                                              ####
data_north <- list.files("1.DataManagement/ch1_data/north/model_data",
                         full.names = T,pattern = ".csv")

#        South                                                              ####
data_south <- list.files("1.DataManagement/ch1_data/south/model_data",
                         full.names = T,pattern = ".csv")

#        Southeast                                                          ####
data_southeast <- list.files("1.DataManagement/ch1_data/southeast/model_data",
                         full.names = T,pattern = ".csv")

###############################################################################
#   North                                                                   ####
#      Data                                                                 ####
#        Data Import                                                        ####

data <- lapply(data_north, read_csv)
#        Correlation Plots                                                  ####

for (i in 1:length(data_north)) {
  
  df <- data[[i]]
  name <- list.files("1.DataManagement/ch1_data/north/model_data",
                     full.names = F,pattern = ".csv") %>% 
    .[[i]] %>% 
    str_replace(".csv",".png") %>% 
    str_replace("final","corplot")
  
  
  cor_bargraph <-corr_cross(df[(str_which(names(df), "geometry")+1):ncol(df)], rm.na = T, max_pvalue = 0.06, 
                            top = 20, grid = T)
  
  ggsave(filename = name,
         plot = cor_bargraph,
         device = "png",
         path = "2.Chapter1/3.Output/covariate_analysis/north",
         width = 14,
         height = 6,
         units = "in")
  
  print(i)
}



###############################################################################
#   South                                                                   ####
#      Data                                                                 ####
#        Data Import                                                        ####
data <- lapply(data_south, read_csv)

#        Correlation Plots                                                  ####

for (i in 1:length(data_south)) {
  
  df <- data[[i]]
  name <- list.files("1.DataManagement/ch1_data/south/model_data",
                     full.names = F,pattern = ".csv") %>% 
    .[[i]] %>% 
    str_replace(".csv",".png") %>% 
    str_replace("final","corplot")
  
  
  cor_bargraph <-corr_cross(df[(str_which(names(df), "geometry")+1):ncol(df)], rm.na = T, max_pvalue = 0.06, 
                            top = 20, grid = T)
  
  ggsave(filename = name,
         plot = cor_bargraph,
         device = "png",
         path = "2.Chapter1/3.Output/covariate_analysis/south",
         width = 14,
         height = 6,
         units = "in")
  
  print(i)
}



###############################################################################
#   Southeast                                                               ####
#      Data                                                                 ####
#        Data Import                                                        ####
data <- lapply(data_southeast, read_csv)

#        Correlation Plots                                                  ####

for (i in 1:length(data_southeast)) {
  
  df <- data[[i]]
  name <- list.files("1.DataManagement/ch1_data/southeast/model_data",
                     full.names = F,pattern = ".csv") %>% 
    .[[i]] %>% 
    str_replace(".csv",".png") %>% 
    str_replace("final","corplot")
  
  
  cor_bargraph <-corr_cross(df[(str_which(names(df), "geometry")+1):ncol(df)], rm.na = T, max_pvalue = 0.06, 
                            top = 20, grid = T)
  
  ggsave(filename = name,
         plot = cor_bargraph,
         device = "png",
         path = "2.Chapter1/3.Output/covariate_analysis/southeast",
         width = 14,
         height = 6,
         units = "in")
  
  print(i)
}



###############################################################################
