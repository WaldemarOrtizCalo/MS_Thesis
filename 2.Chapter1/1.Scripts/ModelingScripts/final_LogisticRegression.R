#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-12-03 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(lares)
library(foreach)
library(doParallel)

#      Functions                                                            ####

#      Data                                                                 ####
#        North                                                              ####
data_north <- list.files("1.DataManagement/ch1_data/north/model_data",
                         full.names = T,pattern = ".csv")

names_north <- list.files("1.DataManagement/ch1_data/north/model_data",
                         full.names = F,pattern = ".csv") %>% 
  str_remove(".csv") %>% 
  str_replace("final","model")

#        South                                                              ####
data_south <- list.files("1.DataManagement/ch1_data/south/model_data",
                         full.names = T,pattern = ".csv")

names_south <- list.files("1.DataManagement/ch1_data/south/model_data",
                          full.names = F,pattern = ".csv") %>% 
  str_remove(".csv") %>% 
  str_replace("final","model")

#        Southeast                                                          ####
data_southeast <- list.files("1.DataManagement/ch1_data/southeast/model_data",
                         full.names = T,pattern = ".csv")

names_southeast <- list.files("1.DataManagement/ch1_data/southeast/model_data",
                          full.names = F,pattern = ".csv") %>% 
  str_remove(".csv") %>% 
  str_replace("final","model")

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



#      Models                                                               ####
#        Dredge Models                                                      ####
#           Start of Cluster                                                ####

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
clusterExport(cl=cl, varlist=c("data","names_north"), envir=environment())

#           Dredge                                                          ####
foreach(i = 1:length(data)) %dopar% {

  # Separating specific dataframe
  df <- data[[i]]
  name <- names_north[[i]]
  
  # Making the list of covariate names and eliminating undesired covariates
  covs <- names(df) %>% 
    .[(str_which(.,"geometry")+1):length(.)] %>% 
    str_subset("shrub",negate = T) %>% 
    str_subset("wetland",negate = T) %>% 
    str_subset("barren",negate = T) 
  
  # Building the formula
  formula <- as.formula(paste("choice ~ ", paste(covs, collapse= "+")))
  
  # Model
  model <- glm(formula = formula, 
               data = df, 
               family = "binomial",na.action = "na.fail")
  
  # Model Dredging
  dredge <- dredge(model)
  
  # Dredge Export
  write_csv(x = dredge,
            file = paste0("2.Chapter1/3.Output/models_dredge_landscapelevel/",names_north[i],"_dredge.csv"),
            append = F)
  
  # Return of the for loop
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