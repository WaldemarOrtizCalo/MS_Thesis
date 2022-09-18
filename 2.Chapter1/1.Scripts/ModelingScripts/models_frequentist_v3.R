#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-09-16 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####
#      Library                                                              ####
library(tidyverse)
library(MuMIn)
library(foreach)
library(doParallel)

#      Functions                                                            ####

#      Data                                                                 ####

#        [North]                                                            ####

data_north <- list.files("1.DataManagement/used_available/north/final_data",
                         full.names = T,
                         pattern = ".csv") %>% lapply(read_csv)

names_north <- list.files("1.DataManagement/used_available/north/final_data",
                          full.names = F,
                          pattern = ".csv") %>% 
  str_remove(".csv") %>% 
  str_remove("_final")

#        [South]                                                            ####

data_south <- list.files("1.DataManagement/used_available/south/final_data",
                         full.names = T,
                         pattern = ".csv") %>% lapply(read_csv)

names_south <- list.files("1.DataManagement/used_available/south/final_data",
                          full.names = F,
                          pattern = ".csv") %>% 
  str_remove(".csv") %>% 
  str_remove("_final")

#        [Southeast]                                                        ####

data_southeast <- list.files("1.DataManagement/used_available/southeast/final_data",
                         full.names = T,
                         pattern = ".csv") %>% lapply(read_csv)

names_southeast <- list.files("1.DataManagement/used_available/southeast/final_data",
                          full.names = F,
                          pattern = ".csv") %>% 
  str_remove(".csv") %>% 
  str_remove("_final")

###############################################################################
#   [North]                                                                 ####
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
clusterExport(cl=cl, varlist=c("data_north","names_north"), envir=environment())

#      [Modeling]                                                           ####

# Modelling Protocol

foreach(i = 1:length(data_north)) %dopar% {
  
  # Separating specific dataframe
  df <- data_north[[i]]
  
  # Model
  model <- glm(choice ~ north_lsi + 
                 north_contag +
                 north_meanshape +
                 north_shdi +
                 north_proportion_2_buffer600m +
                 north_proportion_4_buffer600m +
                 north_proportion_6_buffer600m +
                 north_proportion_7_buffer600m, 
               data = df, 
               family = "binomial",na.action = "na.fail")
  
  # Model Dredging
  dredge <- dredge(model)
  
  # Dredge Export
  write_csv(x = dredge,
            file = paste0("2.Chapter1/3.Output/models_dredge/north/",names_north[i],"_dredge.csv"),
            append = F)
  
  # Return of the for loop
  print(i)
}

#      [Stopping Cluster]                                                   ####

stopCluster(cl)

###############################################################################
#   [South]                                                                 ####
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
clusterExport(cl=cl, varlist=c("data_south","names_south"), envir=environment())

#      [Modeling]                                                           ####

# Modelling Protocol

foreach(i = 1:length(data_south)) %dopar% {
  
  # Separating specific dataframe
  df <- data_south[[i]]
  
  # Model
  model <- glm(choice ~ south_lsi + 
                 south_contag +
                 south_meanshape +
                 south_shdi +
                 south_proportion_2_buffer600m +
                 south_proportion_4_buffer600m +
                 south_proportion_5_buffer600m +
                 south_proportion_6_buffer600m +
                 south_proportion_8_buffer600m +
                 south_proportion_9_buffer600m, 
               data = df, 
               family = "binomial",na.action = "na.fail")
  
  # Model Dredging
  dredge <- dredge(model)
  
  # Dredge Export
  write_csv(x = dredge,
            file = paste0("2.Chapter1/3.Output/models_dredge/south/",names_south[i],"_dredge.csv"),
            append = F)
  
  # Return of the for loop
  print(i)
}

#      [Stopping Cluster]                                                   ####

stopCluster(cl)

###############################################################################
#   [Southeast]                                                             ####
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