#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-12-07 

# Purpose: 

#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(lares)
library(foreach)
library(doParallel)
library(MuMIn)
library(jtools)
library(rstanarm)
library(brms)

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
  
  covs <- names(df) %>% 
    .[(str_which(.,"geometry")+1):length(.)] %>% 
    str_subset("shrub",negate = T) %>% 
    str_subset("wetland",negate = T) %>% 
    str_subset("barren",negate = T) %>% 
    str_subset("water",negate = T)  %>%
    str_subset("TRI",negate = T) %>% 
    str_subset("developed",negate = T) %>% 
    c()
  
  covlist <- foreach(v = 1:length(covs),.combine = c) %do% {str_which(names(df),pattern = covs[v])} %>% unique()
  
  cov_df <- df[covlist]
  
  cor_bargraph <-corr_cross(cov_df, rm.na = T, max_pvalue = 0.06, 
                            top = 40,
                            plot = T)
  
  cor_data <-corr_cross(cov_df, rm.na = T, max_pvalue = 0.06, 
                        top = 60,
                        plot = F)
  
  ggsave(filename = name,
         plot = cor_bargraph,
         device = "png",
         path = "2.Chapter1/3.Output/covariate_analysis/north",
         width = 14,
         height = 6,
         units = "in")
  
  write_csv(cor_data,
            paste0("2.Chapter1/3.Output/covariate_analysis/north/",str_remove(name,".png"),".csv"))
  
  print(i)
}

#        UsedAvail Plots                                                    ####

for (i in 1:length(data_north)) {
  
  # Iteration Monitor 1 
  print(paste0("Starting Dataset ",i, " out of ",length(data_north)))
  
  # Dataframe Extraction and Name
  df <- data[[i]]
  name <- list.files("1.DataManagement/ch1_data/north/model_data",
                     full.names = F,pattern = ".csv") %>% 
    .[[i]] %>% 
    str_remove(".csv") %>% 
    str_replace("final","_usedavail")
  
  # Making a List of Covariates
  covs <- names(df) %>% 
    .[(str_which(.,"geometry")+1):length(.)] %>% 
    str_subset("shrub",negate = T) %>% 
    str_subset("wetland",negate = T) %>% 
    str_subset("barren",negate = T) %>% 
    str_subset("water",negate = T)  %>%
    str_subset("TRI",negate = T) %>% 
    str_subset("developed",negate = T) %>% 
    append("choice",after = 0) %>% 
    c()
  
  # Covariate List 
  covlist <- foreach(v = 1:length(covs),.combine = c) %do% 
    {str_which(names(df),pattern = covs[v])} %>% 
    unique()
  
  # Subsetting Covariates from dataframe
  cov_df <- df[covlist]
  
  # Preparing the data in box and whisker plot format
  boxwhisk_data <- pivot_longer(cov_df,
                                cols = 2:ncol(cov_df))
  
  # Replacing NA values with Zero
  boxwhisk_data$value <- ifelse(is.na(boxwhisk_data$value),
                                0,
                                boxwhisk_data$value)
  
  # Extracting covariates Names
  cov_name <- covs[2:length(covs)]
  
  # Export folder
  export_dir <- paste0("2.Chapter1/3.Output/covariate_analysis/north/used_avail_plots","/",name)
  
  for (v in 1:length(cov_name)) {
    
    # Box and Whisker Plot
    boxwhisk_plot <- ggplot(subset(boxwhisk_data, name == cov_name[[v]]),
                            aes(x = name, y = value, fill = as.factor(choice)))+
      geom_boxplot()+
      labs(fill = "Choice", x = "Covariate") +
      theme_bw()
    
    # Plot Export
    ggsave(filename = paste0(cov_name[[v]],".png"),
           plot = boxwhisk_plot,
           device = "png",
           path = export_dir,
           width = 6,
           height = 6,
           units = "in")
    
    # Iteration Monitor
    print(paste0(v," out of ", length(cov_name)))
  }
}

#      Model                                                                ####
#        [Model Settings]                                                   ####
# Model Settings
chains <- 3 
iter <- 4000
warmup <- iter*.25
thin <- 1
cores <- chains
refresh <- iter * 0.05

#        [Model]                                                            ####

foreach(i = 1:length(data_north)) %do% {
  
  # Separating specific dataframe
  df_raw <- data[[i]]
  name <- names_north[[i]]
  
  df <- df_raw
  
  for (j in (str_which(names(df),"geometry")+1):ncol(df)) {
    df[,j] <- scale(df[,j])
  }
  
  # Making the list of covariate names and eliminating undesired covariates
  covs <- names(df) %>% 
    .[(str_which(.,"geometry")+1):length(.)] %>% 
    str_subset("shrub",negate = T) %>% 
    str_subset("wetland",negate = T) %>% 
    str_subset("barren",negate = T) %>% 
    str_subset("water",negate = T)  %>%
    str_subset("TRI",negate = T) %>% 
    str_subset("developed",negate = T) %>% 
    c()
  
  # Building the formula
  formula <- as.formula(paste("choice ~ ", 
                              paste(covs, collapse= "+"),
                              "+ (1 + ",
                              paste(covs, collapse= "+"),
                              "| individual.local.identifier)"))
  
  # Model
  
  model <- brm(
    formula = formula,
    data = df,
    family = binomial(link = "logit"),
    prior = prior(normal(0, 1)),
    refresh = refresh,
    chains = chains, 
    iter = iter,
    warmup = warmup,
    thin = thin,
    cores = cores
  )
  
  saveRDS(object = model,
          file = paste0("2.Chapter1/3.Output/models_bayesian_randomintercept/north/bayesian_",name,".RDS"))
  
  return(print(paste0("North: ",i," out of ", length(data_north), " is completed")))
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
  
  covs <- names(df) %>% 
    .[(str_which(.,"geometry")+1):length(.)] %>% 
    str_subset("shrub",negate = T) %>% 
    str_subset("wetland",negate = T) %>% 
    str_subset("barren",negate = T) %>% 
    str_subset("water",negate = T)  %>%
    str_subset("TRI",negate = T) %>% 
    str_subset("developed",negate = T) %>%
    str_subset("evergreen",negate = T) %>%
    str_subset("mixed",negate = T) %>%
    c()
  
  covlist <- foreach(v = 1:length(covs),.combine = c) %do% {str_which(names(df),pattern = covs[v])} %>% unique()
  
  cov_df <- df[covlist]
  
  cor_bargraph <-corr_cross(cov_df, rm.na = T, max_pvalue = 0.06, 
                            top = 40,
                            plot = T)
  
  cor_data <-corr_cross(cov_df, rm.na = T, max_pvalue = 0.06, 
                        top = 60,
                        plot = F)
  
  ggsave(filename = name,
         plot = cor_bargraph,
         device = "png",
         path = "2.Chapter1/3.Output/covariate_analysis/south",
         width = 14,
         height = 6,
         units = "in")
  
  write_csv(cor_data,
            paste0("2.Chapter1/3.Output/covariate_analysis/south/",str_remove(name,".png"),".csv"))
  
  print(i)
}

#        UsedAvail Plots                                                    ####

for (i in 1:length(data_south)) {
  
  # Iteration Monitor 1 
  print(paste0("Starting Dataset ",i, " out of ",length(data_south)))
  
  # Dataframe Extraction and Name
  df <- data[[i]]
  name <- list.files("1.DataManagement/ch1_data/south/model_data",
                     full.names = F,pattern = ".csv") %>% 
    .[[i]] %>% 
    str_remove(".csv") %>% 
    str_replace("final","_usedavail")
  
  # Making a List of Covariates
  covs <- names(df) %>% 
    .[(str_which(.,"geometry")+1):length(.)] %>% 
    str_subset("shrub",negate = T) %>% 
    str_subset("wetland",negate = T) %>% 
    str_subset("barren",negate = T) %>% 
    str_subset("water",negate = T)  %>%
    str_subset("TRI",negate = T) %>% 
    str_subset("developed",negate = T) %>%
    str_subset("evergreen",negate = T) %>%
    str_subset("mixed",negate = T) %>%
    append("choice",after = 0) %>% 
    c()
  
  # Covariate List 
  covlist <- foreach(v = 1:length(covs),.combine = c) %do% 
    {str_which(names(df),pattern = covs[v])} %>% 
    unique()
  
  # Subsetting Covariates from dataframe
  cov_df <- df[covlist]
  
  # Preparing the data in box and whisker plot format
  boxwhisk_data <- pivot_longer(cov_df,
                                cols = 2:ncol(cov_df))
  
  # Replacing NA values with Zero
  boxwhisk_data$value <- ifelse(is.na(boxwhisk_data$value),
                                0,
                                boxwhisk_data$value)
  
  # Extracting covariates Names
  cov_name <- covs[2:length(covs)]
  
  # Export folder
  export_dir <- paste0("2.Chapter1/3.Output/covariate_analysis/south/used_avail_plots","/",name)
  
  for (v in 1:length(cov_name)) {
    
    # Box and Whisker Plot
    boxwhisk_plot <- ggplot(subset(boxwhisk_data, name == cov_name[[v]]),
                            aes(x = name, y = value, fill = as.factor(choice)))+
      geom_boxplot()+
      labs(fill = "Choice", x = "Covariate") +
      theme_bw()
    
    # Plot Export
    ggsave(filename = paste0(cov_name[[v]],".png"),
           plot = boxwhisk_plot,
           device = "png",
           path = export_dir,
           width = 6,
           height = 6,
           units = "in")
    
    # Iteration Monitor
    print(paste0(v," out of ", length(cov_name)))
  }
}

#      Model                                                                ####
#        [Model Settings]                                                   ####
# Model Settings
chains <- 3 
iter <- 4000
warmup <- iter*.25
thin <- 1
cores <- chains
refresh <- iter * 0.05

#        [Model]                                                            ####

foreach(i = 1:length(data_south)) %do% {
  
  # Separating specific dataframe
  df_raw <- data[[i]]
  name <- names_south[[i]]
  
  df <- df_raw
  
  for (j in (str_which(names(df),"geometry")+1):ncol(df)) {
    df[,j] <- scale(df[,j])
  }
  
  # Making the list of covariate names and eliminating undesired covariates
  covs <- names(df) %>% 
    .[(str_which(.,"geometry")+1):length(.)] %>% 
    str_subset("shrub",negate = T) %>% 
    str_subset("wetland",negate = T) %>% 
    str_subset("barren",negate = T) %>% 
    str_subset("water",negate = T)  %>%
    str_subset("TRI",negate = T) %>% 
    str_subset("developed",negate = T) %>%
    str_subset("evergreen",negate = T) %>%
    str_subset("mixed",negate = T) %>%
    c()
  
  # Building the formula
  formula <- as.formula(paste("choice ~ ", 
                              paste(covs, collapse= "+"),
                              "+ (1 + ",
                              paste(covs, collapse= "+"),
                              "| individual.local.identifier)"))
  
  # Model
  
  model <- brm(
    formula = formula,
    data = df,
    family = binomial(link = "logit"),
    prior = prior(normal(0, 1)),
    refresh = refresh,
    chains = chains, 
    iter = iter,
    warmup = warmup,
    thin = thin,
    cores = cores
  )
  
  saveRDS(object = model,
          file = paste0("2.Chapter1/3.Output/models_bayesian_randomintercept/south/bayesian_",name,".RDS"))
  
  return(print(paste0("South: ",i," out of ", length(data_south), " is completed")))
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
  
  covs <- names(df) %>% 
    .[(str_which(.,"geometry")+1):length(.)] %>% 
    str_subset("shrub",negate = T) %>% 
    str_subset("wetland",negate = T) %>% 
    str_subset("barren",negate = T) %>% 
    str_subset("water",negate = T)  %>%
    str_subset("TRI",negate = T) %>% 
    str_subset("developed",negate = T) %>% 
    str_subset("evergreen",negate = T) %>%
    str_subset("mixed",negate = T) %>%
    str_subset("grassland",negate = T) %>%
    str_subset("southeast_patchdist_deciduous_small",negate = T) %>%
    str_subset("southeast_dem",negate = T) %>%
    str_subset("southeast_patchdist_deciduous_forest",negate = T) %>%
    c()
  
  covlist <- foreach(v = 1:length(covs),.combine = c) %do% {str_which(names(df),pattern = covs[v])} %>% unique()
  
  cov_df <- df[covlist]
  
  cor_bargraph <-corr_cross(cov_df, rm.na = T, max_pvalue = 0.06, 
                            top = 40,
                            plot = T)
  
  cor_data <-corr_cross(cov_df, rm.na = T, max_pvalue = 0.06, 
                        top = 60,
                        plot = F)
  
  ggsave(filename = name,
         plot = cor_bargraph,
         device = "png",
         path = "2.Chapter1/3.Output/covariate_analysis/southeast",
         width = 14,
         height = 6,
         units = "in")
  
  write_csv(cor_data,
            paste0("2.Chapter1/3.Output/covariate_analysis/southeast/",str_remove(name,".png"),".csv"))
  
  print(i)
}

#        UsedAvail Plots                                                    ####

for (i in 1:length(data_southeast)) {
  
  # Iteration Monitor 1 
  print(paste0("Starting Dataset ",i, " out of ",length(data_southeast)))
  
  # Dataframe Extraction and Name
  df <- data[[i]]
  name <- list.files("1.DataManagement/ch1_data/southeast/model_data",
                     full.names = F,pattern = ".csv") %>% 
    .[[i]] %>% 
    str_remove(".csv") %>% 
    str_replace("final","_usedavail")
  
  # Making a List of Covariates
  covs <- names(df) %>% 
    .[(str_which(.,"geometry")+1):length(.)] %>% 
    str_subset("shrub",negate = T) %>% 
    str_subset("wetland",negate = T) %>% 
    str_subset("barren",negate = T) %>% 
    str_subset("water",negate = T)  %>%
    str_subset("TRI",negate = T) %>% 
    str_subset("developed",negate = T) %>% 
    str_subset("evergreen",negate = T) %>%
    str_subset("mixed",negate = T) %>%
    str_subset("grassland",negate = T) %>%
    str_subset("southeast_patchdist_deciduous_small",negate = T) %>%
    str_subset("southeast_dem",negate = T) %>%
    str_subset("southeast_patchdist_deciduous_forest",negate = T) %>%
    append("choice",after = 0) %>% 
    c()
  
  # Covariate List 
  covlist <- foreach(v = 1:length(covs),.combine = c) %do% 
    {str_which(names(df),pattern = covs[v])} %>% 
    unique()
  
  # Subsetting Covariates from dataframe
  cov_df <- df[covlist]
  
  # Preparing the data in box and whisker plot format
  boxwhisk_data <- pivot_longer(cov_df,
                                cols = 2:ncol(cov_df))
  
  # Replacing NA values with Zero
  boxwhisk_data$value <- ifelse(is.na(boxwhisk_data$value),
                                0,
                                boxwhisk_data$value)
  
  # Extracting covariates Names
  cov_name <- covs[2:length(covs)]
  
  # Export folder
  export_dir <- paste0("2.Chapter1/3.Output/covariate_analysis/southeast/used_avail_plots","/",name)
  
  for (v in 1:length(cov_name)) {
    
    # Box and Whisker Plot
    boxwhisk_plot <- ggplot(subset(boxwhisk_data, name == cov_name[[v]]),
                            aes(x = name, y = value, fill = as.factor(choice)))+
      geom_boxplot()+
      labs(fill = "Choice", x = "Covariate") +
      theme_bw()
    
    # Plot Export
    ggsave(filename = paste0(cov_name[[v]],".png"),
           plot = boxwhisk_plot,
           device = "png",
           path = export_dir,
           width = 6,
           height = 6,
           units = "in")
    
    # Iteration Monitor
    print(paste0(v," out of ", length(cov_name)))
  }
}

#      Model                                                                ####
#        [Model Settings]                                                   ####
# Model Settings
chains <- 3 
iter <- 4000
warmup <- iter*.25
thin <- 1
cores <- chains
refresh <- iter * 0.05

#        [Model]                                                            ####

foreach(i = 1:length(data_southeast)) %do% {
  
  # Separating specific dataframe
  df_raw <- data[[i]]
  name <- names_southeast[[i]]
  
  df <- df_raw
  
  for (j in (str_which(names(df),"geometry")+1):ncol(df)) {
    df[,j] <- scale(df[,j])
  }
  
  # Making the list of covariate names and eliminating undesired covariates
  covs <- names(df) %>% 
    .[(str_which(.,"geometry")+1):length(.)] %>% 
    str_subset("shrub",negate = T) %>% 
    str_subset("wetland",negate = T) %>% 
    str_subset("barren",negate = T) %>% 
    str_subset("water",negate = T)  %>%
    str_subset("TRI",negate = T) %>% 
    str_subset("developed",negate = T) %>% 
    str_subset("evergreen",negate = T) %>%
    str_subset("mixed",negate = T) %>%
    str_subset("grassland",negate = T) %>%
    str_subset("southeast_patchdist_deciduous_small",negate = T) %>%
    str_subset("southeast_dem",negate = T) %>%
    str_subset("southeast_patchdist_deciduous_forest",negate = T) %>%
    c()
  
  # Building the formula
  formula <- as.formula(paste("choice ~ ", 
                              paste(covs, collapse= "+"),
                              "+ (1 + ",
                              paste(covs, collapse= "+"),
                              "| individual.local.identifier)"))
  
  # Model
  
  model <- brm(
    formula = formula,
    data = df,
    family = binomial(link = "logit"),
    prior = prior(normal(0, 1)),
    refresh = refresh,
    chains = chains, 
    iter = iter,
    warmup = warmup,
    thin = thin,
    cores = cores
  )
  
  saveRDS(object = model,
          file = paste0("2.Chapter1/3.Output/models_bayesian_randomintercept/southeast/bayesian_",name,".RDS"))
  
  return(print(paste0("Southeast: ",i," out of ", length(data_southeast), " is completed")))
}

###############################################################################