#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-12-15 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(posterior)
library(jtools)
library(stringr)
library(sf)
library(terra)
library(ggcorrplot)
library(foreach)
library(tidyverse)
library(tidyterra)
library(viridis) 

#      Functions                                                            ####

#      Data                                                                 ####
#        Models                                                             ####

# North
north_models <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/north",
                           full.names = T,
                           pattern = ".RDS")

north_names <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/north",
                          full.names = F,
                          pattern = ".RDS") %>% 
  str_remove("bayesian_") %>% 
  str_remove(".RDS")

# South
south_models  <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/south",
                            full.names = T,
                            pattern = ".RDS")

south_names <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/south",
                          full.names = F,
                          pattern = ".RDS") %>% 
  str_remove("bayesian_") %>% 
  str_remove(".RDS")

# Southeast
southeast_models  <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/southeast",
                                full.names = T,
                                pattern = ".RDS")

southeast_names <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/southeast",
                              full.names = F,
                              pattern = ".RDS") %>% 
  str_remove("bayesian_") %>% 
  str_remove(".RDS")

#        Missouri Shapefiles                                                ####

# State
Missouri_shp <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp") 

# North
studyarea_north <- st_read("1.DataManagement\\shapefiles\\north_studyarea.shp")

# South
studyarea_south <- st_read("1.DataManagement\\shapefiles\\south_studyarea.shp")

# Southeast
studyarea_southeast <- st_read("1.DataManagement\\shapefiles\\southeast_studyarea.shp")

###############################################################################
#   North                                                                   ####
#      Beta Estimates                                                       ####

for (i in 1:length(north_models)) {
  
  model <- readRDS(north_models[[i]])
  name <- north_names[[i]]
  
  viz <- plot_summs(model, scale = TRUE, robust = TRUE) +
    xlim(-1.3,0.4)
  
  ggsave(filename = paste0(name,".png"),
         plot = viz,
         device = "png",
         path = "2.Chapter1/3.Output/visualizations_bayesian_randomint/north/beta_estimates",
         width = 10,
         height = 8,
         units = "in")
  
  print(i)
}

#      Grouped Beta Estimates                                               ####

# Creating Beta Estimate Table
model_betas <- foreach(i = 1:length(north_models), .combine = "bind_rows") %do% {
  
  # Subsetting Model and Model Metadata
  model <- readRDS(north_models[[i]])
  name <- north_names[[i]]
  sex_val <- str_extract(name, "_([^_]+)_") %>% str_remove_all("_")
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Extracting Model Covariates
  model_coefs <- fixef(model) %>% as.data.frame() %>% 
    rownames_to_column("Covariate") %>% 
    mutate(model = name,.before = 1) %>% 
    mutate(sex = sex_val,.after = model) %>% 
    mutate(season = str_remove(.$model,pattern = paste0("northmodel_",sex_val,"_")),.after = sex) %>% 
    mutate(season = factor(season,levels = c("fall","winter","spring","summer"))) %>% 
    rename(min = Q2.5) %>% 
    rename(max = Q97.5) %>% 
    filter(Covariate != "Intercept")
  
  model_coefs$Covariate <- str_remove(model_coefs$Covariate,pattern = "north_")
  
  return(model_coefs)
}

# Plotting
male_plot <- ggplot(data = subset(model_betas,sex == "M"),
                    aes(x = Covariate, y = Estimate, ymin = min, ymax = max, color = season))+ 
  geom_hline(yintercept = 0,linewidth = 1) + 
  geom_pointrange(position=position_dodge(width = 0.9),size = 0.75)+
  coord_flip() + 
  theme_nice()+
  ggtitle("North Male")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-1.5,1.5)

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/north/beta_estimates_grouped/NorthMale.png",
       plot = male_plot,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

female_plot <- ggplot(data = subset(model_betas,sex == "F"),
                      aes(x = Covariate, y = Estimate, ymin = min, ymax = max, color = season))+ 
  geom_hline(yintercept = 0,linewidth = 1) + 
  geom_pointrange(position=position_dodge(width = 0.9),size = 0.75)+
  coord_flip() +
  theme_nice()+
  ggtitle("North Female")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-1.5,1.5)

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/north/beta_estimates_grouped/NorthFemale.png",
       plot = female_plot,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

#      Effect Plots Scaled                                                  ####

# Plot Export directory
export_dir <- "2.Chapter1//3.Output//visualizations_bayesian_randomint//north//effect_plots_scaled"

for (i in 1:length(north_models)) {
  
  # Subsetting Model
  model <- readRDS(north_models[[i]])
  data <- model$data
  name <- north_names[[i]]
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Covariate Loop
  for (v in 1:length(cov_list)) {
    
    # Seperating Covariate for Viz
    cov <- cov_list %>% 
      .[v]
    
    cov_name <- cov_list %>% 
      .[v] %>% str_remove("north_")
    
    # Effect Plot Data
    plot_data <- effect_plot(model, 
                             pred = !!cov, 
                             interval = TRUE, 
                             plot.points = F,
                             x.label = cov,
                             y.label = "Probability of Use") %>% 
      .$data
    
    # Effect Plot
    plot <- ggplot(plot_data, aes_string(x= cov, y = "choice"))+
      geom_smooth(color = "black")+
      geom_ribbon(aes(ymin=ymin, ymax=ymax), alpha=0.2)+
      theme_nice()+
      ylab("Probability of Use")+
      ylim(0, 1)+
      ggtitle(name)+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Plot Export
    ggsave(filename = paste0(export_dir,
                             "/",
                             name,
                             "_",
                             cov,
                             ".png"),
           plot = plot,
           device = "png",
           width = 6,
           height = 4,
           units = "in")
    
    # Iteration Tracker
    print(paste0("Model ", i, ": Covariate ", v, " Completed"))
  }
}

#      Predictive Raster Calculation                                        ####

# Subsetting Model and Model Metadata
model <- readRDS(north_models[[1]])
data <- model$data
cov_list <- names(data)[-1] %>% 
  str_subset("individual.local.identifier",negate = T)

# Preparing Rasters 
raster_list <- list.files("1.DataManagement/CovRasters_Landscape/north",
                          full.names = T)  %>% 
  str_subset(paste0(cov_list, collapse = '|')) %>% 
  rast() %>% 
  crop(studyarea_north) %>% 
  mask(studyarea_north) %>% 
  scale()

for (i in 1:length(north_models)) {
  
  # Subsetting Model and Model Metadata
  model <- readRDS(north_models[[i]])
  name <- north_names[[i]]
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Extracting Model Covariates
  model_coefs <- fixef(model)[,1]
  
  # Making Predictive Raster
  predictive_raster <- exp(model_coefs[[1]] + 
                             model_coefs[[2]]*raster_list[[1]]+
                             model_coefs[[3]]*raster_list[[2]]+
                             model_coefs[[4]]*raster_list[[3]]+
                             model_coefs[[5]]*raster_list[[4]]+
                             model_coefs[[6]]*raster_list[[5]]+
                             model_coefs[[7]]*raster_list[[6]]+
                             model_coefs[[8]]*raster_list[[7]]+
                             model_coefs[[9]]*raster_list[[8]]+
                             model_coefs[[10]]*raster_list[[9]]+
                             model_coefs[[11]]*raster_list[[10]]+
                             model_coefs[[12]]*raster_list[[11]]+
                             model_coefs[[13]]*raster_list[[12]]+
                             model_coefs[[14]]*raster_list[[13]]+
                             model_coefs[[15]]*raster_list[[14]]+
                             model_coefs[[16]]*raster_list[[15]]) /
    (1 + exp(model_coefs[[1]] + 
               model_coefs[[2]]*raster_list[[1]]+
               model_coefs[[3]]*raster_list[[2]]+
               model_coefs[[4]]*raster_list[[3]]+
               model_coefs[[5]]*raster_list[[4]]+
               model_coefs[[6]]*raster_list[[5]]+
               model_coefs[[7]]*raster_list[[6]]+
               model_coefs[[8]]*raster_list[[7]]+
               model_coefs[[9]]*raster_list[[8]]+
               model_coefs[[10]]*raster_list[[9]]+
               model_coefs[[11]]*raster_list[[10]]+
               model_coefs[[12]]*raster_list[[11]]+
               model_coefs[[13]]*raster_list[[12]]+
               model_coefs[[14]]*raster_list[[13]]+
               model_coefs[[15]]*raster_list[[14]]+
               model_coefs[[16]]*raster_list[[15]]))
  
  names(predictive_raster) <- paste0(name,"_predictive_raster")
  
  # Exporting Raster
  writeRaster(predictive_raster,
              filename = paste0("2.Chapter1/3.Output/visualizations_bayesian_randomint/north/predictive_maps/",
                                name,
                                ".tif"),
              overwrite = T)
  
  # Iteration Tracker
  print(paste0(i," out of ", length(north_models), " completed"))
}

#      Predictive Raster Correlation Plot                                   ####

# List of Sexes
sexes <- c("M","F")

for (i in 1:length(sexes)) {
  
  # Loading Rasters
  pred_rasters <- list.files("2.Chapter1/3.Output/visualizations_bayesian_randomint/north/predictive_maps",
                             full.names = T) %>% str_subset(sexes[i]) %>% rast()
  
  # Raster Names
  rast_names <- list.files("2.Chapter1/3.Output/visualizations_bayesian_randomint/north/predictive_maps",
                           full.names = F) %>% str_subset(sexes[i]) %>% str_remove(".tif")
  
  names(pred_rasters) <- rast_names #This is temporary. Fixed it in prior steps
  
  # Pearson Correlation
  pearson_cor <- layerCor(x = pred_rasters, 
                          fun = "pearson",
                          na.rm = T)
  
  # Correlation Plot
  corplot <- ggcorrplot(pearson_cor$pearson, 
                        hc.order = TRUE,
                        lab = TRUE)
  
  # Exporting Plot
  ggsave(filename = paste0("2.Chapter1/3.Output/visualizations_bayesian_randomint/north/predictive_maps_corplots/north_",sexes[i],".png"),
         plot = corplot,
         device = "png",
         width = 8,
         height = 6,
         units = "in")
  
}
#      Heatmaps                                                             ####

# Creating Beta Estimate Table
model_betas <- foreach(i = 1:length(north_models), .combine = "bind_rows") %do% {
  
  # Subsetting Model and Model Metadata
  model <- readRDS(north_models[[i]])
  name <- north_names[[i]]
  sex_val <- str_extract(name, "_([^_]+)_") %>% str_remove_all("_")
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Extracting Model Covariates
  model_coefs <- fixef(model) %>% as.data.frame() %>% 
    rownames_to_column("Covariate") %>% 
    mutate(model = name,.before = 1) %>% 
    mutate(sex = sex_val,.after = model) %>% 
    mutate(season = str_remove(.$model,pattern = paste0("northmodel_",sex_val,"_")),.after = sex) %>% 
    mutate(season = factor(season,levels = c("fall","winter","spring","summer"))) %>% 
    rename(min = Q2.5) %>% 
    rename(max = Q97.5) %>% 
    filter(Covariate != "Intercept")
  
  model_coefs$Covariate <- str_remove(model_coefs$Covariate,pattern = "north_")
  
  return(model_coefs)
}

# Beta Estimate Heatmap
df <- filter(model_betas, sex == 'M') %>% 
  select(Covariate,season,Estimate)

hm <- ggplot(df, aes(season,Covariate)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = Estimate))+
  theme_nice()+
  geom_text(aes(label = round(Estimate, 2)), size=3)

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/north/heatmaps/NorthMale_BetaEstimates.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

df <- filter(model_betas, sex == 'F') %>% 
  select(Covariate,season,Estimate)

hm <- ggplot(df, aes(season,Covariate)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = Estimate))+
  theme_nice()+
  geom_text(aes(label = round(Estimate, 2)), size=3)

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/north/heatmaps/NorthFemale_BetaEstimates.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")



# Beta Directionality
df <- filter(model_betas, sex == 'M') %>% 
  select(Covariate,season,Estimate) %>% 
  mutate(Estimate = ifelse(.$Estimate > 0, 1, -1))

hm <- ggplot(df, aes(season,Covariate)) +                           
  geom_tile(aes(fill = Estimate))+
  theme_nice()

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/north/heatmaps/NorthMale_BetaDirectionality.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

df <- filter(model_betas, sex == 'F') %>% 
  select(Covariate,season,Estimate) %>% 
  mutate(Estimate = ifelse(.$Estimate > 0, 1, -1))

hm <- ggplot(df, aes(season,Covariate)) +                           
  geom_tile(aes(fill = Estimate))+
  theme_nice()

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/north/heatmaps/NorthFemale_BetaDirectionality.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

# Overlapping Zero 
df <- filter(model_betas, sex == 'M') %>% 
  select(Covariate,season,min,max) %>% 
  mutate(value = ifelse(0 >= min & 0 <= max, 1, -1))

hm <- ggplot(df, aes(season,Covariate)) +                           
  geom_tile(aes(fill = value))+
  theme_nice()

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/north/heatmaps/NorthMale_BetaOverlapZero.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

df <- filter(model_betas, sex == 'F') %>% 
  select(Covariate,season,min,max) %>% 
  mutate(value = ifelse(0 >= min & 0 <= max, 1, -1))

hm <- ggplot(df, aes(season,Covariate)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value))+
  theme_nice()

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/north/heatmaps/NorthFemale_BetaOverlapZero.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")
###############################################################################
#   South                                                                   ####
#      Beta Estimates                                                       ####

for (i in 1:length(south_models)) {
  
  model <- readRDS(south_models[[i]])
  name <- south_names[[i]]
  
  viz <- plot_summs(model, scale = TRUE, robust = TRUE) + xlim(-1.2,1.2)
  
  ggsave(filename = paste0(name,".png"),
         plot = viz,
         device = "png",
         path = "2.Chapter1/3.Output/visualizations_bayesian_randomint/south/beta_estimates",
         width = 10,
         height = 8,
         units = "in")
  
  print(i)
}


#      Grouped Beta Estimates                                               ####

# Creating Beta Estimate Table
model_betas <- foreach(i = 1:length(south_models), .combine = "bind_rows") %do% {
  
  # Subsetting Model and Model Metadata
  model <- readRDS(south_models[[i]])
  name <- south_names[[i]]
  sex_val <- str_extract(name, "_([^_]+)_") %>% str_remove_all("_")
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Extracting Model Covariates
  model_coefs <- fixef(model) %>% as.data.frame() %>% 
    rownames_to_column("Covariate") %>% 
    mutate(model = name,.before = 1) %>% 
    mutate(sex = sex_val,.after = model) %>% 
    mutate(season = str_remove(.$model,pattern = paste0("southmodel_",sex_val,"_")),.after = sex) %>% 
    mutate(season = factor(season,levels = c("fall","winter","spring","summer"))) %>% 
    rename(min = Q2.5) %>% 
    rename(max = Q97.5) %>% 
    filter(Covariate != "Intercept")
  
  model_coefs$Covariate <- str_remove(model_coefs$Covariate,pattern = "south_")
  
  return(model_coefs)
}

# Plotting
male_plot <- ggplot(data = subset(model_betas,sex == "M"),
                    aes(x = Covariate, y = Estimate, ymin = min, ymax = max, color = season))+ 
  geom_hline(yintercept = 0,linewidth = 1) + 
  geom_pointrange(position=position_dodge(width = 0.9),size = 0.75)+
  coord_flip() + 
  theme_nice()+
  ggtitle("South Male")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-1.5,1.5)

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/south/beta_estimates_grouped/SouthMale.png",
       plot = male_plot,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

female_plot <- ggplot(data = subset(model_betas,sex == "F"),
                      aes(x = Covariate, y = Estimate, ymin = min, ymax = max, color = season))+ 
  geom_hline(yintercept = 0,linewidth = 1) + 
  geom_pointrange(position=position_dodge(width = 0.9),size = 0.75)+
  coord_flip() + 
  theme_nice()+
  ggtitle("South Female")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-1.5,1.5)

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/south/beta_estimates_grouped/SouthFemale.png",
       plot = female_plot,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

#      Effect Plots Scaled                                                  ####

# Plot Export directory
export_dir <- "2.Chapter1//3.Output//visualizations_bayesian_randomint//south//effect_plots_scaled"

for (i in 1:length(south_models)) {
  
  # Subsetting Model
  model <- readRDS(south_models[[i]])
  data <- model$data
  name <- south_names[[i]]
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Covariate Loop
  for (v in 1:length(cov_list)) {
    
    # Seperating Covariate for Viz
    cov <- cov_list %>% 
      .[v]
    
    cov_name <- cov_list %>% 
      .[v] %>% str_remove("south_")
    
    # Effect Plot Data
    plot_data <- effect_plot(model, 
                             pred = !!cov, 
                             interval = TRUE, 
                             plot.points = F,
                             x.label = cov,
                             y.label = "Probability of Use") %>% 
      .$data
    
    # Effect Plot
    plot <- ggplot(plot_data, aes_string(x= cov, y = "choice"))+
      geom_smooth(color = "black")+
      geom_ribbon(aes(ymin=ymin, ymax=ymax), alpha=0.2)+
      theme_nice()+
      ylab("Probability of Use")+
      ylim(0, 1)+
      ggtitle(name)+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Plot Export
    ggsave(filename = paste0(export_dir,
                             "/",
                             name,
                             "_",
                             cov,
                             ".png"),
           plot = plot,
           device = "png",
           width = 6,
           height = 4,
           units = "in")
    
    # Iteration Tracker
    print(paste0("Model ", i, ": Covariate ", v, " Completed"))
  }
}


#      Predictive Raster Calculation                                        ####

# Subsetting Model and Model Metadata
model <- readRDS(south_models[[1]])
data <- model$data
cov_list <- names(data)[-1] %>% 
  str_subset("individual.local.identifier",negate = T)

# Preparing Rasters 
raster_list <- list.files("1.DataManagement/CovRasters_Landscape/south",
                          full.names = T)  %>% 
  str_subset(paste0(cov_list, collapse = '|')) %>% 
  rast() %>% 
  crop(studyarea_south) %>% 
  mask(studyarea_south) %>% 
  scale()

for (i in 1:length(south_models)) {
  
  # Subsetting Model and Model Metadata
  model <- readRDS(south_models[[i]])
  name <- south_names[[i]]
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Extracting Model Covariates
  model_coefs <- fixef(model)[,1]
  
  # Making Predictive Raster
  predictive_raster <- exp(model_coefs[[1]] + 
                             model_coefs[[2]]*raster_list[[1]]+
                             model_coefs[[3]]*raster_list[[2]]+
                             model_coefs[[4]]*raster_list[[3]]+
                             model_coefs[[5]]*raster_list[[4]]+
                             model_coefs[[6]]*raster_list[[5]]+
                             model_coefs[[7]]*raster_list[[6]]+
                             model_coefs[[8]]*raster_list[[7]]+
                             model_coefs[[9]]*raster_list[[8]]+
                             model_coefs[[10]]*raster_list[[9]]+
                             model_coefs[[11]]*raster_list[[10]]+
                             model_coefs[[12]]*raster_list[[11]]+
                             model_coefs[[13]]*raster_list[[12]]+
                             model_coefs[[14]]*raster_list[[13]]+
                             model_coefs[[15]]*raster_list[[14]]+
                             model_coefs[[16]]*raster_list[[15]]) /
    (1 + exp(model_coefs[[1]] + 
               model_coefs[[2]]*raster_list[[1]]+
               model_coefs[[3]]*raster_list[[2]]+
               model_coefs[[4]]*raster_list[[3]]+
               model_coefs[[5]]*raster_list[[4]]+
               model_coefs[[6]]*raster_list[[5]]+
               model_coefs[[7]]*raster_list[[6]]+
               model_coefs[[8]]*raster_list[[7]]+
               model_coefs[[9]]*raster_list[[8]]+
               model_coefs[[10]]*raster_list[[9]]+
               model_coefs[[11]]*raster_list[[10]]+
               model_coefs[[12]]*raster_list[[11]]+
               model_coefs[[13]]*raster_list[[12]]+
               model_coefs[[14]]*raster_list[[13]]+
               model_coefs[[15]]*raster_list[[14]]+
               model_coefs[[16]]*raster_list[[15]]))
  
  names(predictive_raster) <- paste0(name,"_predictive_raster")
  
  # Exporting Raster
  writeRaster(predictive_raster,
              filename = paste0("2.Chapter1/3.Output/visualizations_bayesian_randomint/south/predictive_maps/",
                                name,
                                ".tif"),
              overwrite = T)
  
  # Iteration Tracker
  print(paste0(i," out of ", length(south_models), " completed"))
}

#      Predictive Raster Correlation Plot                                   ####

# List of Sexes
sexes <- c("M","F")

for (i in 1:length(sexes)) {
  
  # Loading Rasters
  pred_rasters <- list.files("2.Chapter1/3.Output/visualizations_bayesian_randomint/south/predictive_maps",
                             full.names = T) %>% str_subset(sexes[i]) %>% rast()
  
  # Raster Names
  rast_names <- list.files("2.Chapter1/3.Output/visualizations_bayesian_randomint/south/predictive_maps",
                           full.names = F) %>% str_subset(sexes[i]) %>% str_remove(".tif")
  
  names(pred_rasters) <- rast_names #This is temporary. Fixed it in prior steps
  
  # Pearson Correlation
  pearson_cor <- layerCor(x = pred_rasters, 
                          fun = "pearson",
                          na.rm = T)
  
  # Correlation Plot
  corplot <- ggcorrplot(pearson_cor$pearson, 
                        hc.order = TRUE,
                        lab = TRUE)
  
  # Exporting Plot
  ggsave(filename = paste0("2.Chapter1/3.Output/visualizations_bayesian_randomint/south/predictive_maps_corplots/south_",sexes[i],".png"),
         plot = corplot,
         device = "png",
         width = 8,
         height = 6,
         units = "in")
  
}

#      Heatmaps                                                             ####

# Creating Beta Estimate Table
model_betas <- foreach(i = 1:length(south_models), .combine = "bind_rows") %do% {
  
  # Subsetting Model and Model Metadata
  model <- readRDS(south_models[[i]])
  name <- south_names[[i]]
  sex_val <- str_extract(name, "_([^_]+)_") %>% str_remove_all("_")
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Extracting Model Covariates
  model_coefs <- fixef(model) %>% as.data.frame() %>% 
    rownames_to_column("Covariate") %>% 
    mutate(model = name,.before = 1) %>% 
    mutate(sex = sex_val,.after = model) %>% 
    mutate(season = str_remove(.$model,pattern = paste0("southmodel_",sex_val,"_")),.after = sex) %>% 
    mutate(season = factor(season,levels = c("fall","winter","spring","summer"))) %>% 
    rename(min = Q2.5) %>% 
    rename(max = Q97.5) %>% 
    filter(Covariate != "Intercept")
  
  model_coefs$Covariate <- str_remove(model_coefs$Covariate,pattern = "south_")
  
  return(model_coefs)
}

# Beta Estimate Heatmap
df <- filter(model_betas, sex == 'M') %>% 
  select(Covariate,season,Estimate)

hm <- ggplot(df, aes(season,Covariate)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = Estimate))+
  theme_nice()+
  geom_text(aes(label = round(Estimate, 2)), size=3)

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/south/heatmaps/SouthMale_BetaEstimates.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

df <- filter(model_betas, sex == 'F') %>% 
  select(Covariate,season,Estimate)

hm <- ggplot(df, aes(season,Covariate)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = Estimate))+
  theme_nice()+
  geom_text(aes(label = round(Estimate, 2)), size=3)

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/south/heatmaps/SouthFemale_BetaEstimates.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")



# Beta Directionality
df <- filter(model_betas, sex == 'M') %>% 
  select(Covariate,season,Estimate) %>% 
  mutate(Estimate = ifelse(.$Estimate > 0, 1, -1))

hm <- ggplot(df, aes(season,Covariate)) +                           
  geom_tile(aes(fill = Estimate))+
  theme_nice()

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/south/heatmaps/SouthMale_BetaDirectionality.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

df <- filter(model_betas, sex == 'F') %>% 
  select(Covariate,season,Estimate) %>% 
  mutate(Estimate = ifelse(.$Estimate > 0, 1, -1))

hm <- ggplot(df, aes(season,Covariate)) +                           
  geom_tile(aes(fill = Estimate))+
  theme_nice()

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/south/heatmaps/SouthFemale_BetaDirectionality.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

# Overlapping Zero 
df <- filter(model_betas, sex == 'M') %>% 
  select(Covariate,season,min,max) %>% 
  mutate(value = ifelse(0 >= min & 0 <= max, 1, -1))

hm <- ggplot(df, aes(season,Covariate)) +                           
  geom_tile(aes(fill = value))+
  theme_nice()

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/south/heatmaps/SouthMale_BetaOverlapZero.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

df <- filter(model_betas, sex == 'F') %>% 
  select(Covariate,season,min,max) %>% 
  mutate(value = ifelse(0 >= min & 0 <= max, 1, -1))

hm <- ggplot(df, aes(season,Covariate)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value))+
  theme_nice()

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/south/heatmaps/SouthFemale_BetaOverlapZero.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

###############################################################################
#   Southeast                                                               ####
#      Beta Estimates                                                       ####

for (i in 1:length(southeast_models)) {
  
  model <- readRDS(southeast_models[[i]])
  name <- southeast_names[[i]]
  
  viz <- plot_summs(model, scale = TRUE, robust = TRUE) +
    xlim(-1.3,0.4)
  
  ggsave(filename = paste0(name,".png"),
         plot = viz,
         device = "png",
         path = "2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/beta_estimates",
         width = 10,
         height = 8,
         units = "in")
  
  print(i)
}

###############################################################################6
#      Grouped Beta Estimates                                               ####

# Creating Beta Estimate Table
model_betas <- foreach(i = 1:length(southeast_models), .combine = "bind_rows") %do% {
  
  # Subsetting Model and Model Metadata
  model <- readRDS(southeast_models[[i]])
  name <- southeast_names[[i]]
  sex_val <- str_extract(name, "_([^_]+)_") %>% str_remove_all("_")
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Extracting Model Covariates
  model_coefs <- fixef(model) %>% as.data.frame() %>% 
    rownames_to_column("Covariate") %>% 
    mutate(model = name,.before = 1) %>% 
    mutate(sex = sex_val,.after = model) %>% 
    mutate(season = str_remove(.$model,pattern = paste0("southeastmodel_",sex_val,"_")),.after = sex) %>% 
    mutate(season = factor(season,levels = c("fall","winter","spring","summer"))) %>% 
    rename(min = Q2.5) %>% 
    rename(max = Q97.5) %>% 
    filter(Covariate != "Intercept")
  
  model_coefs$Covariate <- str_remove(model_coefs$Covariate,pattern = "southeast_")
  
  return(model_coefs)
}

# Plotting
female_plot <- ggplot(data = subset(model_betas,sex == "F"),
                      aes(x = Covariate, y = Estimate, ymin = min, ymax = max, color = season))+ 
  geom_hline(yintercept = 0,linewidth = 1) + 
  geom_pointrange(position=position_dodge(width = 0.9),size = 0.75)+
  coord_flip() + 
  theme_nice()+
  ggtitle("Southeast Female")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-1.5,1.5)

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/beta_estimates_grouped/SoutheastFemale.png",
       plot = female_plot,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

#      Effect Plots Scaled                                                  ####

# Plot Export directory
export_dir <- "2.Chapter1//3.Output//visualizations_bayesian_randomint//southeast//effect_plots_scaled"

for (i in 1:length(southeast_models)) {
  
  # Subsetting Model
  model <- readRDS(southeast_models[[i]])
  data <- model$data
  name <- southeast_names[[i]]
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Covariate Loop
  for (v in 1:length(cov_list)) {
    
    # Seperating Covariate for Viz
    cov <- cov_list %>% 
      .[v]
    
    cov_name <- cov_list %>% 
      .[v] %>% str_remove("southeast_")
    
    # Effect Plot Data
    plot_data <- effect_plot(model, 
                             pred = !!cov, 
                             interval = TRUE, 
                             plot.points = F,
                             x.label = cov,
                             y.label = "Probability of Use") %>% 
      .$data
    
    # Effect Plot
    plot <- ggplot(plot_data, aes_string(x= cov, y = "choice"))+
      geom_smooth(color = "black")+
      geom_ribbon(aes(ymin=ymin, ymax=ymax), alpha=0.2)+
      theme_nice()+
      ylab("Probability of Use")+
      ylim(0, 1)+
      ggtitle(name)+
      theme(plot.title = element_text(hjust = 0.5))
    
    # Plot Export
    ggsave(filename = paste0(export_dir,
                             "/",
                             name,
                             "_",
                             cov,
                             ".png"),
           plot = plot,
           device = "png",
           width = 6,
           height = 4,
           units = "in")
    
    # Iteration Tracker
    print(paste0("Model ", i, ": Covariate ", v, " Completed"))
  }
}

#      Predictive Raster Calculation                                        ####
 
# Subsetting Model and Model Metadata
model <- readRDS(southeast_models[[1]])
data <- model$data
cov_list <- names(data)[-1] %>% 
  str_subset("individual.local.identifier",negate = T)

# Preparing Rasters 
raster_list <- list.files("1.DataManagement/CovRasters_Landscape/southeast",
                          full.names = T)  %>% 
  str_subset(paste0(cov_list, collapse = '|')) %>% 
  rast() %>% 
  crop(studyarea_southeast) %>% 
  mask(studyarea_southeast) %>% 
  scale()

for (i in 1:length(southeast_models)) {
  
  # Subsetting Model and Model Metadata
  model <- readRDS(southeast_models[[i]])
  name <- southeast_names[[i]]
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Extracting Model Covariates
  model_coefs <- fixef(model)[,1]
  
  # Making Predictive Raster
  predictive_raster <- exp(model_coefs[[1]] + 
                             model_coefs[[2]]*raster_list[[1]]+
                             model_coefs[[3]]*raster_list[[2]]+
                             model_coefs[[4]]*raster_list[[3]]+
                             model_coefs[[5]]*raster_list[[4]]+
                             model_coefs[[6]]*raster_list[[5]]+
                             model_coefs[[7]]*raster_list[[6]]+
                             model_coefs[[8]]*raster_list[[7]]+
                             model_coefs[[9]]*raster_list[[8]]) /
    (1 + exp(model_coefs[[1]] + 
               model_coefs[[2]]*raster_list[[1]]+
               model_coefs[[3]]*raster_list[[2]]+
               model_coefs[[4]]*raster_list[[3]]+
               model_coefs[[5]]*raster_list[[4]]+
               model_coefs[[6]]*raster_list[[5]]+
               model_coefs[[7]]*raster_list[[6]]+
               model_coefs[[8]]*raster_list[[7]]+
               model_coefs[[9]]*raster_list[[8]]))
  
  names(predictive_raster) <- paste0(name,"_predictive_raster")
  
  # Exporting Raster
  writeRaster(predictive_raster,
              filename = paste0("2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/predictive_maps/",
                                name,
                                ".tif"),
              overwrite = T)
  
  # Iteration Tracker
  print(paste0(i," out of ", length(southeast_models), " completed"))
}

#      Predictive Raster Correlation Plot                                   ####

# List of Sexes
sexes <- c("F")

for (i in 1:length(sexes)) {
  
  # Loading Rasters
  pred_rasters <- list.files("2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/predictive_maps",
                             full.names = T) %>% str_subset(sexes[i]) %>% rast()
  
  # Raster Names
  rast_names <- list.files("2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/predictive_maps",
                           full.names = F) %>% str_subset(sexes[i]) %>% str_remove(".tif")
  
  names(pred_rasters) <- rast_names #This is temporary. Fixed it in prior steps
  
  # Pearson Correlation
  pearson_cor <- layerCor(x = pred_rasters, 
                          fun = "pearson",
                          na.rm = T)
  
  # Correlation Plot
  corplot <- ggcorrplot(pearson_cor$pearson, 
                        hc.order = TRUE,
                        lab = TRUE)
  
  # Exporting Plot
  ggsave(filename = paste0("2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/predictive_maps_corplots/southeast_",sexes[i],".png"),
         plot = corplot,
         device = "png",
         width = 8,
         height = 6,
         units = "in")
  
}

#      Heatmaps                                                             ####

# Creating Beta Estimate Table
model_betas <- foreach(i = 1:length(southeast_models), .combine = "bind_rows") %do% {
  
  # Subsetting Model and Model Metadata
  model <- readRDS(southeast_models[[i]])
  name <- southeast_names[[i]]
  sex_val <- str_extract(name, "_([^_]+)_") %>% str_remove_all("_")
  cov_list <- names(data)[-1] %>% 
    str_subset("individual.local.identifier",negate = T)
  
  # Extracting Model Covariates
  model_coefs <- fixef(model) %>% as.data.frame() %>% 
    rownames_to_column("Covariate") %>% 
    mutate(model = name,.before = 1) %>% 
    mutate(sex = sex_val,.after = model) %>% 
    mutate(season = str_remove(.$model,pattern = paste0("southeastmodel_",sex_val,"_")),.after = sex) %>% 
    mutate(season = factor(season,levels = c("fall","winter","spring","summer"))) %>% 
    rename(min = Q2.5) %>% 
    rename(max = Q97.5) %>% 
    filter(Covariate != "Intercept")
  
  model_coefs$Covariate <- str_remove(model_coefs$Covariate,pattern = "southeast_")
  
  return(model_coefs)
}

# Beta Estimate Heatmap
df <- filter(model_betas, sex == 'F') %>% 
  select(Covariate,season,Estimate)

hm <- ggplot(df, aes(season,Covariate)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = Estimate))+
  theme_nice()+
  geom_text(aes(label = round(Estimate, 2)), size=3)

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/heatmaps/SoutheastFemale_BetaEstimates.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")



# Beta Directionality
df <- filter(model_betas, sex == 'F') %>% 
  select(Covariate,season,Estimate) %>% 
  mutate(Estimate = ifelse(.$Estimate > 0, 1, -1))

hm <- ggplot(df, aes(season,Covariate)) +                           
  geom_tile(aes(fill = Estimate))+
  theme_nice()

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/heatmaps/SoutheastFemale_BetaDirectionality.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")

# Overlapping Zero 
df <- filter(model_betas, sex == 'F') %>% 
  select(Covariate,season,min,max) %>% 
  mutate(value = ifelse(0 >= min & 0 <= max, 1, -1))

hm <- ggplot(df, aes(season,Covariate)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value))+
  theme_nice()

ggsave(filename = "2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/heatmaps/SoutheastFemale_BetaOverlapZero.png",
       plot = hm,
       device = "png",
       width = 10,
       height = 12,
       units = "in")


#      Predictive Raster Map                                                ####

# Importing Raster
rasters <- list.files("2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/predictive_maps",
                      pattern = "tif",
                      full.names = T) %>% 
  rast() %>% 
  project("EPSG:4326")

# Extracting and Replacing Raster names
raster_names <- list.files("2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/predictive_maps",
                           pattern = "tif",
                           full.names = F) %>% 
  str_remove("southeastmodel_F_") %>% 
  str_remove(".tif")

names(rasters) <- raster_names

# Making facetted plot
maps <- ggplot() +
  geom_spatraster(data = rasters) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_viridis(option = "D",
                     name = "Relative Pr(use)",
                     na.value="transparent")+
  theme(legend.position = "bottom")

# Exporting
ggsave(filename = "predictive_maps.png",
       plot = maps,
       device = "png",
       path = "2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/predictive_maps",
       width = 8,
       height = 10,
       units = "in",
       dpi = 300)

###############################################################################