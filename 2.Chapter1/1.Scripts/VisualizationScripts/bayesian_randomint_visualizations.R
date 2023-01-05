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

###############################################################################
#      Raster Map                                                           ####
#        Map Metadata and Cov list                                          #### 

#        [Study Area Subsets shapefiles]                                    ####
North_StudyArea <- st_intersects(Missouri_shp,deer_sf_north)
North_StudyArea <- Missouri_shp[which(lengths(North_StudyArea)!=0),]

# Subsetting Model
model <- readRDS(north_models[[i]])
data <- model$data
name <- north_names[[i]]
cov_list <- names(data)[-1] %>% 
  str_subset("individual.local.identifier",negate = T)

# Making a list of covariate rasters
north_rasters <- list.files("1.DataManagement/CovRasters_Landscape/north",
                                 full.names = T)  %>% 
  str_subset(paste0(cov_list, collapse = '|')) %>% 
  rast()
  
names(north_rasters)

model_coefs <- fixef(model)[,1]

predictive_raster_north <- exp(model_coefs[[1]] + 
                                 model_coefs[[2]]*north_rasters[[1]]+
                                 model_coefs[[3]]*north_rasters[[2]]+
                                 model_coefs[[4]]*north_rasters[[3]]+
                                 model_coefs[[5]]*north_rasters[[4]]+
                                 model_coefs[[6]]*north_rasters[[5]]+
                                 model_coefs[[7]]*north_rasters[[6]]+
                                 model_coefs[[8]]*north_rasters[[7]]+
                                 model_coefs[[9]]*north_rasters[[8]]+
                                 model_coefs[[10]]*north_rasters[[9]]+
                                 model_coefs[[11]]*north_rasters[[10]]+
                                 model_coefs[[12]]*north_rasters[[11]]+
                                 model_coefs[[13]]*north_rasters[[12]]+
                                 model_coefs[[14]]*north_rasters[[13]]+
                                 model_coefs[[15]]*north_rasters[[14]]+
                                 model_coefs[[16]]*north_rasters[[15]]) / 
  (1 + exp(model_coefs[[1]] + 
             model_coefs[[2]]*north_rasters[[1]]+
             model_coefs[[3]]*north_rasters[[2]]+
             model_coefs[[4]]*north_rasters[[3]]+
             model_coefs[[5]]*north_rasters[[4]]+
             model_coefs[[6]]*north_rasters[[5]]+
             model_coefs[[7]]*north_rasters[[6]]+
             model_coefs[[8]]*north_rasters[[7]]+
             model_coefs[[9]]*north_rasters[[8]]+
             model_coefs[[10]]*north_rasters[[9]]+
             model_coefs[[11]]*north_rasters[[10]]+
             model_coefs[[12]]*north_rasters[[11]]+
             model_coefs[[13]]*north_rasters[[12]]+
             model_coefs[[14]]*north_rasters[[13]]+
             model_coefs[[15]]*north_rasters[[14]]+
             model_coefs[[16]]*north_rasters[[15]]))


  # lapply(list.files("1.Data/CleanData",
  #                                       pattern = "North_proportion",
  #                                       full.names = T), raster) %>% 
  # stack() %>% 
  # dropLayer(i = 7)


