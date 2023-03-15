#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-03-15 

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

#        Southeast                                                          ####

# Southeast
southeast_models  <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/southeast",
                                full.names = T,
                                pattern = ".RDS")

southeast_names <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/southeast",
                              full.names = F,
                              pattern = ".RDS") %>% 
  str_remove("bayesian_") %>% 
  str_remove(".RDS")

###############################################################################
#   Raw Data Generation                                                     ####
#      Southeast                                                            ####

for (i in 1:length(southeast_models)) {
  
  # Subsetting Model
  model <- readRDS(southeast_models[[i]])
  data <- model$data
  name <- southeast_names[[i]]
  season <- str_remove(name,"southeastmodel_F_")
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
    
    write_csv(plot_data,
              paste0("2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/effect_plots_rawdata/",
                     cov_name,"_",season,"_effectdata.csv"))
  }}

###############################################################################
#   Plots                                                                   ####
#      Southeast                                                            ####

# Subsetting Model Info
model <- readRDS(southeast_models[[1]])
data <- model$data
cov_list <- names(data)[-1] %>% 
  str_subset("individual.local.identifier",negate = T)

cov_list_plotting <- names(data)[-1] %>% 
  str_subset("individual.local.identifier",negate = T) %>% 
  str_remove("southeast_")
  
# Start of the for loops
i <- 1 

for (i in 1:length(cov_list_plotting)) {
  
  # Directory of Raw Data
  dir_rawdata <- "2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/effect_plots_rawdata"
  
  # Patterns for Cov of interest
  cov_csv_patterns <- cov_list_plotting[[i]] %>% 
    paste(c("fall","winter","summer","spring"),sep = "_") %>% 
    paste("effectdata.csv",sep = "_") %>% 
    paste0(dir_rawdata,"/",.)
  
  # Filepaths
  cov_csv_fp <- list.files(dir_rawdata,
                           full.names = T) %>% 
    str_subset(paste(cov_csv_patterns, collapse = "|")) 
  
  # Empty List
  cov_csv_data <- list()
  
  # Adding Seasonality
  for (v in 1:length(cov_csv_fp)) {
    season_id <- str_extract(cov_csv_fp[[v]], paste(c("fall","winter","summer","spring"), collapse = "|"))
    
    csv <- cov_csv_fp[[v]] %>% 
      read_csv(show_col_types = F) %>% 
      mutate(season = season_id)
    
    cov_csv_data[[v]] <- csv
    
  }
  
  # Joining the data 
  cov_csv_data <- do.call(bind_rows,cov_csv_data)
  
  # Effect Plot
  plot <- ggplot(cov_csv_data, 
                 aes(x = .data[[names(cov_csv_data)[[str_which(names(cov_csv_data),"ymax")-1]]]], 
                     y = choice,
                     color = season))+ 
    geom_smooth()+
    geom_ribbon(aes(ymin=ymin, ymax=ymax), alpha=0.2)+
    theme_nice()+
    ylab("Probability of Use")+
    ylim(0, 1)+
    ggtitle(cov_list_plotting[[i]]) +
    labs(x = str_remove(cov_list_plotting[[i]],"southeast_")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Plot Export
  ggsave(filename = paste0("2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/effect_plots_seasonal",
                           "/",
                           cov_list_plotting[[i]],
                           ".png"),
         plot = plot,
         device = "png",
         width = 6,
         height = 4,
         units = "in")
}
###############################################################################