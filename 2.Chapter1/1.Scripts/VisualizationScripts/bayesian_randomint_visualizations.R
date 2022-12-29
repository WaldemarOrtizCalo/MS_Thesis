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

#      Functions                                                            ####

#      Data                                                                 ####

north_models <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/north",
                           full.names = T,
                           pattern = ".RDS")

north_names <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/north",
                          full.names = F,
                          pattern = ".RDS") %>% 
  str_remove("bayesian_") %>% 
  str_remove(".RDS")

south_models  <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/south",
                            full.names = T,
                            pattern = ".RDS")

south_names <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/south",
                          full.names = F,
                          pattern = ".RDS") %>% 
  str_remove("bayesian_") %>% 
  str_remove(".RDS")

southeast_models  <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/southeast",
                                full.names = T,
                                pattern = ".RDS")

southeast_names <- list.files("2.Chapter1/3.Output/models_bayesian_randomintercept/southeast",
                              full.names = F,
                              pattern = ".RDS") %>% 
  str_remove("bayesian_") %>% 
  str_remove(".RDS")

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
###############################################################################


i <- 1
model <- readRDS(north_models[[i]])
data <- model$data
name <- north_names[[i]]

post <- as_draws(model)
conditional_effects(model, 'north_patchdist_forest')

leave_one_out <- loo(model)
marginal_effects(model)

zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")
zinb$camper <- factor(zinb$camper, labels = c("no", "yes"))

fit_zinb1 <- brm(count ~ persons + child + camper, data = zinb,
                 family = zero_inflated_poisson("log"))

conditional_effects(fit_zinb1)


ggplot(data = data,aes(x = north_patchdist_forest,
                       y = choice))+
  geom_smooth()

predict <- predict(model)

