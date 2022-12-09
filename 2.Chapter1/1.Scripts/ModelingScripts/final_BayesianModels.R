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
    str_subset("barren",negate = T) %>% c()
  
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
            paste0("2.Chapter1/3.Output/covariate_analysis/north/",str_remove(name,",png"),".csv"))
  
  print(i)
}

#      Model                                                                ####
#        Bayesian Model                                                     ####

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
  str_subset("barren",negate = T) 

# Building the formula
formula <- as.formula(paste("choice ~ ", paste(covs, collapse= "+")))

# Model

# Model Settings
chains <- 4 
iter <- 5000
warmup <- iter*.25
thin <- 1
cores <- chains
refresh <- iter/10

model <- stan_glm(
  formula = formula,
  data = df,
  family = binomial(link = "logit"),
  prior_intercept = normal(0, 1),
  prior = normal(0, 1),
  QR = TRUE,
  refresh = refresh,
  chains = chains, 
  iter = iter,
  warmup = warmup,
  thin = thin,
  cores = cores
)

saveRDS(object = model,
  file = paste0("2.Chapter1/3.Output/models_bayesian/north/bayesian_",name,".RDS"))

return(print(paste0(i," out of ", length(data_north), " is completed")))

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
    str_subset("barren",negate = T) %>% c()
  
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
            paste0("2.Chapter1/3.Output/covariate_analysis/south/",str_remove(name,",png"),".csv"))
  
  print(i)
}

#      Models                                                               ####
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
    str_subset("barren",negate = T) %>% c()
  
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
            paste0("2.Chapter1/3.Output/covariate_analysis/southeast/",str_remove(name,",png"),".csv"))
  
  print(i)
}

#      Models                                                               ####
###############################################################################
#   [brm]                                                                   ####
library(brms)
i <- 1
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
  str_subset("barren",negate = T) 

# Building the formula
formula <- as.formula(paste("choice ~ ", paste(covs[1:3], collapse= "+")))

# Model

# Model Settings
chains <- 4 
iter <- 1000
warmup <- iter*.25
thin <- 1
cores <- chains
refresh <- iter/10

model_stan <- stan_glm(
  formula = formula,
  data = df,
  family = binomial(link = "logit"),
  prior_intercept = normal(0, 1),
  prior = normal(0, 1),
  QR = TRUE,
  refresh = refresh,
  chains = chains, 
  iter = iter,
  warmup = warmup,
  thin = thin,
  cores = cores
)

model_brm <- brm(
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

summary(model_brm)

summary(model_stan)


###############################################################################
