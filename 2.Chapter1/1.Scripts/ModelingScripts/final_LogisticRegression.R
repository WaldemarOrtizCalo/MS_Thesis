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

#      Models                                                               ####
#        Global Models                                                      ####
#           Models                                                          ####

foreach(i = 1:length(data)) %do% {
  
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
  
  # Export
  saveRDS(model,
          file = paste0("2.Chapter1/3.Output/models_global_landscapelevel/north/",names_north[i],"_global.rds"))
  
  print(paste0(i," out of ",length(data)," completed"))
}


#           Effect Plots                                                    ####

# Importing Data
north_models <- list.files("2.Chapter1/3.Output/models_global_landscapelevel/north",
                           full.names = T) %>% 
  str_subset(".rds")%>% 
  lapply(readRDS)

north_names <- list.files("2.Chapter1/3.Output/models_global_landscapelevel/north",
                          full.names = F) %>% str_remove(".rds")

# Making Plots
for (i in 1:length(north_models)) {
  
  # Choosing Model to Plot
  model <- north_models[[1]]
  model_dir <- paste0("2.Chapter1/3.Output/models_global_landscapelevel/north/effectplots/",north_names[i])
  
  for (v in 2:length(names(model$coefficients))) {
    
    # Extracting a Covariate to Plot
    cov<-names(model$coefficients)[v]
    
    # Effect Plot
    plot <- effect_plot(model, 
                        pred = !!cov, 
                        interval = TRUE, 
                        plot.points = F,
                        x.label = cov,
                        y.label = "Probability of Use")
    
    p <- plot + ylim(0, 1)
    
    # Exporting Plot
    ggsave(filename = paste0(model_dir,"/",cov,".png"),
           plot = p,
           device = "png",
           width = 6,
           height = 4,
           units = "in")
    
    print(paste0("Model ",i, " Coefficient ", v, " out of ", length(names(model$coefficients))))
    
  }
}

#        Dredge Models                                                      ####
#           Start of Cluster                                                ####

# Cluster Number
cl <- makeCluster(3)
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
            file = paste0("2.Chapter1/3.Output/models_dredge_landscapelevel/north/",names_north[i],"_dredge.csv"),
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
#        Global Models                                                      ####

foreach(i = 1:length(data)) %do% {
  
  # Separating specific dataframe
  df <- data[[i]]
  name <- names_south[[i]]
  
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
  
  # Export
  saveRDS(model,
          file = paste0("2.Chapter1/3.Output/models_global_landscapelevel/south/",names_south[i],"_global.rds"))
  
  print(paste0(i," out of ",length(data)," completed"))
}


#           Effect Plots                                                    ####

# Importing Data
south_models <- list.files("2.Chapter1/3.Output/models_global_landscapelevel/south",
                           full.names = T) %>% str_subset(".rds")%>% 
  lapply(readRDS)

south_names <- list.files("2.Chapter1/3.Output/models_global_landscapelevel/south",
                          full.names = F) %>% str_remove(".rds")

# Making Plots
for (i in 1:length(south_models)) {
  
  # Choosing Model to Plot
  model <- south_models[[1]]
  model_dir <- paste0("2.Chapter1/3.Output/models_global_landscapelevel/south/effectplots/",south_names[i])
  
  for (v in 2:length(names(model$coefficients))) {
    
    # Extracting a Covariate to Plot
    cov<-names(model$coefficients)[v]
    
    # Effect Plot
    plot <- effect_plot(model, 
                        pred = !!cov, 
                        interval = TRUE, 
                        plot.points = F,
                        x.label = cov,
                        y.label = "Probability of Use")
    
    p <- plot + ylim(0, 1)
    
    # Exporting Plot
    ggsave(filename = paste0(model_dir,"/",cov,".png"),
           plot = p,
           device = "png",
           width = 6,
           height = 4,
           units = "in")
    
    print(paste0("Model ",i, " Coefficient ", v, " out of ", length(names(model$coefficients))))
    
  }
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
#        Global Models                                                      ####

foreach(i = 1:length(data)) %do% {
  
  # Separating specific dataframe
  df <- data[[i]]
  name <- names_southeast[[i]]
  
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
  
  # Export
  saveRDS(model,
          file = paste0("2.Chapter1/3.Output/models_global_landscapelevel/southeast/",names_southeast[i],"_global.rds"))
  
  print(paste0(i," out of ",length(data)," completed"))
}


#           Effect Plots                                                    ####

# Importing Data
southeast_models <- list.files("2.Chapter1/3.Output/models_global_landscapelevel/southeast",
                           full.names = T) %>% str_subset(".rds")%>% 
  lapply(readRDS)

southeast_names <- list.files("2.Chapter1/3.Output/models_global_landscapelevel/southeast",
                          full.names = F) %>% str_remove(".rds")

# Making Plots
for (i in 1:length(southeast_models)) {
  
  # Choosing Model to Plot
  model <- southeast_models[[1]]
  model_dir <- paste0("2.Chapter1/3.Output/models_global_landscapelevel/southeast/effectplots/",southeast_names[i])
  
  for (v in 2:length(names(model$coefficients))) {
    
    # Extracting a Covariate to Plot
    cov<-names(model$coefficients)[v]
    
    # Effect Plot
    plot <- effect_plot(model, 
                        pred = !!cov, 
                        interval = TRUE, 
                        plot.points = F,
                        x.label = cov,
                        y.label = "Probability of Use")
    
    p <- plot + ylim(0, 1)
    
    # Exporting Plot
    ggsave(filename = paste0(model_dir,"/",cov,".png"),
           plot = p,
           device = "png",
           width = 6,
           height = 4,
           units = "in")
    
    print(paste0("Model ",i, " Coefficient ", v, " out of ", length(names(model$coefficients))))
    
  }
}

###############################################################################
#   Bayesian                                                                ####

pplot<-plot(model2, "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)

###############################################################################
# Notes 

# Dredging the model takes a incredible amount of time. 
# 3 Variable Dredge = 10s
# 6 Variable Dredge = 97s
# 9 Variable Dredge = 858s
# 12 Variable Dredge = 7722s
# 15 Variable Dredge = 69498s
# 18 Variable Dredge = 625401s