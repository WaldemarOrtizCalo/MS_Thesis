#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-03-06 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(survival)
library(ggsurvfit)
library(MuMIn)
library(lares)
library(doParallel)

#      Functions                                                            ####

#      Data                                                                 ####

covs_csvs <- list.files("1.DataManagement/ch2_data/clean/cov_extractions",
                        full.names = T,
                        pattern = ".csv") 

mort_data <- read_csv("1.DataManagement/ch2_data/clean/locs/deer_mortalitylocs.csv")

#      Cluster Setup                                                        ####

# Cluster Number
clust <- makeCluster(6)
registerDoParallel(clust)

# Exporting Packages
clusterEvalQ(clust,
             {
               library(tidyverse)
               library(MuMIn)
               library(survival)
             })

###############################################################################
#   Model North                                                             ####
#      Setup                                                                ####

# Home Range Data
hr_data <- read_csv(covs_csvs[[1]]) %>% 
  rename(individual.local.identifier = id,
         ordered_int_id = int_id)

# Mortality Data
mort <- mort_data %>% 
  distinct(individual.local.identifier,ordered_int_id,t_start,t_end, .keep_all = TRUE) %>% 
  dplyr::select(-c(location.long,location.lat,timestamp,date))

# Final Data
data_final <- left_join(hr_data,mort) %>% 
  relocate(names(mort),.after= ordered_int_id) %>% 
  arrange(individual.local.identifier, ordered_int_id) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  mutate(age = factor(age, levels = c("F","Y","A")),
         year = as.character(year)) %>% 
  filter(hr_type == "akde_95")

# Scaling Data
for (j in ((str_which(names(data_final),"hr_type")+1):ncol(data_final))) {
  data_final[,j] <- scale(data_final[,j])
  print(j)
}

#      Checking for Autocorrelation                                         ####

# Initial list of covs
cov_names_init <- names(data_final) %>% 
  str_subset("individual.local.identifier",negate = T)  %>%
  str_subset("month",negate = T)  %>%
  str_subset("site",negate = T)  %>%
  str_subset("ordered_int_id",negate = T)  %>%
  str_subset("t_start",negate = T)  %>%
  str_subset("t_end",negate = T)  %>%
  str_subset("event",negate = T)  %>%
  str_subset("hr_type",negate = T)  %>%
  str_subset("Shrub",negate = T) %>% 
  str_subset("Wetland",negate = T) %>% 
  str_subset("Barren",negate = T) %>% 
  str_subset("Water",negate = T)  %>%
  c()

# Making Correlation Plot
cov_df <- data_final[cov_names_init]

cor_bargraph <-corr_cross(cov_df, rm.na = T, max_pvalue = 0.07, 
                          top = 40,
                          plot = T)

# Final list of covs
cov_names_final <- cov_names_init %>% 
  str_subset("slope",negate = T) %>% 
  str_subset("lsi",negate = T) %>% 
  str_subset("patchdensity_Grassland",negate = T)



#      Survival Models                                                      ####

# Creating Survival Object
surv_object <- Surv(time = data_final$t_start,
                    time2 = data_final$t_end,
                    event = data_final$event,
                    type = "counting")

#        Kaplan-Meier Model                                                 ####

model_kaplanmeier_north <- survfit(surv_object ~ 1, data=data_final)

#        Cox Model                                                          ####

# Building the formula
formula <- as.formula(paste("surv_object ~ ", 
                            paste(cov_names_final, collapse= "+")))

# Building Global Model
cox <- coxph(formula, 
             data = data_final,
             na.action = "na.fail")

# Exporting data to clusters
clusterExport(cl=clust, varlist=c("cox","data_final","surv_object"), envir=environment())

# Dredge
print(paste0("Start Time:",Sys.time()))

dredge_north <- dredge(cox,
                       cluster = clust)

print(paste0("End Time:",Sys.time()))
###############################################################################
#   Model South                                                             ####
#      Setup                                                                ####

# Home Range Data
hr_data <- read_csv(covs_csvs[[2]]) %>% 
  rename(individual.local.identifier = id,
         ordered_int_id = int_id)

# Mortality Data
mort <- mort_data %>% 
  distinct(individual.local.identifier,ordered_int_id,t_start,t_end, .keep_all = TRUE) %>% 
  dplyr::select(-c(location.long,location.lat,timestamp,date))

# Final Data
data_final <- left_join(hr_data,mort) %>% 
  relocate(names(mort),.after= ordered_int_id) %>% 
  arrange(individual.local.identifier, ordered_int_id) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  mutate(age = factor(age, levels = c("F","Y","A")),
         year = as.character(year)) %>% 
  filter(hr_type == "akde_95")

# Scaling Data
for (j in ((str_which(names(data_final),"hr_type")+1):ncol(data_final))) {
  data_final[,j] <- scale(data_final[,j])
  print(j)
}

#      Checking for Autocorrelation                                         ####

# Initial list of covs
cov_names_init <- names(data_final) %>% 
  str_subset("individual.local.identifier",negate = T)  %>%
  str_subset("month",negate = T)  %>%
  str_subset("site",negate = T)  %>%
  str_subset("ordered_int_id",negate = T)  %>%
  str_subset("t_start",negate = T)  %>%
  str_subset("t_end",negate = T)  %>%
  str_subset("event",negate = T)  %>%
  str_subset("hr_type",negate = T)  %>%
  str_subset("Shrub",negate = T) %>% 
  str_subset("Wetland",negate = T) %>% 
  str_subset("Barren",negate = T) %>% 
  str_subset("Water",negate = T)  %>%
  c()

# Making Correlation Plot
cov_df <- data_final[cov_names_init]

cor_bargraph <-corr_cross(cov_df, rm.na = T, max_pvalue = 0.07, 
                          top = 40,
                          plot = T)

# Final list of covs
cov_names_final <- cov_names_init %>% 
  str_subset("slope",negate = T) %>% 
  str_subset("proportion_Deciduous",negate = T) %>% 
  str_subset("patchdensity_Evergreen",negate = T) %>% 
  str_subset("patchdensity_Mixed",negate = T) %>% 
  str_subset("lsi",negate = T) %>% 
  str_subset("meanpatcharea_Grassland",negate = T) 

#      Survival Models                                                      ####

# Creating Survival Object
surv_object <- Surv(time = data_final$t_start,
                    time2 = data_final$t_end,
                    event = data_final$event,
                    type = "counting")

#        Kaplan-Meier Model                                                 ####

model_kaplanmeier_south <- survfit(surv_object ~ 1, data=data_final)

#        Cox Model                                                          ####

# Building the formula
formula <- as.formula(paste("surv_object ~ ", 
                            paste(cov_names_final, collapse= "+")))

# Building Global Model
cox <- coxph(formula, 
             data = data_final,
             na.action = "na.fail")

# Exporting data to clusters
clusterExport(cl=clust, varlist=c("cox","data_final","surv_object"), envir=environment())
print(Sys.time())

# Dredge
print(paste0("Start Time:",Sys.time()))

dredge_south <- dredge(cox,
                       cluster = clust)

print(paste0("End Time:",Sys.time()))

###############################################################################