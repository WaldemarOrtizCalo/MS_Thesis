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
library(coxme)
library(lubridate)
library(gtsummary)
library(tidycmprsk)
library(condSURV)

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
               library(coxme)
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
         sex = factor(sex, levels = c("F","M")),
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
  str_subset("patchdensity",negate = T) %>% 
  str_subset("aspect",negate = T) %>% 
  str_subset("hr_size_ha",negate = T) %>% 
  str_subset("proportion",negate = T) %>% 
  str_subset("dem",negate = T)

#      Survival Models                                                      ####

# Creating Survival Object
surv_object <- Surv(time = data_final$t_start,
                    time2 = data_final$t_end,
                    event = data_final$event,
                    type = "counting")

#        Kaplan-Meier Model                                                 ####
#           General                                                         ####
model_kaplanmeier_north <- survfit(surv_object ~ 1, data=data_final)

gen_plot <-   ggsurvfit(model_kaplanmeier_north) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  add_confidence_interval()+
  add_risktable()

model_kaplanmeier_north %>% 
  tbl_survfit(
    times = 365.25,
    label_header = "**1-year survival (95% CI)**"
  )

#           Sex                                                             ####
model_kaplanmeier_north_sex <- survfit(surv_object ~ sex, data=data_final)

sex_plot <- ggsurvfit(model_kaplanmeier_north_sex) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  add_confidence_interval()+
  add_risktable()


model_kaplanmeier_north_sex %>% 
  tbl_survfit(
    times = 365.25,
    label_header = "**1-year survival (95% CI)**"
  )

#           Age                                                             ####

model_kaplanmeier_north_age <- survfit(surv_object ~ age, data=data_final)

age_plot <- ggsurvfit(model_kaplanmeier_north_age) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  add_confidence_interval()+
  add_risktable()


model_kaplanmeier_north_age %>% 
  tbl_survfit(
    times = 365.25,
    label_header = "**1-year survival (95% CI)**"
  )


#           Sex + Age (Exploration)                                         ####

model_kaplanmeier_north_sexage <- survfit(surv_object ~ age + sex, data=data_final)

sexage_plot <- ggsurvfit(model_kaplanmeier_north_sexage) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  add_confidence_interval()+
  add_risktable()


model_kaplanmeier_north_age %>% 
  tbl_survfit(
    times = 365.25,
    label_header = "**1-year survival (95% CI)**"
  )

#        Cox Model                                                          ####

# Building the formula
formula <- as.formula(paste("surv_object ~ ", 
                            paste(cov_names_final, collapse= "+")))

# Building Global Model
cox_north <- coxph(formula, 
             data = data_final,
             na.action = "na.fail")

# Exporting data to clusters
clusterExport(cl=clust, varlist=c("cox_north","data_final","surv_object"), envir=environment())

# Dredge
print(paste0("Start Time:",Sys.time()))

dredge_north <- dredge(cox,
                       cluster = clust)

print(paste0("End Time:",Sys.time()))

# Dredge Export
write_csv(dredge_north,
          "3.Chapter2/3.Output/dredge/dredge_north.csv")

# Dredge Export Top Models 

dredge_north_top <- read_csv("3.Chapter2/3.Output/dredge/dredge_north.csv") %>% 
  slice_head(n = 200) %>% 
  write_csv("3.Chapter2/3.Output/dredge/dredge_north_topmodels.csv",
            append = F)

#        Cox-RandSlope Sub Model                                            ####

cov_names_randslope <- cov_names_final %>% str_subset(pattern = "year",
                                                      negate = T)

# Building the formula
formula <- as.formula(paste("surv_object ~ ", 
                            paste(cov_names_randslope, collapse= "+"),
                            "+ (year|1)"))

# Example
coxme_model <- coxme(formula, 
                     data = data_final,
                     na.action = "na.fail")

# Exporting data to clusters
clusterExport(cl=clust, varlist=c("coxme_model","data_final","surv_object"), envir=environment())

# Dredge
print(paste0("Start Time:",Sys.time()))

dredge_north_coxme <- dredge(coxme_model,
                             cluster = clust)

print(paste0("End Time:",Sys.time()))

# Dredge Export
write_csv(dredge_north_coxme,
          "3.Chapter2/3.Output/dredge/dredge_north_randint.csv")

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
         sex = factor(sex, levels = c("F","M")),
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
  str_subset("patchdensity",negate = T) %>% 
  str_subset("aspect",negate = T) %>% 
  str_subset("hr_size_ha",negate = T) %>% 
  str_subset("proportion",negate = T) %>% 
  str_subset("dem",negate = T)

#      Survival Models                                                      ####

# Creating Survival Object
surv_object <- Surv(time = data_final$t_start,
                    time2 = data_final$t_end,
                    event = data_final$event,
                    type = "counting")

#        Kaplan-Meier Model                                                 ####
#           General                                                         ####
model_kaplanmeier_south <- survfit(surv_object ~ 1, data=data_final)

gen_plot <-   ggsurvfit(model_kaplanmeier_south) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  add_confidence_interval()+
  add_risktable()

model_kaplanmeier_south %>% 
  tbl_survfit(
    times = 365.25,
    label_header = "**1-year survival (95% CI)**"
  )

#           Sex                                                             ####
model_kaplanmeier_south_sex <- survfit(surv_object ~ sex, data=data_final)

sex_plot <- ggsurvfit(model_kaplanmeier_south_sex) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  add_confidence_interval()+
  add_risktable()


model_kaplanmeier_south_sex %>% 
  tbl_survfit(
    times = 365.25,
    label_header = "**1-year survival (95% CI)**"
  )

#           Age                                                             ####

model_kaplanmeier_south_age <- survfit(surv_object ~ age, data=data_final)

age_plot <- ggsurvfit(model_kaplanmeier_south_age) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  add_confidence_interval()+
  add_risktable()


model_kaplanmeier_south_age %>% 
  tbl_survfit(
    times = 365.25,
    label_header = "**1-year survival (95% CI)**"
  )


#           Sex + Age (Exploration)                                         ####

model_kaplanmeier_south_sexage <- survfit(surv_object ~ age + sex, data=data_final)

sexage_plot <- ggsurvfit(model_kaplanmeier_south_sexage) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  add_confidence_interval()+
  add_risktable()


model_kaplanmeier_south_age %>% 
  tbl_survfit(
    times = 365.25,
    label_header = "**1-year survival (95% CI)**"
  )
#        Cox Model                                                          ####

# Building the formula
formula <- as.formula(paste("surv_object ~ ", 
                            paste(cov_names_final, collapse= "+")))

# Building Global Model
cox_south <- coxph(formula, 
                   data = data_final,
                   na.action = "na.fail")

# Exporting data to clusters
clusterExport(cl=clust, varlist=c("cox_south","data_final","surv_object"), envir=environment())

# Dredge
print(paste0("Start Time:",Sys.time()))

dredge_south <- dredge(cox,
                       cluster = clust)

print(paste0("End Time:",Sys.time()))

# Dredge Export
write_csv(dredge_south,
          "3.Chapter2/3.Output/dredge/dredge_south.csv")

# Dredge Export Top Models 

dredge_south_top <- read_csv("3.Chapter2/3.Output/dredge/dredge_south.csv") %>% 
  slice_head(n = 200) %>% 
  write_csv("3.Chapter2/3.Output/dredge/dredge_south_topmodels.csv",
            append = F)

#        Cox-RandSlope Sub Model                                            ####

cov_names_randslope <- cov_names_final %>% str_subset(pattern = "year",
                                                      negate = T)

# Building the formula
formula <- as.formula(paste("surv_object ~ ", 
                            paste(cov_names_randslope, collapse= "+"),
                            "+ (year|1)"))

# Example
coxme_model <- coxme(formula, 
                     data = data_final,
                     na.action = "na.fail")

# Exporting data to clusters
clusterExport(cl=clust, varlist=c("coxme_model","data_final","surv_object"), envir=environment())

# Dredge
print(paste0("Start Time:",Sys.time()))

dredge_south_coxme <- dredge(coxme_model,
                             cluster = clust)

print(paste0("End Time:",Sys.time()))

# Dredge Export
write_csv(dredge_south_coxme,
          "3.Chapter2/3.Output/dredge/dredge_south_randint.csv")

###############################################################################