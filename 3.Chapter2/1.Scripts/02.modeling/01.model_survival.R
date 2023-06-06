#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-03-06 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(survival)
library(brms)
library(tidybayes)
library(tidyverse)
library(MuMIn)
library(coxme)
library(doParallel)
library(lares)
library(ggsurvfit)
library(gtsummary)

#      Functions                                                            ####

#      Data                                                                 ####
#        Importing Data                                                     ####

covs_csvs <- list.files("1.DataManagement/ch2_data/clean/cov_extractions",
                        full.names = T,
                        pattern = ".csv") 

mort_table <- read_csv("1.DataManagement\\ch2_data\\mort_tables\\mort_tbl_v3.csv") %>% 
  filter(event == 1)

#        Adding timescale                                                   ####
anchor_date <- ymd_hms("2015-01-01T00:00:01")

studyday_calc <- function(date,anchor_date) {
  
  # Input day
  input_date <- floor_date(date, unit = "day")
  
  # Day calculation
  study_day <- difftime(input_date, anchor_date, units = "day") %>% 
    as.numeric() %>% 
    floor() + 1
  
  # Returning object
  return(study_day)
}


# Creating time table with locs and periods
loc_timedata <- read_csv("1.DataManagement/ch2_data/clean/locs/deer_mortalitylocs.csv") %>% 
  group_by(individual.local.identifier,
           ordered_int_id) %>% 
  summarise(last_day = max(timestamp),
            study_time = studyday_calc(date = last_day,
                                       anchor_date)) %>% 
  rename(id = individual.local.identifier,
         int_id = ordered_int_id,
         event_day = study_time) %>%
  mutate(tstart = event_day - 7,
         tend = event_day,
         tduration = tend - tstart) %>% 
  select(- c(event_day))

#        Adding Sex and Age                                                 ####

# Dataframe with demographic data
deer_age_sex <- read_csv("1.DataManagement/ch2_data/clean/locs/deer_mortalitylocs.csv") %>% 
  mutate(age = factor(age,levels = c("F", "Y" , "A")),
         sex = factor(sex,levels = c("F", "M" ))) %>% 
  group_by(individual.local.identifier,ordered_int_id) %>% 
  summarize(sex = names(which.max(table(sex))),
            age = names(which.max(table(age)))) %>% 
  rename("id" = "individual.local.identifier",
         "int_id"= "ordered_int_id")


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

# Importing Data
hr_cov_data <- covs_csvs[1] %>% 
  read_csv() 

# Identifying Max Interval 
locs_maxintid <- hr_cov_data %>% 
  group_by(id) %>% 
  summarize(int_id_max = max(int_id))

# Creating Relevant Mortality Table
loc_mort_table <- locs_maxintid %>%
  mutate(event = ifelse(.$id %in% mort_table$individual.local.identifier,1,0)) %>% 
  rename("int_id" = "int_id_max")

# # Creating Final Dataset
loc_data_final <- left_join(hr_cov_data,
                            loc_mort_table,
                            by = c("id",
                                   "int_id")) %>% 
  relocate(event, .after = hr_size_ha) %>% 
  mutate(event = replace_na(event,0)) %>% 
  left_join(loc_timedata,
            by = c("id", "int_id")) %>% 
  relocate(c("last_day",
             "tstart",
             "tend",
             "tduration"),
           .before = event) %>% 
  left_join(deer_age_sex,
            by = c("id", "int_id")) %>% 
  relocate(c("sex",
             "age"),
           .after = int_id) %>% 
  replace(is.na(.), 0) %>% 
  mutate(year = year(last_day),.after = last_day)

# Scaling Data
for (j in ((str_which(names(loc_data_final),"event")+1):ncol(loc_data_final))) {
  loc_data_final[,j] <- scale(loc_data_final[,j])
  print(j)
}   

#      Checking for Autocorrelation                                         ####

# Initial list of covs
cov_names_init <- names(loc_data_final) %>% 
  str_subset("id",negate = T)  %>%
  str_subset("int_id",negate = T)  %>%
  str_subset("site",negate = T)  %>%
  str_subset("ordered_int_id",negate = T)  %>%
  str_subset("tstart",negate = T)  %>%
  str_subset("tend",negate = T)  %>%
  str_subset("event",negate = T)  %>%
  str_subset("hr_size_ha",negate = T) %>%
  str_subset("hr_type",negate = T) %>%
  str_subset("last_day",negate = T) %>%
  str_subset("tduration",negate = T) %>% 
  str_subset("Shrub",negate = T) %>% 
  str_subset("Wetland",negate = T) %>% 
  str_subset("Barren",negate = T) %>% 
  str_subset("Water",negate = T)  %>%
  str_subset("slope",negate = T)  %>%
  str_subset("proportion",negate = T)  %>%
  str_subset("patchdensity_Forest",negate = T)  %>%
  str_subset("meanpatcharea_cropland",negate = T)  %>%
  c()

# Making Correlation Plot
cov_df <- loc_data_final[cov_names_init]

cor_bargraph <-corr_cross(cov_df, rm.na = T, max_pvalue = 0.07, 
                          top = 40,
                          plot = T)

# Final list of covs
cov_names_final <- cov_names_init 

#      Survival Models                                                      ####

# Creating Survival Object
surv_object <- Surv(time = loc_data_final$tstart,
                    time2 = loc_data_final$tend,
                    event = loc_data_final$event,
                    type = "counting")

#        Kaplan-Meier Model                                                 ####
#           General                                                         ####
model_kaplanmeier_north <- survfit(surv_object ~ 1, data=loc_data_final)

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
model_kaplanmeier_north_sex <- survfit(surv_object ~ sex, data=loc_data_final)

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

model_kaplanmeier_north_age <- survfit(surv_object ~ age, data=loc_data_final)

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


#        Cox Model                                                          ####

# Building the formula
formula <- as.formula(paste("surv_object ~ ", 
                            paste(cov_names_final, collapse= "+")))

# Building Global Model
cox_north <- coxph(formula, 
             data = loc_data_final,
             na.action = "na.fail")

# Exporting data to clusters
clusterExport(cl=clust, varlist=c("cox_north","loc_data_final","surv_object"), envir=environment())

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
                     data = loc_data_final,
                     na.action = "na.fail")

# Exporting data to clusters
clusterExport(cl=clust, varlist=c("coxme_model","loc_data_final","surv_object"), envir=environment())

# Dredge
print(paste0("Start Time:",Sys.time()))

dredge_north_coxme <- dredge(coxme_model,
                             cluster = clust)

print(paste0("End Time:",Sys.time()))

# Dredge Export
write_csv(dredge_north_coxme,
          "3.Chapter2/3.Output/dredge/dredge_north_randint.csv")

#          Testing for PH                                                   ####
# resids <- cox.zph(coxme_model)

#        AICc Model Selection                                               ####

# dredge_df <- dredge(coxme_model,
#                     cluster = clust) %>% 
#   subset(delta < 2)
# 
# mod_list <- get.models(dredge_df,
#                        subset = delta < 2)
# 
# model_averages <- list()
# 
# cov_list <- names(dredge_df)
# 
# cutoff <- cov_list %>% str_which("df")-1
# 
# cov_list_final <- names(dredge_df) %>% 
#   .[1:cutoff] %>% 
#   str_subset("age",negate = T) %>% 
#   str_subset("sex",negate = T)
# 
# for (i in 1:length(cov_list_final)) {
#   
#   model_averages[[i]] <- modavg(mod_list,
#                                 parm = cov_list_final[[i]])
#   
#   print(i)
# }

###############################################################################
#   Model south                                                             ####
#      Setup                                                                ####

# Importing Data
hr_cov_data <- covs_csvs[2] %>% 
  read_csv() 

# Identifying Max Interval 
locs_maxintid <- hr_cov_data %>% 
  group_by(id) %>% 
  summarize(int_id_max = max(int_id))

# Creating Relevant Mortality Table
loc_mort_table <- locs_maxintid %>%
  mutate(event = ifelse(.$id %in% mort_table$individual.local.identifier,1,0)) %>% 
  rename("int_id" = "int_id_max")

# # Creating Final Dataset
loc_data_final <- left_join(hr_cov_data,
                            loc_mort_table,
                            by = c("id",
                                   "int_id")) %>% 
  relocate(event, .after = hr_size_ha) %>% 
  mutate(event = replace_na(event,0)) %>% 
  left_join(loc_timedata,
            by = c("id", "int_id")) %>% 
  relocate(c("last_day",
             "tstart",
             "tend",
             "tduration"),
           .before = event) %>% 
  left_join(deer_age_sex,
            by = c("id", "int_id")) %>% 
  relocate(c("sex",
             "age"),
           .after = int_id) %>% 
  replace(is.na(.), 0) %>% 
  mutate(year = year(last_day),.after = last_day)

# Scaling Data
for (j in ((str_which(names(loc_data_final),"event")+1):ncol(loc_data_final))) {
  loc_data_final[,j] <- scale(loc_data_final[,j])
  print(j)
}   

#      Checking for Autocorrelation                                         ####

# Initial list of covs
cov_names_init <- names(loc_data_final) %>% 
  str_subset("id",negate = T)  %>%
  str_subset("int_id",negate = T)  %>%
  str_subset("site",negate = T)  %>%
  str_subset("ordered_int_id",negate = T)  %>%
  str_subset("tstart",negate = T)  %>%
  str_subset("tend",negate = T)  %>%
  str_subset("event",negate = T)  %>%
  str_subset("hr_size_ha",negate = T) %>%
  str_subset("hr_type",negate = T) %>%
  str_subset("last_day",negate = T) %>%
  str_subset("tduration",negate = T) %>% 
  str_subset("Shrub",negate = T) %>% 
  str_subset("Wetland",negate = T) %>% 
  str_subset("Barren",negate = T) %>% 
  str_subset("Water",negate = T)  %>%
  str_subset("slope",negate = T)  %>%
  str_subset("proportion",negate = T)  %>%
  str_subset("patchdensity_Forest",negate = T)  %>%
  str_subset("meanpatcharea_cropland",negate = T)  %>%
  c()

# Making Correlation Plot
cov_df <- loc_data_final[cov_names_init]

cor_bargraph <-corr_cross(cov_df, rm.na = T, max_pvalue = 0.07, 
                          top = 40,
                          plot = T)

# Final list of covs
cov_names_final <- cov_names_init 

#      Survival Models                                                      ####

# Creating Survival Object
surv_object <- Surv(time = loc_data_final$tstart,
                    time2 = loc_data_final$tend,
                    event = loc_data_final$event,
                    type = "counting")

#        Kaplan-Meier Model                                                 ####
#           General                                                         ####
model_kaplanmeier_south <- survfit(surv_object ~ 1, data=loc_data_final)

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
model_kaplanmeier_south_sex <- survfit(surv_object ~ sex, data=loc_data_final)

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

model_kaplanmeier_south_age <- survfit(surv_object ~ age, data=loc_data_final)

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


#        Cox Model                                                          ####

# Building the formula
formula <- as.formula(paste("surv_object ~ ", 
                            paste(cov_names_final, collapse= "+")))

# Building Global Model
cox_south <- coxph(formula, 
                   data = loc_data_final,
                   na.action = "na.fail")

# Exporting data to clusters
clusterExport(cl=clust, varlist=c("cox_south","loc_data_final","surv_object"), envir=environment())

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
                     data = loc_data_final,
                     na.action = "na.fail")

# Exporting data to clusters
clusterExport(cl=clust, varlist=c("coxme_model","loc_data_final","surv_object"), envir=environment())

# Dredge
print(paste0("Start Time:",Sys.time()))

dredge_south_coxme <- dredge(coxme_model,
                             cluster = clust)

print(paste0("End Time:",Sys.time()))

# Dredge Export
write_csv(dredge_south_coxme,
          "3.Chapter2/3.Output/dredge/dredge_south_randint.csv")

# #          Testing for PH                                                   ####
# resids <- cox.zph(coxme_model)
# 
# #        AICc Model Selection                                               ####
# 
# dredge_df <- dredge(coxme_model,
#                     cluster = clust) %>% 
#   subset(delta < 2)
# 
# mod_list <- get.models(dredge_df,
#                        subset = delta < 2)
# 
# model_averages <- list()
# 
# cov_list <- names(dredge_df)
# 
# cutoff <- cov_list %>% str_which("df")-1
# 
# cov_list_final <- names(dredge_df) %>% 
#   .[1:cutoff] %>% 
#   str_subset("age",negate = T) %>% 
#   str_subset("sex",negate = T)
# 
# for (i in 1:length(cov_list_final)) {
#   
#   model_averages[[i]] <- modavg(mod_list,
#                                 parm = cov_list_final[[i]])
#   
#   print(i)
# }

###############################################################################