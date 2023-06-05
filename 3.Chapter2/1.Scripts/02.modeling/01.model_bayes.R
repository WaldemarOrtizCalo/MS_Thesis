#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-05-27 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(survival)
library(brms)
library(tidybayes)
library(tidyverse)

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

###############################################################################
#   North                                                                   ####
#      Data Import and Cleaning                                             ####

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
  mutate(event = ifelse(is.na(event),0,1)) %>% 
  left_join(loc_timedata,
            by = c("id", "int_id"))

#      Frequentist Model Check                                              ####
data_sub <- loc_data_final

id_sub <- unique(data_sub$id) %>% .[1:20]

data_sub <- data_sub[data_sub$id %in% id_sub, ]

# Creating Survival Object
surv_object <- Surv(time = data_sub$tstart,
                    time2 = data_sub$tend,
                    event = data_sub$event)

# model
cox_north <- coxph(surv_object ~ shdi, 
                   data = data_sub,
                   na.action = "na.fail")
summary(cox_north)

#      Bayesian Model                                                       ####

brm_coxph_test <- brm(
  formula = event ~ as.factor(tend) + shdi+ offset(log(tduration)),
  family = poisson(),
  prior = set_prior('normal(0, 4)', class = 'b'), 
  data = data_sub, chains = 3, cores = 3, iter = 1000,
  thin = 3)

coef <- fixef(brm_coxph_test)

###############################################################################
