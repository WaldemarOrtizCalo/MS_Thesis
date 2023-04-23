#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-04-19 

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
library(survminer)

#      Functions                                                            ####

#      Data                                                                 ####
covs_csvs <- list.files("1.DataManagement/ch2_data/clean/cov_extractions",
                        full.names = T,
                        pattern = ".csv") 

mort_data <- read_csv("1.DataManagement/ch2_data/clean/locs/deer_mortalitylocs.csv")

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
  filter(hr_type == "akde_95") %>% 
  mutate(t_start = t_start+28,
         t_end = t_end+28,)



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
#           Risk Tables                                                     ####

# Setting up initial table

# General
risk_table_genpop <- data_final %>%
  group_by(year,month) %>% 
  summarise(n_deer = n_distinct(individual.local.identifier),
            n_events = sum(event))

# Male
risk_table_males <-data_final %>%
  filter(sex == "M") %>% 
  group_by(year,month) %>% 
  summarise(n_males = n_distinct(individual.local.identifier),
            events_males = sum(event))

# Female
risk_table_females <-data_final %>%
  filter(sex == "F") %>% 
  group_by(year,month) %>% 
  summarise(n_females = n_distinct(individual.local.identifier),
            events_females = sum(event))

# Final
risk_table_final <- list(risk_table_genpop, 
                         risk_table_males, 
                         risk_table_females) %>% 
  reduce(left_join, by = c("year","month")) %>% 
  ungroup() %>% 
  mutate(int_id = 1:nrow(.),.before = 1) %>% 
  mutate_if(is.numeric, ~replace_na(., 0))

# Cumulative sum of events

for (i in 2:nrow(risk_table_final)) {
  # Gen Pop
  event_t1 <- risk_table_final[i,5] %>% as.numeric()
  event_t2 <- risk_table_final[i-1,5] %>% as.numeric()
  cum_sum <- event_t1+event_t2
  
  risk_table_final[i,5] <- cum_sum
  
  # Males
  event_t1 <- risk_table_final[i,7] %>% as.numeric()
  event_t2 <- risk_table_final[i-1,7] %>% as.numeric()
  cum_sum <- event_t1+event_t2
  
  risk_table_final[i,7] <- cum_sum
  
  # Females
  event_t1 <- risk_table_final[i,9] %>% as.numeric()
  event_t2 <- risk_table_final[i-1,9] %>% as.numeric()
  cum_sum <- event_t1+event_t2
  
  risk_table_final[i,9] <- cum_sum
} 

# Plotting
ndeer_color <- "green4"
nevent_color <- "red4"

# General
ggplot(risk_table_final,aes(x = int_id))+
  geom_line(aes(y = n_deer),color = ndeer_color)+
  geom_line(aes(y = n_events),color = nevent_color) +
  scale_y_continuous(
    # Features of the first axis
    name = "Deer at Risk",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Number of Events"))+
  theme(axis.title.y = element_text(color = ndeer_color, size=13),
        axis.title.y.right = element_text(color = nevent_color, size=13)) +
  theme_bw()+
  ggtitle("General Population")+
  scale_x_continuous(name = "Months since study start",breaks = c(1,12,24,36,48,60,72))

# Male
ggplot(risk_table_final,aes(x = int_id))+
  geom_line(aes(y = n_males),color = ndeer_color)+
  geom_line(aes(y = events_males),color = nevent_color) +
  scale_y_continuous(
    # Features of the first axis
    name = "Deer at Risk",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Number of Events"))+
  theme(axis.title.y = element_text(color = ndeer_color, size=13),
        axis.title.y.right = element_text(color = nevent_color, size=13)) +
  theme_bw()+
  ggtitle("Males")+
  scale_x_continuous(name = "Months since study start",breaks = c(1,12,24,36,48,60,72))

# Female
ggplot(risk_table_final,aes(x = int_id))+
  geom_line(aes(y = n_females),color = ndeer_color)+
  geom_line(aes(y = events_females),color = nevent_color) +
  scale_y_continuous(
    # Features of the first axis
    name = "Deer at Risk",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Number of Events"))+
  theme(axis.title.y = element_text(color = ndeer_color, size=13),
        axis.title.y.right = element_text(color = nevent_color, size=13)) +
  theme_bw()+
  ggtitle("Females")+
  scale_x_continuous(name = "Months since study start",breaks = c(1,12,24,36,48,60,72))

#           General                                                         ####

model_kaplanmeier_north <- survfit(surv_object ~ 1, data=data_final)

gen_plot <- ggsurvfit(model_kaplanmeier_north) +
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

km_data_overall <- data_final 
surv_object_overall <- Surv(time = km_data_overall$t_start,
                            time2 = km_data_overall$t_end,
                            event = km_data_overall$event,
                            type = "counting")
km_model_overall <- survfit(surv_object_overall ~ sex, data= km_data_overall)

km_plot_sex <- ggsurvplot(km_model_overall,
                   conf.int = T,
                   risk.table = T,
                   break.x.by = 100,
                   surv.median.line= "hv")

ggsave(filename = "3.Chapter2/3.Output/kaplan_meier_plots/north/km_sex_final.png",
       plot = km_plot_sex,
       device = "png",
       width = 6,
       height = 4,
       units = "in")

#           Age                                                             ####

km_data_overall <- data_final 
surv_object_overall <- Surv(time = km_data_overall$t_start,
                            time2 = km_data_overall$t_end,
                            event = km_data_overall$event,
                            type = "counting")
km_model_overall <- survfit(surv_object_overall ~ age, data= km_data_overall)

km_plot_age <- ggsurvplot(km_model_overall,
                          conf.int = T,
                          censor = F,
                          risk.table = T,
                          break.x.by = 100,
                          surv.median.line= "hv")

###############################################################################
#   Model south                                                             ####
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
  filter(hr_type == "akde_95") %>% 
  mutate(t_start = t_start+28,
         t_end = t_end+28,)



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
#           Risk Tables                                                     ####

# Setting up initial table

# General
risk_table_genpop <- data_final %>%
  group_by(year,month) %>% 
  summarise(n_deer = n_distinct(individual.local.identifier),
            n_events = sum(event))

# Male
risk_table_males <-data_final %>%
  filter(sex == "M") %>% 
  group_by(year,month) %>% 
  summarise(n_males = n_distinct(individual.local.identifier),
            events_males = sum(event))

# Female
risk_table_females <-data_final %>%
  filter(sex == "F") %>% 
  group_by(year,month) %>% 
  summarise(n_females = n_distinct(individual.local.identifier),
            events_females = sum(event))

# Final
risk_table_final <- list(risk_table_genpop, 
                         risk_table_males, 
                         risk_table_females) %>% 
  reduce(left_join, by = c("year","month")) %>% 
  ungroup() %>% 
  mutate(int_id = 1:nrow(.),.before = 1) %>% 
  mutate_if(is.numeric, ~replace_na(., 0))

# Cumulative sum of events

for (i in 2:nrow(risk_table_final)) {
  # Gen Pop
  event_t1 <- risk_table_final[i,5] %>% as.numeric()
  event_t2 <- risk_table_final[i-1,5] %>% as.numeric()
  cum_sum <- event_t1+event_t2
  
  risk_table_final[i,5] <- cum_sum
  
  # Males
  event_t1 <- risk_table_final[i,7] %>% as.numeric()
  event_t2 <- risk_table_final[i-1,7] %>% as.numeric()
  cum_sum <- event_t1+event_t2
  
  risk_table_final[i,7] <- cum_sum
  
  # Females
  event_t1 <- risk_table_final[i,9] %>% as.numeric()
  event_t2 <- risk_table_final[i-1,9] %>% as.numeric()
  cum_sum <- event_t1+event_t2
  
  risk_table_final[i,9] <- cum_sum
} 

# Plotting
ndeer_color <- "green4"
nevent_color <- "red4"

# General
ggplot(risk_table_final,aes(x = int_id))+
  geom_line(aes(y = n_deer),color = ndeer_color)+
  geom_line(aes(y = n_events),color = nevent_color) +
  scale_y_continuous(
    # Features of the first axis
    name = "Deer at Risk",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Number of Events"))+
  theme(axis.title.y = element_text(color = ndeer_color, size=13),
        axis.title.y.right = element_text(color = nevent_color, size=13)) +
  theme_bw()+
  ggtitle("General Population")+
  scale_x_continuous(name = "Months since study start",breaks = c(1,12,24,36,48,60,72))

# Male
ggplot(risk_table_final,aes(x = int_id))+
  geom_line(aes(y = n_males),color = ndeer_color)+
  geom_line(aes(y = events_males),color = nevent_color) +
  scale_y_continuous(
    # Features of the first axis
    name = "Deer at Risk",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Number of Events"))+
  theme(axis.title.y = element_text(color = ndeer_color, size=13),
        axis.title.y.right = element_text(color = nevent_color, size=13)) +
  theme_bw()+
  ggtitle("Males")+
  scale_x_continuous(name = "Months since study start",breaks = c(1,12,24,36,48,60,72))

# Female
ggplot(risk_table_final,aes(x = int_id))+
  geom_line(aes(y = n_females),color = ndeer_color)+
  geom_line(aes(y = events_females),color = nevent_color) +
  scale_y_continuous(
    # Features of the first axis
    name = "Deer at Risk",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Number of Events"))+
  theme(axis.title.y = element_text(color = ndeer_color, size=13),
        axis.title.y.right = element_text(color = nevent_color, size=13)) +
  theme_bw()+
  ggtitle("Females")+
  scale_x_continuous(name = "Months since study start",breaks = c(1,12,24,36,48,60,72))

#           General                                                         ####

model_kaplanmeier_south <- survfit(surv_object ~ 1, data=data_final)

gen_plot <- ggsurvfit(model_kaplanmeier_south) +
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

km_data_overall <- data_final 
surv_object_overall <- Surv(time = km_data_overall$t_start,
                            time2 = km_data_overall$t_end,
                            event = km_data_overall$event,
                            type = "counting")
km_model_overall <- survfit(surv_object_overall ~ sex, data= km_data_overall)

km_plot_sex <- ggsurvplot(km_model_overall,
                          conf.int = T,
                          risk.table = T,
                          break.x.by = 100,
                          surv.median.line= "hv")

#           Age                                                             ####

km_data_overall <- data_final 
surv_object_overall <- Surv(time = km_data_overall$t_start,
                            time2 = km_data_overall$t_end,
                            event = km_data_overall$event,
                            type = "counting")
km_model_overall <- survfit(surv_object_overall ~ age, data= km_data_overall)

km_plot_age <- ggsurvplot(km_model_overall,
                          conf.int = T,
                          censor = F,
                          risk.table = T,
                          break.x.by = 100,
                          surv.median.line= "hv")


###############################################################################
