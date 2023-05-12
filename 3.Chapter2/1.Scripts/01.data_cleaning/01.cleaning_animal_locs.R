#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-01-27 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####
#      Library                                                              ####
library(tidyverse)
library(readxl)
library(lubridate)
library(foreach)

#      Functions                                                            ####
#        Time Calculation Function                                          ####

# Function
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

#      Data                                                                 ####

# Importing Mortality table
table_mortality_original <- read_xlsx("1.DataManagement\\ch2_data\\dbo_tblMortality.xlsx") %>% 
  filter(StudyArea %in% c("N","S")) %>% 
  filter(EstAge %in% c("F","Y","A")) %>% 
  mutate(StudyArea = factor(StudyArea, levels = c("N","S"))) %>%
  mutate(Sex = factor(Sex, levels = c("M","F"))) %>% 
  mutate(EstAge = factor(EstAge, levels = c("F","Y","A"))) %>% 
  rename(individual.local.identifier = DeerID)

# Deer Locations
locs_deer <- read_csv("1.DataManagement/loc_data/deer_all_final_clean.csv") %>% 
  filter(site %in% c("North", "South")) %>% 
  mutate(age = ifelse(age == "A1" | age == "A2" |age == "A3" |age == "A4", "A", age)) %>% 
  filter(age %in% c("F","Y","A")) %>% 
  mutate(site = factor(site, levels = c("North","South"))) %>%
  mutate(sex = factor(sex, levels = c("M","F"))) %>% 
  mutate(age = factor(age, levels = c("F","Y","A"))) %>% 
  mutate(date = floor_date(timestamp,unit = "day"),.after= timestamp) %>% 
  mutate(interval_id = NA)

###############################################################################
#   Summary of Data                                                         ####
#      Mortality table                                                      ####

# Summary of mortality table
summary_mortality <-   
  table_mortality_original %>% 
  group_by(StudyArea,Sex,EstAge) %>% 
  summarise(n.indiv = n(),
            alive = sum(!is.na(MortDate)),
            dead = sum(is.na(MortDate))) %>% 
  arrange(StudyArea,Sex, desc(EstAge),.by_group = T)

summary_mortality

#      Collar locs                                                          ####

# Summary of relevant collar information
summary_locs <- locs_deer %>% 
  group_by(site,sex,age) %>% 
  summarise(n.indiv = n_distinct(individual.local.identifier)) %>% 
  arrange(desc(age),.by_group = TRUE)

summary_locs

#      Inclusion Tables                                                     ####

ids_mort <- unique(table_mortality_original$individual.local.identifier)
ids_locs <- unique(locs_deer$individual.local.identifier)

inclusion_table_locs <- data.frame(individual.local.identifier = ids_locs) %>% 
  mutate(inclusion_locs = ifelse(ids_locs %in% ids_mort,1,0))

inclusion_table_mort <- data.frame(individual.local.identifier = ids_mort) %>% 
  mutate(inclusion_mort = ifelse(ids_mort %in% ids_locs,1,0))

inclusion_table_final <- merge(x=inclusion_table_locs,
                   y=inclusion_table_mort,
                   by="individual.local.identifier",
                   all=TRUE) %>% 
  mutate(valid_data = ifelse(inclusion_locs == 1 & inclusion_mort == 1,
                             T,F))

#      TimeOverlap                                                          ####

temporal_overlap <- locs_deer %>% 
  group_by(individual.local.identifier) %>% 
  summarize(min_date = min(timestamp),
            max_date = max(timestamp)) %>% 
  left_join(table_mortality_original, by='individual.local.identifier') %>% 
  select(c(individual.local.identifier,min_date,max_date,MortDate)) %>%
  mutate(temp_overlap = ifelse(MortDate >= min_date & 
                                 MortDate <= max_date,T,F))
  

###############################################################################
#   Data Cleaning: Collar Data                                              ####
#      Adding Intervals to Data                                             ####

# Creating an empty data fame
locs_interval_df <- locs_deer
locs_interval_df <- locs_interval_df[FALSE,]

# Deer ID list
list_deerIDs <- unique(locs_deer$individual.local.identifier)

for (i in 1:length(list_deerIDs)) {
  
  # Filtering deer
  loc_subset <- locs_deer %>% 
    filter(individual.local.identifier == list_deerIDs[[i]])
  
  # Extracting last date 
  max_date  <- max(loc_subset$timestamp) %>% floor_date(unit = "day")
  min_date <- min(loc_subset$timestamp) %>% floor_date(unit = "day")
  
  # Establishing Initial Settings
  init_date <- max_date
  init_id <- 1
  
  while (init_date >= min_date) {
    tmp_max_date <- init_date
    tmp_min_date <- tmp_max_date - ddays(6)
    
    date_seq <- seq(tmp_min_date,tmp_max_date, by = 'day')
    
    tmp_locs <- loc_subset %>% 
      filter(date %in% date_seq) %>% 
      mutate(interval_id = init_id)
    
    locs_interval_df <- rbind(locs_interval_df,tmp_locs)
    
    # Updating settings for next iteration
    init_date <- tmp_min_date - ddays(1)
    init_id <- init_id + 1
  }
  print(paste0(i," out of ",length(list_deerIDs)," completed"))
}

#      Adding Ordered Intervals                                             ####

# List of individuals
list_deerIDs <- unique(locs_interval_df$individual.local.identifier)

# Foreach loop to order intervals
ordered_locs <- foreach(i = 1:length(list_deerIDs),
                        .combine = bind_rows) %do% {
                          
                          loc_subset <- locs_interval_df %>% 
                            filter(individual.local.identifier == list_deerIDs[[i]])
                          
                          classification_table <- data.frame(interval_id = unique(loc_subset$interval_id),
                                                             ordered_int_id  = sort(unique(loc_subset$interval_id),decreasing = T))
                          
                          ordered <- merge(x=loc_subset,y=classification_table,by="interval_id",all.x=TRUE)
                          print(i/length(list_deerIDs))
                          return(ordered)
                        } %>% 
  select(-c(interval_id)) %>% 
  arrange(timestamp)

#      Adding time periods for cox-models                                   ####

# List of individuals
list_deerIDs <- unique(ordered_locs$individual.local.identifier)

# Establishing anchor date
anchor_date <- min(ordered_locs$timestamp) %>% floor_date(unit = "day")

# Protocol to establish t_start and t_end
locs_t_periods <- foreach(i = 1:length(list_deerIDs),
                          .combine = bind_rows) %do% {
                            
                            # Subsetting an individual 
                            sub <- ordered_locs %>% 
                              filter(individual.local.identifier == list_deerIDs[[i]])
                            
                            # Extracting unique intervals for the individual
                            n_intervals <- unique(sub$ordered_int_id)
                            
                            # Assigning t_start and t_end for each
                            locs_t_intervals <- foreach(j = 1:length(n_intervals),
                                                        .combine = bind_rows) %do% {
                                                          sub_2 <- sub %>% filter(ordered_int_id == n_intervals[[j]])
                                                          
                                                          t_ints <- sub_2 %>% 
                                                            mutate(t_start = studyday_calc(min(timestamp),anchor_date = anchor_date),
                                                                   t_end = studyday_calc(max(timestamp),anchor_date = anchor_date))
                                                          
                                                          return(t_ints)
                                                        }
                            
                            # Iteration Tracker
                            print(i/length(list_deerIDs))
                            
                            # Output
                            return(locs_t_intervals)
                          }

###############################################################################
#   Data Cleaning: Mortality Table                                          ####

# Cleaning Mortality Table
mortality_table <- table_mortality_original %>% 
  mutate(status = ifelse(!(is.na(MortDate)) | !(is.na(HarvestDate)),1,0)) %>% 
  select(individual.local.identifier,MortDate,HarvestDate,Harvest,status) %>% 
  mutate(Harvest = ifelse(Harvest == "Y",1,0)) %>% 
  filter(status == 1) %>% 
  mutate(MortDate = ymd(as.character(MortDate)),
         HarvestDate =ymd(as.character(HarvestDate))) %>% 
  mutate(event_date  = ifelse(!(is.na(MortDate)),as.character(MortDate),2))

# Natural Mortalities
natmort <- mortality_table %>% 
  filter(!(is.na(MortDate))) %>% 
  select(c(individual.local.identifier,Harvest,MortDate,status)) %>% 
  rename(death_date = MortDate)

# Harvest Mortalities
harvmort <- mortality_table %>% 
  filter(!(is.na(HarvestDate))) %>% 
  select(c(individual.local.identifier,HarvestDate,Harvest,status)) %>% 
  rename(death_date = HarvestDate)

# Final Mort Table
mortality_table <- full_join(harvmort,natmort)

###############################################################################
#   Data Cleaning: Final Location Data                                      ####
#      Adding Mortality information to relevant locs                        ####

# List of individuals
list_deerIDs <- unique(locs_t_periods$individual.local.identifier)
list_deerIDs_mort <-  unique(mortality_table$individual.local.identifier)
list_deerIDs_inclusion <- intersect(list_deerIDs,list_deerIDs_mort)

# Adding information to mortalities
options(dplyr.summarise.inform = FALSE)

locs_mortalities <- foreach(i = 1:length(list_deerIDs_inclusion),
                            .combine = bind_rows) %do% {
                              
                              indiv <- list_deerIDs_inclusion[[i]]
                              
                              mort <- filter(mortality_table,
                                             individual.local.identifier == indiv)
                              
                              locs <- filter(locs_t_periods,
                                             individual.local.identifier == indiv)
                              
                              mort_t_periods <- locs %>% 
                                mutate(event = ifelse(mort$death_date >= date &
                                                        mort$death_date <= date,1,0)) %>% 
                                select(individual.local.identifier,ordered_int_id,t_start,t_end,event) %>% 
                                group_by(individual.local.identifier,t_start,t_end) %>% 
                                summarise(event = max(event))
                              
                              # Iteration Tracker
                              print(i/length(list_deerIDs_inclusion))
                              
                              return(mort_t_periods)
                            } 
options(dplyr.summarise.inform = T)

#      Adding mortalities to loc df                                         ####

locs_final <- locs_t_periods %>% 
  left_join(y = locs_mortalities, 
            by = NULL) %>% mutate(event = ifelse(is.na(event),0,event))

#      Export Locs                                                          ####

write_csv(locs_final,
          "1.DataManagement/ch2_data/clean/locs/deer_mortalitylocs.csv")

###############################################################################
#   Producing revised mort tables                                           ####
#      Data Import Mort                                                     ####

# Importing Mortality table
table_mortality_original <- read_xlsx("1.DataManagement\\ch2_data\\dbo_tblMortality.xlsx") %>% 
  filter(StudyArea %in% c("N","S")) %>% 
  filter(EstAge %in% c("F","Y","A")) %>% 
  mutate(StudyArea = factor(StudyArea, levels = c("N","S"))) %>%
  mutate(Sex = factor(Sex, levels = c("M","F"))) %>% 
  mutate(EstAge = factor(EstAge, levels = c("F","Y","A"))) %>% 
  rename(individual.local.identifier = DeerID)

# Cleaning Mortality Table
mortality_table <- table_mortality_original %>% 
  mutate(status = ifelse(!(is.na(MortDate)) | !(is.na(HarvestDate)),1,0)) %>% 
  select(individual.local.identifier,MortDate,HarvestDate,Harvest,status) %>% 
  mutate(Harvest = ifelse(Harvest == "Y",1,0)) %>% 
  filter(status == 1) %>% 
  mutate(MortDate = ymd(as.character(MortDate)),
         HarvestDate =ymd(as.character(HarvestDate))) %>% 
  mutate(event_date  = ifelse(!(is.na(MortDate)),as.character(MortDate),2))

# Natural Mortalities
natmort <- mortality_table %>% 
  filter(!(is.na(MortDate))) %>% 
  select(c(individual.local.identifier,Harvest,MortDate,status)) %>% 
  rename(death_date = MortDate)

# Harvest Mortalities
harvmort <- mortality_table %>% 
  filter(!(is.na(HarvestDate))) %>% 
  select(c(individual.local.identifier,HarvestDate,Harvest,status)) %>% 
  rename(death_date = HarvestDate)

# Final Mort Table
mortality_table <- full_join(harvmort,natmort)

#      Data Import Loc                                                      ####
locs_mort <- read_csv("1.DataManagement/ch2_data/clean/locs/deer_mortalitylocs.csv")

#      Producing New mortality tables                                       ####

mortality_table_comp <- left_join(mortality_table,
                                  table_mortality_original,
                                  by = "individual.local.identifier") %>% 
  select(c(individual.local.identifier,
           death_date,
           MortDate,
           HarvestDate,
           RecovDate,
           GPSOff))

last_loc <-locs_mort %>% group_by(individual.local.identifier) %>% 
  summarize(last_loc = max(timestamp))

last_loc_event <-locs_mort %>% group_by(individual.local.identifier) %>% 
  slice_tail() %>% 
  select(individual.local.identifier,age,sex,event)

mortality_table_comp_v2 <- mortality_table_comp %>% 
  left_join(last_loc,by = "individual.local.identifier") %>% 
  left_join(last_loc_event,by = "individual.local.identifier") %>% 
  arrange(individual.local.identifier)

write_csv(mortality_table_comp_v2,
          "1.DataManagement/ch2_data/mort_tables/mort_tbl_v2.csv")

mortality_table_comp_v2$death_datePOS <- mortality_table_comp_v2$death_date %>% 
  paste0(" 23:59:59") %>% 
  ymd_hms()

mortality_table_comp_v3 <- mortality_table_comp_v2 %>% 
  mutate(date_diff = as.double(difftime(death_datePOS,last_loc, units = c("days")))) %>% 
  mutate(event = ifelse(date_diff <= 4, 1,0))

write_csv(mortality_table_comp_v3,
          "1.DataManagement/ch2_data/mort_tables/mort_tbl_v3.csv")

mortality_table_comp_v3 %>% 
  mutate(site = ifelse(str_detect(individual.local.identifier, "N"),"north","south")) %>% 
  group_by(site,age,sex) %>% 
  summarise(n_deaths = sum(event)) %>% 
  na.omit()

###############################################################################