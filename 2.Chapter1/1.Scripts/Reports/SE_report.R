
#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-03-05 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####

#      Functions                                                            ####

#      Data                                                                 ####

###############################################################################

# Deer Locations
locs_deer <- locs_deer <- read_csv("1.DataManagement/loc_data/deer_all_final_clean.csv") %>% 
  filter(site %in% c("Southeast")) %>% 
  mutate(age = ifelse(age == "A1" | age == "A2" |age == "A3" |age == "A4", "A", age)) %>% 
  mutate(sex = factor(sex, levels = c("M","F"))) %>% 
  mutate(age = factor(age, levels = c("F","Y","A")))

min(locs_deer$timestamp)
max(locs_deer$timestamp)
 
locs_summary <- locs_deer %>% 
  group_by(individual.local.identifier) %>% 
  summarize(nloc = n())

demographic_summary <- locs_deer %>% 
  group_by(sex, age) %>% 
  summarize(indivs = n_distinct(individual.local.identifier))

demographiclocs_summary <- locs_deer %>% 
  group_by(sex, age) %>% 
  summarize(nloc = n())

loctime_summary <- locs_deer %>% 
  group_by(individual.local.identifier) %>% 
  summarize(indivs = n_distinct(individual.local.identifier))

