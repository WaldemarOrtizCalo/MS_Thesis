library(readxl)
library(tidyverse)
library(lubridate)

#   Data Import Mort                                                                 ####

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
#   Data Import Loc                                                                  ####
locs_mort <- read_csv("1.DataManagement/ch2_data/clean/locs/deer_mortalitylocs.csv")


###############################################################################


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
          "D:/Drive/Work/UMontana/2.INPROGRESS/1.Masters/MS_Thesis_Analysis/mort_tbl_v2.csv")

mortality_table_comp_v2$death_datePOS <- mortality_table_comp_v2$death_date %>% 
  paste0(" 23:59:59") %>% 
  ymd_hms()

mortality_table_comp_v3 <- mortality_table_comp_v2 %>% 
  mutate(date_diff = as.double(difftime(death_datePOS,last_loc, units = c("days")))) %>% 
  mutate(event = ifelse(date_diff <= 4, 1,0))

write_csv(mortality_table_comp_v3,
          "D:/Drive/Work/UMontana/2.INPROGRESS/1.Masters/MS_Thesis_Analysis/mort_tbl_v3.csv")

mortality_table_comp_v3 %>% 
  mutate(site = ifelse(str_detect(individual.local.identifier, "N"),"north","south")) %>% 
  group_by(site,age,sex) %>% 
  summarise(n_deaths = sum(event)) %>% 
  na.omit()
