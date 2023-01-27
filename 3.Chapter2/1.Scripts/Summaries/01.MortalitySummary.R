#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-01-20 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(readxl)

#      Functions                                                            ####

#      Data                                                                 ####
mort_table <- read_xlsx("1.DataManagement\\ch2_data\\dbo_tblMortality.xlsx")

###############################################################################
#   Creating Summary Table                                                  ####

sum <- mort_table %>% 
  group_by(StudyArea,Sex,EstAge) %>% 
  summarise(n()) %>% 
  na.omit() %>%
  filter(StudyArea %in% c("N","S")) %>% 
  filter(EstAge %in% c("NN","F","Y","A")) %>% 
  mutate(StudyArea = factor(StudyArea, levels = c("N","S"))) %>%
  mutate(Sex = factor(Sex, levels = c("M","F"))) %>% 
  mutate(EstAge = factor(EstAge, levels = c("NN","F","Y","A"))) %>% 
  arrange(StudyArea,Sex, desc(EstAge),.by_group = T)

  
write_csv(sum,
          "1.DataManagement\\ch2_data\\MortalitySummary.csv")

###############################################################################
