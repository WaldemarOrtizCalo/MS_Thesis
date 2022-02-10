#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-01-10 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(dplyr)

#      Functions                                                            ####

#      Data                                                                 ####
df <- read.csv("1.DataExploration/deer_all_clean.csv")

###############################################################################
#   [Data Cleaning]                                                         ####
#      [Age]                                                                ####

df$age <- case_when(df$age == "F"  ~ "F",
                    df$age == "Y"  ~ "Y",
                    df$age == "A"  ~ "A",
                    df$age == "A1" ~ "A",
                    df$age == "A2" ~ "A",
                    df$age == "A3" ~ "A")

#      [Study Area]                                                         ####

# For the Study, here are the real classifications
# North = Glaciated Plains
# South = Ozark Region
# Southeat = Adjacent Cropland Study 

df$site <- case_when(df$site == "North" ~ "North",
                     df$site == "South" ~ "South",
                     df$site == "Southeast" ~ "CroplandStudy")


#      [Age Dataframes]                                                     ####

df_fawn       <- subset(df,df$age == "F")
df_yearling   <- subset(df,df$age == "Y")
df_adult      <- subset(df,df$age == "A")
 
#      [Sex Dataframes]                                                     ####

df_male   <- subset(df,df$sex == "M")
df_female <- subset(df,df$sex == "F")

###############################################################################
#   [Summarize]                                                             ####
#      [Prepping Dataframe]                                                 ####

# Creating new Dataframe
df_all <- df

# Making Sex a Factor
df_all$sex <- factor(df_all$sex, levels = c("M","F"))

# Making Age a Factor
df_all$age <- factor(df_all$age, levels = c("A","Y","F"))

#        [Number of Relocations by Site, Sex, and Age]                      ####

data_summary <- df_all %>% 
  group_by(site,sex,age) %>% 
  summarise("n_indiv"=n_distinct(id),"n_loc"= n()) %>% 
  mutate("avg_loc"= n_loc / n_indiv)



