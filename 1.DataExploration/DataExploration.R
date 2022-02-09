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
#   [Number of Unique Adults]                                               ####

#      [North and South]                                                    ####
df_adult %>% 
  subset(df_adult$site == "North" | df_adult$site == "South") %>% 
  pull(id) %>% 
  unique %>% 
  length

#      [North]                                                              ####

n_adult_total_North <- df_adult %>% 
  subset(df_adult$site == "North") %>% 
  pull(id) %>% 
  unique %>% 
  length %>% 
  as.numeric()

#        [Male]                                                             ####

n_adult_male_North <- df_adult %>% 
  subset(df_adult$site == "North" & df_adult$sex == "M") %>% 
  pull(id) %>% 
  unique %>% 
  length %>% 
  as.numeric()

#        [Female]                                                           ####

n_adult_female_North <-df_adult %>% 
  subset(df_adult$site == "North" & df_adult$sex == "F") %>% 
  pull(id) %>% 
  unique %>% 
  length %>% 
  as.numeric()

#      [South]                                                              ####

n_adult_total_South <- df_adult %>% 
  subset(df_adult$site == "South") %>% 
  pull(id) %>% 
  unique %>% 
  length %>% 
  as.numeric()

#        [Male]                                                             ####

n_adult_male_South <- df_adult %>% 
  subset(df_adult$site == "South" & df_adult$sex == "M") %>% 
  pull(id) %>% 
  unique %>% 
  length %>% 
  as.numeric()

#        [Female]                                                           ####

n_adult_female_South <-df_adult %>% 
  subset(df_adult$site == "South" & df_adult$sex == "F") %>% 
  pull(id) %>% 
  unique %>% 
  length %>% 
  as.numeric()

#      [Cropland]                                                           ####

df_adult %>% 
  subset(df_adult$site == "CroplandStudy") %>% 
  pull(id) %>% 
  unique %>% 
  length


#   [Number of Adult Relocations]                                           ####

#      [North]                                                              ####
#        [Male]                                                             ####

nloc_adult_male_North <- df_adult %>% 
  subset(df_adult$site == "North" & df_adult$sex == "M") %>% 
  pull(id) %>% 
  length %>% 
  as.numeric()

#        [Female]                                                           ####

nloc_adult_female_North <- df_adult %>% 
  subset(df_adult$site == "North" & df_adult$sex == "F") %>% 
  pull(id) %>% 
  length %>% 
  as.numeric()

#      [South]                                                              ####
#        [Male]                                                             ####

nloc_adult_male_South <- df_adult %>% 
  subset(df_adult$site == "South" & df_adult$sex == "M") %>% 
  pull(id) %>% 
  length %>% 
  as.numeric()

#        [Female]                                                           ####

nloc_adult_female_South <- df_adult %>% 
  subset(df_adult$site == "South" & df_adult$sex == "F") %>% 
  pull(id) %>% 
  length %>% 
  as.numeric()

#      [CroplandStudy]                                                      ####

nrow(subset(df_adult,df_adult$site == "CroplandStudy"))

#   [Average Adult Relocations]                                           ####

#      [North]                                                              ####
#        [Male]                                                              ####

avgloc_adult_male_North <- df_adult %>% 
  subset(df_adult$site == "North" & df_adult$sex == "M") %>% 
  group_by(id) %>%
  count(id) %>% 
  pull(n) %>% 
  mean

#        [Female]                                                              ####

avgloc_adult_female_North <- df_adult %>% 
  subset(df_adult$site == "North" & df_adult$sex == "F") %>% 
  group_by(id) %>%
  count(id) %>% 
  pull(n) %>% 
  mean

#      [South]                                                              ####

#        [Male]                                                              ####

avgloc_adult_male_South <- df_adult %>% 
  subset(df_adult$site == "South" & df_adult$sex == "M") %>% 
  group_by(id) %>%
  count(id) %>% 
  pull(n) %>% 
  mean

#        [Female]                                                              ####

avgloc_adult_female_South <- df_adult %>% 
  subset(df_adult$site == "South" & df_adult$sex == "F") %>% 
  group_by(id) %>%
  count(id) %>% 
  pull(n) %>% 
  mean
###############################################################################
