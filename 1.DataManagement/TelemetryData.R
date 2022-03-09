#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-02-18 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####

library(tidyverse)
library(lubridate)

#      Functions                                                            ####

#      Data                                                                 ####

#        [Deer Data]                                                        ####

df <- read_csv("1.DataManagement/RawData/csv_files/deer_all_clean.csv")

###############################################################################
#   [Data Cleaning]                                                         ####
#      [Age]                                                                ####

df$age <- case_when(df$age == "F"  ~ "F",
                    df$age == "Y"  ~ "Y",
                    df$age == "A"  ~ "A",
                    df$age == "A1" ~ "A",
                    df$age == "A2" ~ "A",
                    df$age == "A3" ~ "A")

df$age <- factor(df$age, levels = c("A","Y","F"))

#      [Sex]                                                                ####

# Making Sex a Factor
df$sex <- factor(df$sex, levels = c("M","F"))

#      [Study Area]                                                         ####

# For the Study, here are the real classifications
# North = Glaciated Plains
# South = Ozark Region
# Southeat = Adjacent Cropland Study 

df$site <- case_when(df$site == "North" ~ "North",
                     df$site == "South" ~ "South",
                     df$site == "Southeast" ~ "CroplandStudy")



#      [Setting Dates as Times Variables]                                   ####
df$t <- ymd_hms(df$t)

#      [Adding more Time Variables]                                         ####

df <- df %>% 
  dplyr::select(-c(id_yr,id_yr_sn)) %>% 
  relocate(year, .after = age)

df <- df %>% relocate(year, .after = t) %>% 
  mutate("month" = month(t,label = T,abbr = T),.after = year) %>% 
  mutate("week" = epiweek(t), .after = month) 

#      [Export]                                                             ####
write_csv(df,"1.DataManagement/CleanData/deer_all_clean.csv")

###############################################################################
#   [Summarize Telemetry Data]                                              ####
#      [Number of Relocations by Site, Sex, and Age]                        ####

data_summary <- df %>% 
  group_by(site,sex,age) %>% 
  summarise("n_indiv"=n_distinct(id),"n_loc"= n()) %>% 
  mutate("avg_loc"= n_loc / n_indiv)


###############################################################################
#   [Livetime Plots]                                                        ####

#      [Creating Dataframe for Time Series]                                 ####

# Creating Time Series Dataframe
df_timeseries <- data.frame("id"   = df$id,
                            "site" = df$site,
                            "sex"  = df$sex,
                            "age"  = df$age,
                            "t"    = df$t)

df_timeseries$t_my <-format(df_timeseries$t,'%m-%Y') %>% my()

#      [Creating livetime function]                                         ####

# Function

lv_function <- function(indivID,df_pop){
  
  # Subsets an individual from the dataframe
  indiv <- subset(df_pop, df$id == indivID)
  
  # Keep unique months and years for an individual
  livetime <- indiv %>% distinct(t_my,.keep_all =T)
  
  return(livetime)
}

# Testing
lv_function("N15001",df_timeseries)

#      [Creating livetime dataframe for all the data]                       ####

# Create list of unique animal IDs
uniqueID <- unique(df_timeseries$id)

# lapply function to create livetime df

livetime_pop <- lapply(uniqueID, lv_function,df_timeseries) %>% 
  do.call(what = rbind,.) %>% 
  arrange(t)

livetime_pop$id <- factor(livetime_pop$id,levels = unique(livetime_pop$id)) 

#      [Population Plot]                                                    ####

ggplot(livetime_pop, aes(x = t_my,y = id,fill = age))+
  geom_point(size = 6, shape = 21)+
  scale_x_date(date_labels = "%b-%Y")+
  theme_bw()+
  xlab("")+
  ggtitle("All Deer")

#        [North Male]                                                       ####

ggplot(subset(livetime_pop,site=="North" & sex == "M"), aes(x = t_my,y = id,fill = age))+
  geom_point(size = 6, shape = 21)+
  scale_x_date(date_labels = "%b-%Y")+
  theme_bw()+
  xlab("")+
  ggtitle("North Male")

#        [North Female]                                                     ####

ggplot(subset(livetime_pop,site=="North" & sex == "F"), aes(x = t_my,y = id,fill = age))+
  geom_point(size = 6, shape = 21)+
  scale_x_date(date_labels = "%b-%Y")+
  theme_bw()+
  xlab("")+
  ggtitle("North Female")

#        [South Male]                                                       ####

ggplot(subset(livetime_pop,site=="South" & sex == "M"), aes(x = t_my,y = id,fill = age))+
  geom_point(size = 6, shape = 21)+
  scale_x_date(date_labels = "%b-%Y")+
  theme_bw()+
  xlab("")+
  ggtitle("South Male")

#        [South Female]                                                     ####

ggplot(subset(livetime_pop,site=="South" & sex == "F"), aes(x = t_my,y = id,fill = age))+
  geom_point(size = 6, shape = 21)+
  scale_x_date(date_labels = "%b-%Y")+
  theme_bw()+
  xlab("")+
  ggtitle("South Female")

#        [CroplandStudy Female]                                             ####

ggplot(subset(livetime_pop,site=="CroplandStudy" & sex == "F"), aes(x = t_my,y = id,fill = age))+
  geom_point(size = 6, shape = 21)+
  scale_x_date(date_labels = "%b-%Y")+
  theme_bw()+
  xlab("")+
  ggtitle("CroplandStudy")

###############################################################################