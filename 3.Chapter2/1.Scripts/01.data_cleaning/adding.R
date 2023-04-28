
locs_mort_raw <- read_csv("1.DataManagement/ch2_data/clean/locs/deer_mortalitylocs.csv")

# Importing Mortality table
n_mort <- mortality_table %>% 
  mutate(site = if_else(str_detect(individual.local.identifier,"N"),"N","S")) %>% 
  filter(site == "N")

id_incl <- intersect(unique(locs_mort_raw$individual.local.identifier),
                     unique(n_mort$individual.local.identifier)) %>% 
  sort()

locs_finalized <- locs_mort_raw

for (i in 1:length(id_incl)) {
  
  # Dataframe cleaning
  id <- id_incl %>% 
    .[[i]]
  
  # Slicing last observation and assinging event status
  event_locs <- locs_finalized %>% 
    filter(individual.local.identifier == id) %>% 
    slice_tail(n = 1) %>% 
    mutate(event = 1) %>% 
    select(individual.local.identifier,ordered_int_id,event)
  
  # Joining
  locs_finalized <- left_join(locs_finalized,event_locs)
  
  print(i)
}

locs_sum <- locs_finalized %>% 
  group_by(individual.local.identifier,site,ordered_int_id) %>% 
  summarize(event = max(event)) %>% 
  filter(site == "North")

sum(locs_sum$event)


i <- 3

# Dataframe cleaning
id <- id_incl %>% 
  .[[i]]

# Slicing last observation and assinging event status
event_locs <- locs_finalized %>% 
  filter(individual.local.identifier == id) %>% 
  slice_tail(n = 1) %>% 
  mutate(event = 1) %>% 
  select(individual.local.identifier,ordered_int_id,event) %>% 
  rename(event2 = event)

# Joining
locs_finalized <- left_join(locs_finalized,event_locs)


mort_locs <- locs_finalized %>%
  group_by(individual.local.identifier) %>% 
  summarize(event = max(event)) %>% 
  filter(event == 1)


setdiff(id_incl,
        unique(mort_locs$individual.local.identifier))

