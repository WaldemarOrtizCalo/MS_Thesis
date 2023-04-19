
df <- mort_data %>% 
  mutate(age = factor(age, levels = c("F","Y","A")),
         sex = factor(sex, levels = c("F","M")),
         year = as.character(year))

loc_sum <- df %>% 
  group_by(site,sex,age) %>% 
  summarize(n())

length(unique(mort_data$individual.local.identifier))

deer_sum <- mort_data %>% 
  group_by(site,sex,age) %>% 
  summarize(n_distinct(individual.local.identifier))


mort_data %>% 
  group_by(site) %>% 
  summarize(n())

df %>% 
  group_by(site,sex,age) %>% 
  summarize(n_distinct(individual.local.identifier))
 