#   Testing with own data                                                       ####

data_sub <- data_final 

id_sub <- unique(data_final$individual.local.identifier) %>% .[1:200]

data_sub <- data_sub[data_sub$individual.local.identifier %in% id_sub, ]

#   Frequentist                                                                 ####
# Creating Survival Object
surv_object <- Surv(time = data_sub$t_start,
                    time2 = data_sub$t_end,
                    event = data_sub$event,
                    type = "counting")

# model
cox_north <- coxph(surv_object ~ lsi + contag + shdi, 
                   data = data_sub,
                   na.action = "na.fail")
summary(cox_north)

#   Poisson                                                                     ####

brm_coxph_test <- brm(
  formula = event ~ as.factor(t_end) + lsi + contag + shdi,
  family = poisson(),
  prior = set_prior('normal(0, 4)', class = 'b'), 
  data = data_sub, chains = 3, cores = 3, iter = 2000)



est <- posterior_summary(brm_coxph_test)[c("b_lsi","b_contag","b_shdi") , c("Estimate","Est.Error")]

launch_shinystan(brm_coxph_test)


#   [Trial]                                                                 ####
# Library
library(survival)
library(brms)
library(tidybayes)
#   Data                                                                 ####
df <- as_tibble(leukemia) %>% 
  mutate(id = seq_len(n())) 

#choose cut points
#events only + the last one. 
lasttime <- max(df$time) 
events <- df %>% 
  filter(status == 1) %>% 
  pull(time) %>% unique() %>% sort
cut_events <- c(events, lasttime)

#make long. Want time start/stop, duration, status, covariates. 
df_long <- survSplit(
  formula = Surv(time, status) ~ ., 
  data = df, 
  cut = cut_events) %>%
  rename(tstop = time) %>% 
  mutate(tduration = tstop - tstart)

head(df_long)

#   Cox Proportional Hazards Model                                                                 ####
fit_cox_short <- coxph(Surv(time, status)~x, 
                       data = df)
fit_cox_long <- coxph(Surv(tstart, tstop, status)~x, 
                      data = df_long)

#   Poisson Model in brsm ####

#a_j is a factor
fit_BRM_factor <- brm(
  formula = status ~ as.factor(tstop) + x + offset(log(tduration)),
  family = poisson(),
  prior = set_prior('normal(0, 4)', class = 'b'), 
  data = df_long, chains = 3, cores = 3, iter = 2000)

#a_j is a smooth function that changes over time
#number of knots will be length of cutpoints
fit_BRM_spline <- brm(
  formula = status ~ s(tstop, k = length(cut_events)) + x + offset(log(tduration)),
  family = poisson(),
  prior = set_prior('normal(0, 4)', class = 'b'), 
  data = df_long, chains = 3, cores = 3, iter = 2000)

#baseline hazard, time = factor
haz_timefactor <- tidy_draws(fit_BRM_factor) %>% 
  select(b_Intercept:b_as.factortstop161) %>% 
  mutate_at(vars(contains('as.factor')), .funs = function(x) x + .$b_Intercept) %>% 
  mutate(draw = seq_len(n())) %>% 
  rename(b_as.factortstop5 = b_Intercept) %>% 
  pivot_longer(contains('b_as.factor'), names_prefix = 'b_as.factortstop') %>% 
  mutate(time = as.integer(name))

#plot for time = factor
ggplot(haz_timefactor, aes(as.factor(time), exp(value))) +
  geom_jitter(width = .1, height = 0, alpha = .01) +
  #geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Hazard for time = factor',
       x = 'time interval (j)', 
       y = expression(paste('baseline hazard (', lambda['j'], ')') ))
