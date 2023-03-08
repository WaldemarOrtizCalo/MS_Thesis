for (i in 1:length(southeast_models)) {

# Subsetting Model
model <- readRDS(southeast_models[[i]])
data <- model$data
name <- southeast_names[[i]]
season <- str_remove(name,"southeastmodel_F_")
cov_list <- names(data)[-1] %>% 
  str_subset("individual.local.identifier",negate = T)

# Covariate Loop
for (v in 1:length(cov_list)) {

# Seperating Covariate for Viz
cov <- cov_list %>% 
  .[v]

cov_name <- cov_list %>% 
  .[v] %>% str_remove("southeast_")

# Effect Plot Data
plot_data <- effect_plot(model, 
                         pred = !!cov, 
                         interval = TRUE, 
                         plot.points = F,
                         x.label = cov,
                         y.label = "Probability of Use") %>% 
  .$data

write_csv(plot_data,
          paste0("2.Chapter1/3.Output/visualizations_bayesian_randomint/southeast/effect_plots_rawdata/",
                 "southeast/",
                 cov_name,"_",season,"_effectdata.csv"))
}}




