#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-11-21 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(sf)
library(terra)
library(adehabitatHR)
library(foreach)

#      Functions                                                            ####

#      Data                                                                 ####

#        Deer Locations                                                     ####

deer_all <- read_csv("1.DataManagement/CleanData/deer_all_revised.csv") 

deer_all %>% group_by(sex,age, .add = T) %>% 
  summarise(n = n())

###############################################################################
#   North                                                                   ####
#      Data Import                                                          ####
#        Deer locations                                                     ####

deer <- deer_all %>% filter(site == "North")

#      Sampling Availability                                                ####
sex_list <- unique(deer$sex)
season_list <- unique(deer$season)

foreach(i = 1:length(sex_list), .errorhandling = "pass") %do% {
  foreach(j = 1:length(season_list), .errorhandling = "pass") %do% {
    
    # Subsetting Deer Locs
    deer_sub <- deer %>% 
      filter(sex == sex_list[i]) %>% 
      filter(season == season_list[j])
    
    # Making the data a spatial object
    spdf <- deer_sub
    coordinates(spdf) <- ~location.long + location.lat
    proj4string(spdf) <- CRS("+init=epsg:5070")
    
    # Creating and exporting the kernel density estimate
    kde <- kernelUD(spdf, h = "href")
    kde_UD <- getverticeshr(kde, 95)%>% 
      st_as_sf() %>% 
      st_write(paste0("1.DataManagement/ch1_data/north/kde/north_kde_",sex_list[i],"_",season_list[j],".shp"),
               append=FALSE,
               quiet = T)
    
    # Sampling available points
    avail <- st_sample(x = kde_UD,
                       size = 5*nrow(deer_sub)) %>% 
      do.call(rbind, .) %>% 
      as.data.frame() %>% 
      mutate(choice = 0, .before = 1) %>% 
      rename(location.long = V1,location.lat = V2) %>% 
      st_as_sf(coords = c("location.long","location.lat"),crs = 5070) %>% 
      mutate(individual.local.identifier = rep(deer_sub$individual.local.identifier,each = 5),.before = 1) %>%
      mutate(sex = sex_list[i],.after = individual.local.identifier) %>% 
      mutate(season = season_list[j],.after = sex) %>% 
      mutate(year = rep(deer_sub$year,each = 5),.after = season) %>%
      mutate(age = rep(deer_sub$age,each = 5),.after = sex) %>% 
      mutate(site = "North",.after = age)
    
    # Designating used points
    used <- deer_sub %>% 
      mutate(choice = 1) %>% 
      st_as_sf(coords = c("location.long","location.lat"),crs = 5070)
    
    # Joining used and available
    final <- bind_rows(used,avail)
    
    # Exporting used-available dataset
    write_csv(x = final,
              file = paste0("1.DataManagement/ch1_data/north/used_available/northdata_",
                            sex_list[i],
                            "_",
                            season_list[j],
                            ".csv"))
    print(paste0(sex_list[i],":",season_list[j]," Completed"))
  }
}

#        Expanded Grid for Male Summer                                      ####

i <- 1
j <- 3

# Subsetting Deer Locs
deer_sub <- deer %>% 
  filter(sex == sex_list[i]) %>% 
  filter(season == season_list[j])

# Making the data a spatial object
spdf <- deer_sub
coordinates(spdf) <- ~location.long + location.lat
proj4string(spdf) <- CRS("+init=epsg:5070")

# Creating and exporting the kernel density estimate
xmin <- spdf@bbox[1,1] - (30*1000)
xmax <- spdf@bbox[1,2] + (30*1000)
ymin <- spdf@bbox[2,1] - (30*1000)
ymax <- spdf@bbox[2,2] + (30*1000)

x <- seq(xmin, xmax, by=30) 
y <- seq(ymin, ymax, by=30)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

kde <- kernelUD(spdf, h = "href", grid = xy)
kde_UD <- getverticeshr(kde, 95)%>% 
  st_as_sf() %>% 
  st_write(paste0("1.DataManagement/ch1_data/north/kde/north_kde_",sex_list[i],"_",season_list[j],".shp"),
           append=FALSE,
           quiet = T)

# Sampling available points
avail <- st_sample(x = kde_UD,
                   size = 5*nrow(deer_sub)) %>% 
  do.call(rbind, .) %>% 
  as.data.frame() %>% 
  mutate(choice = 0, .before = 1) %>% 
  rename(location.long = V1,location.lat = V2) %>% 
  st_as_sf(coords = c("location.long","location.lat"),crs = 5070) %>% 
  mutate(individual.local.identifier = rep(deer_sub$individual.local.identifier,each = 5),.before = 1) %>%
  mutate(sex = sex_list[i],.after = individual.local.identifier) %>% 
  mutate(season = season_list[j],.after = sex) %>% 
  mutate(year = rep(deer_sub$year,each = 5),.after = season) %>%
  mutate(age = rep(deer_sub$age,each = 5),.after = sex) %>% 
  mutate(site = "North",.after = age)

# Designating used points
used <- deer_sub %>% 
  mutate(choice = 1) %>% 
  st_as_sf(coords = c("location.long","location.lat"),crs = 5070)

# Joining used and available
final <- bind_rows(used,avail)

# Exporting used-available dataset
write_csv(x = final,
          file = paste0("1.DataManagement/ch1_data/north/used_available/northdata_",
                        sex_list[i],
                        "_",
                        season_list[j],
                        ".csv"))

#      Covariate Sampling                                                   ####
#        Covariate Rasters                                                  ####

cov_rasts <- list.files("1.DataManagement/CovRasters_Landscape/north",
                        full.names = T) %>% 
  str_subset("patches",negate = T) %>% 
  str_subset("patcharea",negate = T) %>% rast()


###############################################################################
#   South                                                                   ####
#      Data Import                                                          ####
#        Deer locations                                                     ####

deer <- deer_all %>% filter(site == "South")

#      Sampling Availability                                                ####
sex_list <- unique(deer$sex)
season_list <- unique(deer$season)

foreach(i = 1:length(sex_list), .errorhandling = "pass") %do% {
  foreach(j = 1:length(season_list), .errorhandling = "pass") %do% {
    
    # Subsetting Deer Locs
    deer_sub <- deer %>% 
      filter(sex == sex_list[i]) %>% 
      filter(season == season_list[j])
    
    # Making the data a spatial object
    spdf <- deer_sub
    coordinates(spdf) <- ~location.long + location.lat
    proj4string(spdf) <- CRS("+init=epsg:5070")
    
    # Creating and exporting the kernel density estimate
    kde <- kernelUD(spdf, h = "href")
    kde_UD <- getverticeshr(kde, 95)%>% 
      st_as_sf() %>% 
      st_write(paste0("1.DataManagement/ch1_data/south/kde/south_kde_",sex_list[i],"_",season_list[j],".shp"),
               append=FALSE,
               quiet = T)
    
    # Sampling available points
    avail <- st_sample(x = kde_UD,
                       size = 5*nrow(deer_sub)) %>% 
      do.call(rbind, .) %>% 
      as.data.frame() %>% 
      mutate(choice = 0, .before = 1) %>% 
      rename(location.long = V1,location.lat = V2) %>% 
      st_as_sf(coords = c("location.long","location.lat"),crs = 5070) %>% 
      mutate(individual.local.identifier = rep(deer_sub$individual.local.identifier,each = 5),.before = 1) %>%
      mutate(sex = sex_list[i],.after = individual.local.identifier) %>% 
      mutate(season = season_list[j],.after = sex) %>% 
      mutate(year = rep(deer_sub$year,each = 5),.after = season) %>%
      mutate(age = rep(deer_sub$age,each = 5),.after = sex) %>% 
      mutate(site = "South",.after = age)
    
    # Designating used points
    used <- deer_sub %>% 
      mutate(choice = 1) %>% 
      st_as_sf(coords = c("location.long","location.lat"),crs = 5070)
    
    # Joining used and available
    final <- bind_rows(used,avail)
    
    # Exporting used-available dataset
    write_csv(x = final,
              file = paste0("1.DataManagement/ch1_data/south/used_available/southdata_",
                            sex_list[i],
                            "_",
                            season_list[j],
                            ".csv"))
    print(paste0(sex_list[i],":",season_list[j]," Completed"))
  }
}

#      Covariate Sampling                                                   ####
#        Covariate Rasters                                                  ####

cov_rasts <- list.files("1.DataManagement/CovRasters_Landscape/south",
                        full.names = T) %>% 
  str_subset("patches",negate = T) %>% 
  str_subset("patcharea",negate = T) %>% rast()


###############################################################################
#   Southeast                                                               ####
#      Data Import                                                          ####
#        Deer locations                                                     ####

deer <- deer_all %>% filter(site == "CroplandStudy")

#      Sampling Availability                                                ####
sex_list <- unique(deer$sex)
season_list <- unique(deer$season)

foreach(i = 1:length(sex_list), .errorhandling = "pass") %do% {
  foreach(j = 1:length(season_list), .errorhandling = "pass") %do% {
    
    # Subsetting Deer Locs
    deer_sub <- deer %>% 
      filter(sex == sex_list[i]) %>% 
      filter(season == season_list[j])
    
    # Making the data a spatial object
    spdf <- deer_sub
    coordinates(spdf) <- ~location.long + location.lat
    proj4string(spdf) <- CRS("+init=epsg:5070")
    
    # Creating and exporting the kernel density estimate
    kde <- kernelUD(spdf, h = "href")
    kde_UD <- getverticeshr(kde, 95)%>% 
      st_as_sf() %>% 
      st_write(paste0("1.DataManagement/ch1_data/southeast/kde/southeast_kde_",sex_list[i],"_",season_list[j],".shp"),
               append=FALSE,
               quiet = T)
    
    # Sampling available points
    avail <- st_sample(x = kde_UD,
                       size = 5*nrow(deer_sub)) %>% 
      do.call(rbind, .) %>% 
      as.data.frame() %>% 
      mutate(choice = 0, .before = 1) %>% 
      rename(location.long = V1,location.lat = V2) %>% 
      st_as_sf(coords = c("location.long","location.lat"),crs = 5070) %>% 
      mutate(individual.local.identifier = rep(deer_sub$individual.local.identifier,each = 5),.before = 1) %>%
      mutate(sex = sex_list[i],.after = individual.local.identifier) %>% 
      mutate(season = season_list[j],.after = sex) %>% 
      mutate(year = rep(deer_sub$year,each = 5),.after = season) %>%
      mutate(age = rep(deer_sub$age,each = 5),.after = sex) %>% 
      mutate(site = "Southeast",.after = age)
    
    # Designating used points
    used <- deer_sub %>% 
      mutate(choice = 1) %>% 
      st_as_sf(coords = c("location.long","location.lat"),crs = 5070)
    
    # Joining used and available
    final <- bind_rows(used,avail)
    
    # Exporting used-available dataset
    write_csv(x = final,
              file = paste0("1.DataManagement/ch1_data/southeast/used_available/southeastdata_",
                            sex_list[i],
                            "_",
                            season_list[j],
                            ".csv"))
    print(paste0(sex_list[i],":",season_list[j]," Completed"))
  }
}

#      Covariate Sampling                                                   ####
#        Covariate Rasters                                                  ####

cov_rasts <- list.files("1.DataManagement/CovRasters_Landscape/southeast",
                        full.names = T) %>% 
  str_subset("patches",negate = T) %>% 
  str_subset("patcharea",negate = T) %>% rast()


###############################################################################
#   [Expanded grid version]                                                 ####
# Subsetting Deer Locs
deer_sub <- deer %>% 
  filter(sex == sex_list[i]) %>% 
  filter(season == season_list[j])

# Making the data a spatial object
spdf <- deer_sub
coordinates(spdf) <- ~location.long + location.lat
proj4string(spdf) <- CRS("+init=epsg:5070")

# Creating and exporting the kernel density estimate
xmin <- spdf@bbox[1,1] - (30*100)
xmax <- spdf@bbox[1,2] + (30*100)
ymin <- spdf@bbox[2,1] - (30*100)
ymax <- spdf@bbox[2,2] + (30*100)

x <- seq(xmin, xmax, by=30) 
y <- seq(ymin, ymax, by=30)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

kde <- kernelUD(spdf, h = "href", grid = xy)
kde_UD <- getverticeshr(kde, 95)%>% 
  st_as_sf() %>% 
  st_write(paste0("1.DataManagement/ch1_data/north/kde/north_kde_",sex_list[i],"_",season_list[j],".shp"),
           append=FALSE)

print(Sys.time())
# Sampling available points
avail <- st_sample(x = kde_UD,
                   size = 5*nrow(deer_sub)) %>% 
  do.call(rbind, .) %>% 
  as.data.frame() %>% 
  mutate(choice = 0, .before = 1) %>% 
  rename(location.long = V1,location.lat = V2) %>% 
  st_as_sf(coords = c("location.long","location.lat"),crs = 5070) %>% 
  mutate(individual.local.identifier = rep(deer_sub$individual.local.identifier,each = 5),.before = 1) %>%
  mutate(sex = sex_list[i],.after = individual.local.identifier) %>% 
  mutate(season = season_list[j],.after = sex) %>% 
  mutate(year = rep(deer_sub$year,each = 5),.after = season) %>%
  mutate(age = rep(deer_sub$age,each = 5),.after = sex) %>% 
  mutate(site = "North",.after = age)

# Designating used points
used <- deer_sub %>% 
  mutate(choice = 1) %>% 
  st_as_sf(coords = c("location.long","location.lat"),crs = 5070)

# Joining used and available
final <- bind_rows(used,avail)

# Exporting used-available dataset
write_csv(x = final,
          file = paste0("1.DataManagement/ch1_data/north/used_available/northdata_",
                        sex_list[i],
                        "_",
                        season_list[j],
                        ".csv"))
print(paste0(sex_list[i],":",season_list[j]," Completed"))
###############################################################################