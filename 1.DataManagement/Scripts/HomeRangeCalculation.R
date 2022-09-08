#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-09-06 

# Purpose: This code is designed to model animal home ranges from the 3 study 
# populations. 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(ctmm)
library(adehabitatHR)
library(tidyverse)
library(move)
library(mapview)
library(sf)
library(foreach)
library(doParallel)

#      Functions                                                            ####

#      Data                                                                 ####

# Deer Data
deer_df <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv") %>% 
  rename(individual.local.identifier = id,
         timestamp = t,
         location.long = x,
         location.lat = y)

###############################################################################
#   [North]                                                                 ####
#      [Data Prep]                                                          ####

# Creating Telemetry Object for the Data 

data <- deer_df %>% 
  filter(site == "North")

data_aggregated <- data

telemetry_object <- move(x=data$location.long, 
                         y=data$location.lat, time=as.POSIXct(data$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), 
                         proj=CRS("+init=epsg:5070"),
                         data=data, 
                         animal=data$individual.local.identifier) %>% 
  as.telemetry()


#      [Aggregated Home Ranges]                                             ####
#        [Creating Dataframe]                                               ####

# Creating SpatialPointsDataFrame
spdf <- data_aggregated
coordinates(spdf) <- ~location.long + location.lat
proj4string(spdf) <- CRS("+init=epsg:5070")

#        [MCP]                                                              ####

# Creating MCP 
mcp <- mcp(spdf, percent=95, 
           unin = c("m"),
           unout = c("km2")) %>% 
  st_as_sf() %>% 
  st_write("1.DataManagement/HomeRangePolygons/north/AggregatedPolygons/mcp_north.shp",
           append=FALSE)

# Verifying Polygon

# mapview(mcp)

#        [KDE]                                                              ####

# Creating KDE
kde <- kernelUD(spdf, h = "href")
kde_UD <- getverticeshr(kde, 95)%>% 
  st_as_sf() %>% 
  st_write("1.DataManagement/HomeRangePolygons/north/AggregatedPolygons/kde_north.shp",
           append=FALSE)

# Verifying Polygon

# mapview(kde_UD)

#      [Individual Home Ranges - All Locations]                             ####
#        [Setting Up Cluster for Parallel Computing]                        ####

# Setting the Settings for the Cluster
myCluster <- makeCluster(4,
                         type = "PSOCK") 

# Registering 
registerDoParallel(myCluster)

# Exporting Packages
clusterEvalQ(myCluster,
             {
               library(ctmm)
               library(tidyverse)
             })

# Exporting data to clusters
clusterExport(cl = myCluster, varlist = c("telemetry_object"), envir = environment())

#        [Home Range Calculation and Export]                                ####

ctmm_modeltypes <- foreach(i = 1:length(telemetry_object), 
                           .errorhandling="pass",
                           .combine = bind_rows) %dopar% {
                             
                             # Subsetting and Individual 
                             indiv <- telemetry_object[[i]]
                             
                             # Creating CTMM fit objects for the smoothing
                             M.IID <- ctmm.fit(indiv) 
                             GUESS <- ctmm.guess(indiv,interactive=FALSE) 
                             M.OUF <- ctmm.fit(indiv,GUESS) 
                             
                             # Home Range Polygons
                             KDE <- akde(indiv,M.IID) 
                             AKDE <- akde(indiv,M.OUF) 
                             
                             # Exporting
                             writeShapefile(object = KDE,
                                            folder = "1.DataManagement/HomeRangePolygons/north/IndividualPolygons",
                                            file=paste0(AKDE@info$identity,"_kde_all"),
                                            overwrite = T)
                             
                             writeShapefile(object = AKDE,
                                            folder = "1.DataManagement/HomeRangePolygons/north/IndividualPolygons",
                                            file=paste0(AKDE@info$identity,"_akde_all"),
                                            overwrite = T)
                             
                             # Model Details
                             model_types <- ctmm.select(indiv, CTMM=GUESS, verbose=TRUE) %>% 
                               summary(fitted.mods) %>% 
                               as.data.frame() %>% 
                               add_column(model = row.names(.), .before = 1) %>% 
                               add_column(ID = indiv@info[[1]], .before = 1)
                             
                             rownames(model_types)<- c()
                             
                             return(model_types)
                           }

# Exporting dataframe with Model info.
ctmm_modeltypes %>% rename(delta_AIC = ΔAICc,
                           delta_RMSPE = "ΔRMSPE (m)") %>% 
  write_csv(file = "1.DataManagement/HomeRangePolygons/north/IndividualPolygons/north_ctmm_models.csv",
            append = F)

#        [Closing Cluster]                                                  ####

# Stopping Cluster
stopCluster(myCluster)
###############################################################################
#   [South]                                                                 ####
#      [Data Prep]                                                          ####

# Creating Telemetry Object for the Data 

data <- deer_df %>% 
  filter(site == "South")

data_aggregated <- data

telemetry_object <- move(x=data$location.long, 
                         y=data$location.lat, time=as.POSIXct(data$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), 
                         proj=CRS("+init=epsg:5070"),
                         data=data, 
                         animal=data$individual.local.identifier) %>% 
  as.telemetry()


#      [Aggregated Home Ranges]                                             ####
#        [Creating Dataframe]                                               ####

# Creating SpatialPointsDataFrame
spdf <- data_aggregated
coordinates(spdf) <- ~location.long + location.lat
proj4string(spdf) <- CRS("+init=epsg:5070")

#        [MCP]                                                              ####

# Creating MCP 
mcp <- mcp(spdf, percent=95, 
           unin = c("m"),
           unout = c("km2")) %>% 
  st_as_sf() %>% 
  st_write("1.DataManagement/HomeRangePolygons/south/AggregatedPolygons/mcp_south.shp",
           append=FALSE)

# Verifying Polygon

# mapview(mcp)

#        [KDE]                                                              ####

# Creating KDE
kde <- kernelUD(spdf, h = "href")
kde_UD <- getverticeshr(kde, 95)%>% 
  st_as_sf() %>% 
  st_write("1.DataManagement/HomeRangePolygons/south/AggregatedPolygons/kde_south.shp",
           append=FALSE)

# Verifying Polygon

# mapview(kde_UD)

#      [Individual Home Ranges - All Locations]                             ####
#        [Setting Up Cluster for Parallel Computing]                        ####

# Setting the Settings for the Cluster
myCluster <- makeCluster(4,
                         type = "PSOCK") 

# Registering 
registerDoParallel(myCluster)

# Exporting Packages
clusterEvalQ(myCluster,
             {
               library(ctmm)
               library(tidyverse)
             })

# Exporting data to clusters
clusterExport(cl = myCluster, varlist = c("telemetry_object"), envir = environment())

#        [Home Range Calculation and Export]                                ####

ctmm_modeltypes <- foreach(i = 1:length(telemetry_object), 
                           .errorhandling="pass",
                           .combine = bind_rows) %dopar% {
                             
                             # Subsetting and Individual 
                             indiv <- telemetry_object[[i]]
                             
                             # Creating CTMM fit objects for the smoothing
                             M.IID <- ctmm.fit(indiv) 
                             GUESS <- ctmm.guess(indiv,interactive=FALSE) 
                             M.OUF <- ctmm.fit(indiv,GUESS) 
                             
                             # Home Range Polygons
                             KDE <- akde(indiv,M.IID) 
                             AKDE <- akde(indiv,M.OUF) 
                             
                             # Exporting
                             writeShapefile(object = KDE,
                                            folder = "1.DataManagement/HomeRangePolygons/south/IndividualPolygons",
                                            file=paste0(AKDE@info$identity,"_kde_all"),
                                            overwrite = T)
                             
                             writeShapefile(object = AKDE,
                                            folder = "1.DataManagement/HomeRangePolygons/south/IndividualPolygons",
                                            file=paste0(AKDE@info$identity,"_akde_all"),
                                            overwrite = T)
                             
                             # Model Details
                             model_types <- ctmm.select(indiv, CTMM=GUESS, verbose=TRUE) %>% 
                               summary(fitted.mods) %>% 
                               as.data.frame() %>% 
                               add_column(model = row.names(.), .before = 1) %>% 
                               add_column(ID = indiv@info[[1]], .before = 1)
                             
                             rownames(model_types)<- c()
                             
                             return(model_types)
                           }

# Exporting dataframe with Model info.
ctmm_modeltypes %>% rename(delta_AIC = ΔAICc,
                           delta_RMSPE = "ΔRMSPE (m)") %>% 
  write_csv(file = "1.DataManagement/HomeRangePolygons/south/IndividualPolygons/south_ctmm_models.csv",
            append = F)

#        [Closing Cluster]                                                  ####

# Stopping Cluster
stopCluster(myCluster)
###############################################################################
#   [Southeast]                                                             ####
#      [Data Prep]                                                          ####

# Creating Telemetry Object for the Data 

data <- deer_df %>% 
  filter(site == "CroplandStudy")

data_aggregated <- data

telemetry_object <- move(x=data$location.long, 
                         y=data$location.lat, time=as.POSIXct(data$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), 
                         proj=CRS("+init=epsg:5070"),
                         data=data, 
                         animal=data$individual.local.identifier) %>% 
  as.telemetry()


#      [Aggregated Home Ranges]                                             ####
#        [Creating Dataframe]                                               ####

# Creating SpatialPointsDataFrame
spdf <- data_aggregated
coordinates(spdf) <- ~location.long + location.lat
proj4string(spdf) <- CRS("+init=epsg:5070")

#        [MCP]                                                              ####

# Creating MCP 
mcp <- mcp(spdf, percent=95, 
           unin = c("m"),
           unout = c("km2")) %>% 
  st_as_sf() %>% 
  st_write("1.DataManagement/HomeRangePolygons/southeast/AggregatedPolygons/mcp_southeast.shp",
           append=FALSE)

# Verifying Polygon

# mapview(mcp)

#        [KDE]                                                              ####

# Creating KDE
kde <- kernelUD(spdf, h = "href")
kde_UD <- getverticeshr(kde, 95)%>% 
  st_as_sf() %>% 
  st_write("1.DataManagement/HomeRangePolygons/southeast/AggregatedPolygons/kde_southeast.shp",
           append=FALSE)

# Verifying Polygon

# mapview(kde_UD)

#      [Individual Home Ranges - All Locations]                             ####
#        [Setting Up Cluster for Parallel Computing]                        ####

# Setting the Settings for the Cluster
myCluster <- makeCluster(4,
                         type = "PSOCK") 

# Registering 
registerDoParallel(myCluster)

# Exporting Packages
clusterEvalQ(myCluster,
             {
               library(ctmm)
               library(tidyverse)
             })

# Exporting data to clusters
clusterExport(cl = myCluster, varlist = c("telemetry_object"), envir = environment())

#        [Home Range Calculation and Export]                                ####

ctmm_modeltypes <- foreach(i = 1:length(telemetry_object), 
                          .errorhandling="pass",
                          .combine = bind_rows) %dopar% {
                            
                            # Subsetting and Individual 
                            indiv <- telemetry_object[[i]]
                            
                            # Creating CTMM fit objects for the smoothing
                            M.IID <- ctmm.fit(indiv) 
                            GUESS <- ctmm.guess(indiv,interactive=FALSE) 
                            M.OUF <- ctmm.fit(indiv,GUESS) 
                            
                            # Home Range Polygons
                            KDE <- akde(indiv,M.IID) 
                            AKDE <- akde(indiv,M.OUF) 
                            
                            # Exporting
                            writeShapefile(object = KDE,
                                           folder = "1.DataManagement/HomeRangePolygons/southeast/IndividualPolygons",
                                           file=paste0(AKDE@info$identity,"_kde_all"),
                                           overwrite = T)
                            
                            writeShapefile(object = AKDE,
                                           folder = "1.DataManagement/HomeRangePolygons/southeast/IndividualPolygons",
                                           file=paste0(AKDE@info$identity,"_akde_all"),
                                           overwrite = T)
                            
                            # Model Details
                            model_types <- ctmm.select(indiv, CTMM=GUESS, verbose=TRUE) %>% 
                              summary(fitted.mods) %>% 
                              as.data.frame() %>% 
                              add_column(model = row.names(.), .before = 1) %>% 
                              add_column(ID = indiv@info[[1]], .before = 1)
                            
                            rownames(model_types)<- c()
                            
                            return(model_types)
                          }

# Exporting dataframe with Model info.
ctmm_modeltypes %>% rename(delta_AIC = ΔAICc,
                           delta_RMSPE = "ΔRMSPE (m)") %>% 
  write_csv(file = "1.DataManagement/HomeRangePolygons/southeast/IndividualPolygons/southeast_ctmm_models.csv",
            append = F)

#        [Closing Cluster]                                                  ####

# Stopping Cluster
stopCluster(myCluster)
###############################################################################