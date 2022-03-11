#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-02-23 

# Purpose: Used to Available Analysis

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(raster)
library(tidyverse)
library(adehabitatHR)
library(mapview)
library(FedData)
library(lubridate)
library(foreach)
library(doParallel)
library(stringr)

#      Functions                                                            ####
#      Data                                                                 ####
#        [Deer]                                                             ####

df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv") 

#        [NLCD]                                                             ####

NLCD_Missouri <- raster("1.DataManagement\\CleanData\\NLCD_Missouri.tif")
NLCD_available_north <- raster("1.DataManagement\\CleanData\\NLCD_available_north.tif")
NLCD_available_south <- raster("1.DataManagement\\CleanData\\NLCD_available_south.tif")
NLCD_available_southeast <- raster("1.DataManagement\\CleanData\\NLCD_available_southeast.tif")

#        [DEM and derivative layers]                                        ####

Topo_Missouri <- stackOpen("1.DataManagement/CleanData/Topo_Missouri.stk")
Topo_available_north <- stackOpen("1.DataManagement/CleanData/Topo_available_north.stk")
Topo_available_south <- stackOpen("1.DataManagement/CleanData/Topo_available_south.stk")
Topo_available_southeast <- stackOpen("1.DataManagement/CleanData/Topo_available_southeast.stk")


#        [Missouri Shapefile]                                               ####

shp_Missouri <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp")

#        [Covariate Stack]                                                  ####

covariate_stack <- stack(NLCD_Missouri,Topo_Missouri)

###############################################################################
#   [Running Simulations at the Individual Scale]                           ####
#      [Sim Settings]                                                       ####
pop_df <- df_deer
time_partition_type <- "week"
raster_stack <- covariate_stack
n.avail.range <- 1:5
n.sim <- 25
export_dir <- "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Data"

#      [Simulation]                                                         ####

indiv_id_list <- unique(pop_df$id)

for (z in 1:length(indiv_id_list)) {
  
  print(paste0("IndivStart:",z,"/",length(indiv_id_list)))
  
  #   [Subsetting Data by individual]                                       ####
  subset_indiv <- pop_df %>% 
    filter(id == indiv_id_list[z])
  
  #   [Identifying Unique time partitions]                                  ####
  if(time_partition_type == "week"){
    time_partition <- distinct(subset_indiv, year, month, week)
  } else if (time_partition_type == "month"){
    time_partition <- distinct(subset_indiv, year, month)
  } else if (time_partition_type == "quarter") {
    time_partition <- distinct(subset_indiv, year, month)
  } else print("Please indicate time partition type")
  
  #   [Sim]                                                                 ####
  sim <- foreach(t = 1:nrow(time_partition),.errorhandling = 'remove', .combine = 'rbind') %do% {
    
    print(paste0(t,"/",nrow(time_partition)))
    
    #   [Temporal Subsets]                                                  ####
    year_per  <- time_partition[[t,1]] 
    month_per <- time_partition[[t,2]] 
    week_per  <- time_partition[[t,3]]
    
    # Subsetting by time partition 
    subset <- subset_indiv %>% 
      filter(year == year_per &
               month == month_per &
               week == week_per)
    
    #   [Spatio-temporal Subsets]                                           ####
    # Making the subset df a SpatialPointsDataFrame
    subset_spat <- SpatialPointsDataFrame(coords = cbind(subset$x,subset$y),
                                          data = subset,
                                          proj4string = crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
    
    # Minimum Convex Polygon of the subset Pop 
    mcp <- mcp(xy = subset_spat,
               percent = 95,
               unin = "m",
               unout = "km2")
    
    # Croping and Masking Covariate Raster based on MCP
    available_covariate_stack <- terra::crop(x = raster_stack,
                                             y = mcp,
                                             snap = "near",
                                             mask = F) %>% mask(mcp)
    #   [Used-Avail sim]                                                    ####
    
    # For Loop
    res<-foreach(i= 1:length(n.avail.range)) %do% {
      foreach(j= 1:n.sim,.combine = "rbind") %do% {
        
        samp <- sampleRandom(available_covariate_stack,(nrow(subset)*n.avail.range[i])) %>% as.data.frame()
        
        cbind("SimID" = paste0(indiv_id_list[z],"_","Y",year_per,"M",month_per,"W",week_per,"_","Available",n.avail.range[i],"_","Sim",j),
              "n.avail" = n.avail.range[i],
              year = year_per,
              month =  month_per,
              week = week_per,
              samp)
      }
    }
  }
  
  
  sim_results <- do.call(rbind, sim)
  
  write_csv(sim_results,file = paste0(export_dir,
                                      "/",
                                      indiv_id_list[z],
                                      "_",
                                      time_partition_type,
                                      ".csv"))
}


###############################################################################
#   [Plots]                                                                 ####
#      [List of csvs]                                                       ####
sim_data_list<- list.files(export_dir,
                      full.names = T)

#      [Fig Loop]                                                           ####
#        [Loop Settings]                                                    ####
time_partition_type <- "week"
export_dir_figs <- "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures"

#        [For Loop]                                                         ####

for (i in 1:length(sim_data_list)) {
  
  print(paste0("Indiv:",i,"/",length(sim_data_list)))
  
  # Extracting Individual's Data
  fig_data <- read_csv(sim_data_list[i])
  
  # Time partitions
  if(time_partition_type == "week"){
    time_partition <- distinct(fig_data, year, month, week)
  } else if (time_partition_type == "month"){
    time_partition <- distinct(fig_data, year, month)
  } else if (time_partition_type == "quarter") {
    time_partition <- distinct(fig_data, year, month)
  } else print("Please indicate time partition type")
  
  # Start of the other for loop
  
  for (t in 1:nrow(time_partition)) {
    print(paste0("Timestep:",t,"/",nrow(time_partition)))
    
    # Time Partition and subset
    year_par  <- time_partition[[t,1]] 
    month_par <- time_partition[[t,2]] 
    week_par  <- time_partition[[t,3]]
    
    fig_data_timesubset <- fig_data %>% 
      filter(year == year_par &
               month == month_par &
               week == week_par)
    
    # Extracting Info on Deer
    id <- str_sub(fig_data_timesubset$SimID,
                  start = 1,
                  end = str_locate(fig_data_timesubset$SimID,
                                   pattern = "_Y")[1]-1) %>% unique
    
    timebin <- str_sub(fig_data_timesubset$SimID,
                       start = str_locate(fig_data_timesubset$SimID,
                                          pattern = "_Y")[1]+1,
                       end = str_locate(fig_data_timesubset$SimID,
                                        pattern = "_A")[1]-1) %>% 
      unique()
    
    # Figures
    
    #   [Density Plots]                                              ####
    density_plot_Elevation <- ggplot(data = fig_data_timesubset, 
                                     aes(x = DEM_Missouri,group = SimID))+
      geom_density(size = 1, colour=alpha("black", 0.3))+
      theme_classic()+
      xlab("Elevation") +
      facet_grid(rows = vars(n.avail))
    
    ggsave(paste0(id,"_",timebin,"_DenPlot_","Elevation.jpeg"),
           plot = density_plot_Elevation,
           device = "jpeg",
           path = export_dir_figs,
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    density_plot_Slope <- ggplot(data = fig_data_timesubset, 
                                 aes(x = DEM_Slope,group = SimID))+
      geom_density(size = 1, colour=alpha("black", 0.3))+
      theme_classic()+
      xlab("Slope")+
      facet_grid(rows = vars(n.avail))
    
    ggsave(paste0(id,"_",timebin,"_DenPlot_","Slope.jpeg"),
           plot = density_plot_Slope,
           device = "jpeg",
           path = export_dir_figs,
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    density_plot_Aspect <- ggplot(data = fig_data_timesubset, 
                                  aes(x = DEM_Aspect,group = SimID))+
      geom_density(size = 1, colour=alpha("black", 0.3))+
      theme_classic()+
      xlab("Aspect")+
      facet_grid(rows = vars(n.avail))
    
    ggsave(paste0(id,"_",timebin,"_DenPlot_","Aspect.jpeg"),
           plot = density_plot_Aspect,
           device = "jpeg",
           path = export_dir_figs,
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    density_plot_TRI <- ggplot(data = fig_data_timesubset, 
                               aes(x = DEM_TRI,group = SimID))+
      geom_density(size = 1, colour=alpha("black", 0.3))+
      theme_classic()+
      xlab("TRI")+
      facet_grid(rows = vars(n.avail))
    
    ggsave(paste0(id,"_",timebin,"_DenPlot_","TRI.jpeg"),
           plot = density_plot_TRI,
           device = "jpeg",
           path = export_dir_figs,
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    
    #   [Topo - Boxplots]                                                    ####
    
    box_plot_Elevation <- ggplot(data = fig_data_timesubset, 
                                 aes(x = as.character(n.avail),y = DEM_Missouri))+
      geom_boxplot()+
      theme_classic()+
      xlab("Used to Available Ratio 1:x")+
      ylab("Elevation")
    
    ggsave(paste0(id,"_",timebin,"_BoxPlot_","Elevation.jpeg"),
           plot = box_plot_Elevation,
           device = "jpeg",
           path = export_dir_figs,
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    box_plot_Slope <- ggplot(data = fig_data_timesubset, 
                             aes(x = as.character(n.avail),y = DEM_Slope))+
      geom_boxplot()+
      theme_classic()+
      xlab("Used to Available Ratio 1:x")+
      ylab("Slope")
    
    ggsave(paste0(id,"_",timebin,"_BoxPlot_","Slope.jpeg"),
           plot = box_plot_Slope,
           device = "jpeg",
           path = export_dir_figs,
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    box_plot_Aspect <- ggplot(data = fig_data_timesubset, 
                              aes(x = as.character(n.avail),y = DEM_Aspect))+
      geom_boxplot()+
      theme_classic()+
      xlab("Used to Available Ratio 1:x")+
      ylab("Aspect")
    
    ggsave(paste0(id,"_",timebin,"_BoxPlot_","Aspect.jpeg"),
           plot = box_plot_Aspect,
           device = "jpeg",
           path = export_dir_figs,
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    box_plot_TRI <- ggplot(data = fig_data_timesubset, 
                           aes(x = as.character(n.avail),y = DEM_TRI))+
      geom_boxplot()+
      theme_classic()+
      xlab("Used to Available Ratio 1:x")+
      ylab("TRI")
    
    ggsave(paste0(id,"_",timebin,"_BoxPlot_","TRI.jpeg"),
           plot = box_plot_TRI,
           device = "jpeg",
           path = export_dir_figs,
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    #   [NLCD - Boxplots]                                                    ####
    
    
    fig_data_timesubset <- fig_data_timesubset %>% group_by(SimID,n.avail)
    
    
    legend <- pal_nlcd()
    legend <- data.frame("NLCD_Missouri" = legend$code,
                         "Category" = legend$class)
    
    fig_data_timesubset$NLCD_Missouri<-legend[match(fig_data_timesubset$NLCD_Missouri, legend$NLCD_Missouri),2]
    
    freq_table <- fig_data_timesubset %>% 
      with(table(SimID, NLCD_Missouri)) %>% 
      as.data.frame() %>% 
      group_by(SimID) %>% 
      mutate("n.avail" = str_extract(SimID,pattern = "(?<=Available)\\d"), .after = SimID) %>% 
      mutate("Total" = sum(Freq)) %>% 
      mutate("Proportion" = round(Freq/Total, digits = 3)) 
    
    NLCD_plot <-ggplot(freq_table,aes(x = NLCD_Missouri, y = Proportion))+
      geom_boxplot() +
      theme_bw()+
      xlab("NLCD Category")+
      facet_grid(rows = vars(n.avail))
    
    ggsave(paste0(id,"_",timebin,"_","NLCD.jpeg"),
           plot = NLCD_plot,
           device = "jpeg",
           path = export_dir_figs,
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
  }
}

###############################################################################