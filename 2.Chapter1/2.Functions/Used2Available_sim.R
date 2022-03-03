Used2Available_sim <- function(pop_df, year_quarter, site.name,
                               raster_stack, n.avail.range,
                               n.sim,export_sim = F,export_dir = NULL){
  
  # Subsetting by yearly quarter and site 
  quarter_subset <- pop_df %>% 
    filter(site == site.name & quarteryear == year_quarter)
  
  # Making the subset df a SpatialPointsDataFrame
  quarter_subset <- SpatialPointsDataFrame(coords = cbind(quarter_subset$x,quarter_subset$y),
                                           data = quarter_subset,
                                           proj4string = crs("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
  
  # Minimum Convex Polygon of the subset Pop 
  mcp <- mcp(xy = quarter_subset,
             percent = 95,
             unin = "m",
             unout = "km2")
  
  # Croping and Masking Covariate Raster based on MCP 
  available_covariate_stack <- terra::crop(x = raster_stack,
                                           y = mcp,
                                           snap = "near",
                                           mask = F) %>% mask(mcp)
  
  # Making an empty vector for sims
  
  
  # For Loop
  res<-foreach(i= 1:length(n.avail.range)) %do% {
    foreach(j= 1:n.sim, .combine = 'rbind') %do% {
      
      samp <- sampleRandom(available_covariate_stack,(nrow(quarter_subset)*n.avail.range[i])) %>% as.data.frame()
      
      cbind("SimID" = paste0(site.name,"_",year_quarter,"_","Available",n.avail.range[i],"_","Sim",j),
            "n.avail" = n.avail.range[i],
            samp)
    }
  }
  
  result <- do.call(rbind,res)
  
  if(missing(export_sim)) {
    
    return(result)
    
  } else {
    
    #   [RAW Data]                                              ####
    write_csv(result,paste0(export_dir,"/",site.name,"_",year_quarter,".csv"))
    
    #      [Topo - DensityPlots]                                                ####
    
    density_plot_Elevation <- ggplot(data = result, 
                                     aes(x = DEM_Missouri,group = SimID))+
      geom_density(size = 1, colour=alpha("black", 0.3))+
      theme_classic()+
      xlab("Elevation") +
      facet_grid(rows = vars(n.avail))
    
    ggsave(paste0(site.name,"_",year_quarter,"_DenPlot_","Elevation.jpeg"),
           plot = density_plot_Elevation,
           device = "jpeg",
           path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    density_plot_Slope <- ggplot(data = result, 
                                 aes(x = DEM_Slope,group = SimID))+
      geom_density(size = 1, colour=alpha("black", 0.3))+
      theme_classic()+
      xlab("Slope")+
      facet_grid(rows = vars(n.avail))
    
    ggsave(paste0(site.name,"_",year_quarter,"_DenPlot_","Slope.jpeg"),
           plot = density_plot_Slope,
           device = "jpeg",
           path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    density_plot_Aspect <- ggplot(data = result, 
                                  aes(x = DEM_Aspect,group = SimID))+
      geom_density(size = 1, colour=alpha("black", 0.3))+
      theme_classic()+
      xlab("Aspect")+
      facet_grid(rows = vars(n.avail))
    
    ggsave(paste0(site.name,"_",year_quarter,"_DenPlot_","Aspect.jpeg"),
           plot = density_plot_Aspect,
           device = "jpeg",
           path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    density_plot_TRI <- ggplot(data = result, 
                               aes(x = DEM_TRI,group = SimID))+
      geom_density(size = 1, colour=alpha("black", 0.3))+
      theme_classic()+
      xlab("TRI")+
      facet_grid(rows = vars(n.avail))
    
    ggsave(paste0(site.name,"_",year_quarter,"_DenPlot_","TRI.jpeg"),
           plot = density_plot_TRI,
           device = "jpeg",
           path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    
    #      [Topo - Boxplots]                                                    ####
    
    box_plot_Elevation <- ggplot(data = result, 
                                 aes(x = as.character(n.avail),y = DEM_Missouri))+
      geom_boxplot()+
      theme_classic()+
      xlab("Used to Available Ratio 1:x")+
      ylab("Elevation")
    
    ggsave(paste0(site.name,"_",year_quarter,"_BoxPlot_","Elevation.jpeg"),
           plot = box_plot_Elevation,
           device = "jpeg",
           path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    box_plot_Slope <- ggplot(data = result, 
                             aes(x = as.character(n.avail),y = DEM_Slope))+
      geom_boxplot()+
      theme_classic()+
      xlab("Used to Available Ratio 1:x")+
      ylab("Slope")
    
    ggsave(paste0(site.name,"_",year_quarter,"_BoxPlot_","Slope.jpeg"),
           plot = box_plot_Slope,
           device = "jpeg",
           path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    box_plot_Aspect <- ggplot(data = result, 
                              aes(x = as.character(n.avail),y = DEM_Aspect))+
      geom_boxplot()+
      theme_classic()+
      xlab("Used to Available Ratio 1:x")+
      ylab("Aspect")
    
    ggsave(paste0(site.name,"_",year_quarter,"_BoxPlot_","Aspect.jpeg"),
           plot = box_plot_Aspect,
           device = "jpeg",
           path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    box_plot_TRI <- ggplot(data = result, 
                           aes(x = as.character(n.avail),y = DEM_TRI))+
      geom_boxplot()+
      theme_classic()+
      xlab("Used to Available Ratio 1:x")+
      ylab("TRI")
    
    ggsave(paste0(site.name,"_",year_quarter,"_BoxPlot_","TRI.jpeg"),
           plot = box_plot_TRI,
           device = "jpeg",
           path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    #      [NLCD - Boxplots]                                                    ####
    
    
    result <- result %>% group_by(SimID,n.avail)
    
    
    legend <- pal_nlcd()
    legend <- data.frame("NLCD_Missouri" = legend$code,
                         "Category" = legend$class)
    
    result$NLCD_Missouri<-legend[match(result$NLCD_Missouri, legend$NLCD_Missouri),2]
    
    freq_table <- result %>% 
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
    
    ggsave(paste0(site.name,"_",year_quarter,"_","NLCD.jpeg"),
           plot = NLCD_plot,
           device = "jpeg",
           path = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Figures",
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    return(result)
  }
}
