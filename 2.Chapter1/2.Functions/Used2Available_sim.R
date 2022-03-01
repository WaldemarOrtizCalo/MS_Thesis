Used2Available_sim <- function(pop_df, year_quarter, site.name,raster_stack, n.avail.range,n.sim){
  
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
      
      cbind("SimID" = paste0(site.name,"_",year_quarter,"_","Available",n.avail.range[i],"_","Sim",j),samp)
      
    }
  }
  
  # Output of function
  return(do.call(rbind,res))
}