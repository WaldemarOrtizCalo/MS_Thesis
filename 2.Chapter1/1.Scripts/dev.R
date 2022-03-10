
pop_df = df_deer
indiv_id = "N15001"
time_partition_type = "week"
site.name = "North"
raster_stack = covariate_stack
n.avail.range = 1
n.sim = 2


# Subsetting Data by individual
subset_indiv <- pop_df %>% 
  filter(site == site.name & id == indiv_id)

# Identifying Unique time partitions 
if(time_partition_type == "week"){
  time_partition <- distinct(subset_indiv, year, month, week)
} else if (time_partition_type == "month"){
  time_partition <- distinct(subset_indiv, year, month)
} else if (time_partition_type == "quarter") {
  time_partition <- distinct(subset_indiv, year, month)
} else print("Please indicate time partition type")

# Foreach loop for everything
test <- foreach(t = 1:nrow(time_partition),.errorhandling = 'remove', .combine = 'rbind') %do% { 
  
  year_per  <- time_partition[[t,1]] 
  month_per <- time_partition[[t,2]] 
  week_per  <- time_partition[[t,3]]
  
  # Subsetting by time partition 
  subset <- subset_indiv %>% 
    filter(year == year_per &
             month == month_per &
             week == week_per)
  
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
  # For Loop
  res<-foreach(i= 1:length(n.avail.range)) %do% {
    foreach(j= 1:n.sim, .combine = 'rbind') %do% {
      
      samp <- sampleRandom(available_covariate_stack,(nrow(subset)*n.avail.range[i])) %>% as.data.frame()
      
      cbind("SimID" = paste0(indiv_id,"_","Y",year_per,"M",month_per,"W",week_per,"_","Available",n.avail.range[i],"_","Sim",j),
            "n.avail" = n.avail.range[i],
            samp)
    }
  }
}
result <- do.call(rbind,test)
