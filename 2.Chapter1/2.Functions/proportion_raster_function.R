proportion_raster_function <- function(raster,landcover_num, buffer_radius, export = F, export.filepath = NULL){ 
  
  if(export == F){
    r <- raster == landcover_num 
    r <- r %>% ratify
    
    fw <- ceiling(focalWeight(r, buffer_radius, type='circle'))
    
    r_focal <- raster::focal(x = r,
                             w = fw,
                             fun = mean,
                             na.rm = T,
                             pad=F)
    
    names(r_focal)<-paste0("proportion_",landcover_num)
    
    return(r_focal)
  }
  if(export == T){
    r <- raster == landcover_num 
    r <- r %>% ratify
    
    fw <- ceiling(focalWeight(r, buffer_radius, type='circle'))
    
    r_focal <- raster::focal(x = r,
                             w = fw,
                             fun = mean,
                             na.rm = T,
                             pad=F)
    
    names(r_focal)<-paste0("proportion_",landcover_num,"_buffer",buffer_radius,"m")
    
    writeRaster(r_focal,filename= paste0(export.filepath,names(r_focal)),format = "GTiff",overwrite = T)
    
    return(r_focal)
  }
}
