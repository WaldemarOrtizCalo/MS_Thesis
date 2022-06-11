#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-05-05 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(raster)
library(tidyverse)
library(sf)
library(mapview)
library(pbapply)
library(doParallel)
library(stringr)
library(landscapemetrics)
library(foreach)
library(terra)
###############################################################################

ffun <- function(x,na.rm) {
  ifelse(all(is.na(x)),
         NA,
         {ras <- matrix(data = x, nrow=3, ncol=3) %>% rast()
         pat <- patches(ras,directions = 8)
         nrow(unique(pat))})
}


data <- c(1,1,1,NA,NA,NA,
          1,1,1,NA,NA,NA,
          1,1,1,NA,NA,NA,
          NA,NA,NA,NA,1,1,
          NA,NA,NA,NA,1,1,
          1,1,NA,NA,1,1)

if(all(is.na(x))) {return(NA)} else {
    ras <- matrix(data = x, nrow=3, ncol=3) %>% rast()
    pat <- patches(ras,directions = 8)
    return(length(unique(pat)))
  }


x <- as.vector(data)

ifelse(all(is.na(x)),
       NA,
       {ras <- matrix(data = x, nrow=6, ncol=6) %>% rast()
       pat <- patches(ras,directions = 8)
       nrow(unique(pat))})

ras <- matrix(data = data, nrow=6, ncol=6) %>% rast()
pat <- patches(ras,directions = 8)
nrow(unique(pat))

###############################################################################


data <- c(1,1,1,NA,NA,NA,
          1,1,1,NA,NA,NA,
          1,1,1,NA,NA,NA,
          NA,NA,NA,NA,1,1,
          NA,NA,NA,NA,1,1,
          1,1,NA,NA,1,1)

ras <- rast(matrix(data, ncol = 6, nrow = 6))

f <- focal(x = ras,
           w = matrix(data = 1, nrow = 3, ncol = 3),
           fun = function(x,...) {sum(x,...)},
           na.rm = T)

plot(f)


as.vector(patches(ras))



###############################################################################



pilot_data <- data <- c(1,1,NA,
                        1,1,NA,
                        1,1,NA)

pilot_data <- data <- c(NA,NA,NA,
                        NA,NA,NA,
                        NA,NA,NA)

pilot_ras <- rast(matrix(pilot_data, ncol = 3, nrow = 3))


f <- focal(x = ras,
           w = matrix(data = 1, nrow = 3, ncol = 3),
           fun = function(x, ...) {
             pilot_ras <- rast(matrix(pilot_data, ncol = 3, nrow = 3))
             pat <- patches(pilot_ras) %>% as.vector(mode = "numeric") %>% unique() %>% na.omit()
             length(pat)
           },
           na.rm = T)

matrix(pilot_ras,ncol = 3,nrow = 3, byrow = T) %>% sum(na.rm = T)

pat <- patches(pilot_ras) %>% as.vector(mode = "numeric") %>% unique() %>% na.omit()

length(pat)


data <- c(2,1,1,NA,NA,NA,
          1,1,1,NA,NA,NA,
          1,1,3,NA,NA,NA,
          NA,NA,NA,NA,1,1,
          NA,NA,NA,NA,1,1,
          2,2,NA,NA,1,2)

ras <- rast(matrix(data, ncol = 6, nrow = 6,byrow = T))
f <- focal(x = ras,
      w = matrix(data = 1, nrow = 3, ncol = 3),
      fun = function(x, ...) {diversity(na.omit(x),index = "shannon")},
      na.rm = T,
      pad = T)

plot(f)

f <- window_lsm(ras,
                w = matrix(data = 1, nrow = 3, ncol = 3),
                what = "lsm_l_shdi")

plot(f[[1]][[1]])

diversity(na.omit(data))

###############################################################################



SplitRas <- function(raster,ppside,save,plot){
  h        <- ceiling(ncol(raster)/ppside)
  v        <- ceiling(nrow(raster)/ppside)
  agg      <- aggregate(raster,fact=c(h,v))
  agg[]    <- 1:ncell(agg)
  agg_poly <- rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()
  for(i in 1:ncell(agg)){
    e1          <- extent(agg_poly[agg_poly$polis==i,])
    r_list[[i]] <- crop(raster,e1)
  }
  if(save==T){
    for(i in 1:length(r_list)){
      writeRaster(r_list[[i]],filename=paste("SplitRas",i,sep=""),
                  format="GTiff",datatype="FLT4S",overwrite=TRUE)  
    }
  }
  if(plot==T){
    par(mfrow=c(ppside,ppside))
    for(i in 1:length(r_list)){
      plot(r_list[[i]],axes=F,legend=F,bty="n",box=FALSE)  
    }
  }
  return(r_list)
}

data <- c(1,1,1,NA,NA,4,
          1,1,1,NA,NA,NA,
          1,1,1,NA,NA,NA,
          NA,NA,NA,NA,2,2,
          NA,NA,NA,NA,2,2,
          3,3,NA,NA,2,2)

ras <- raster(matrix(data, ncol = 6, nrow = 6,byrow = T)) 


a <- raster::aggregate(ras,3)

p <- as(a, "SpatialPolygons")
x <- lapply(seq_along(p), function(i) crop(ras, p[i]))

plot(p, border='gray')
z <- lapply(x, function(i) plot(i, add=T, legend=F))
