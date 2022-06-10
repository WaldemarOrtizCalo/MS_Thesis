ffun <- function(x,na.rm) {
  ifelse(all(is.na(x)),
         NA,
         {ras <- matrix(data = x, nrow=3, ncol=3) %>% rast()
         pat <- patches(ras,directions = 8)
         nrow(unique(pat))})
}

w <- matrix(data = 1, nrow = 3, ncol = 3)

f2 <- terra::focal(r, 
                    w = w, 
                    fun = ffun, 
                    na.rm=T,
                    na.policy="all",
                    fillvalue=NA) 

plot(f2)




data <- c(1,1,1,NA,NA,NA,
  1,1,1,NA,NA,NA,
  1,1,1,NA,NA,NA,
  NA,NA,NA,NA,1,1,
  NA,NA,NA,NA,1,1,
  NA,NA,NA,NA,1,1)

ras <- matrix(data = data, nrow=6, ncol=6) %>% rast()
pat <- patches(ras,directions = 8) 

length(unique(pat))

result <- na.omit(unique(pat[,1])) %>% length


x <- na.omit(c(1,NA,3))


class(result)

class(sum(2+2))


if(is.na(a)) return(NA)
if (a < 0) a = 90 - a else 
  if (a > 90) a = 360 - a + 90 else 
    a = 90 - a
return(a)

ffun <- function(x,na.rm) {
  if(sum(x))
  
  ras <- matrix(data = x, nrow=3, ncol=3) %>% rast()
  pat <- patches(ras,directions = 8) %>% 
    length(unique(pat))
  
  if(is.na(a)) return(NA)
}











x <- c(1,2,3)
x <- c(NA,NA,NA)


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


if(all(is.na(x))) {
  return(print("Why"))}
else {
  return(sum(x))
}

x <- as.vector(data)

ifelse(all(is.na(x)),
       NA,
       {ras <- matrix(data = x, nrow=3, ncol=3) %>% rast()
       pat <- patches(ras,directions = 8)
       nrow(unique(pat))})

ras <- matrix(data = data, nrow=6, ncol=6) %>% rast()
pat <- patches(ras,directions = 8)
nrow(unique(pat))



