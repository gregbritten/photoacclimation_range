
library(sf)
library(fields)
library(raster)
library(fasterize)

##--READ SHAPE FILE--#############################
longshp <- st_read("data/longhurst_v4_2010/Longhurst_world_v4_2010.shp")
longshp$NUM <- as.numeric(row.names(longshp))

##--RASTERIZE SHAPE FILE--#######################
r0       <- raster(nrow=180,ncol=360)
longrast <- fasterize(longshp,r0,field="NUM")
longmat  <- t(as.matrix(longrast))[,(180):1]

long <- longmat
long[long%in%c(38,37,35,23,10,7)] <- 100   #oligotrophic
long[long%in%c(40,39,22,9,8,41)] <- 101    #tropical
long[long%in%c(52,51,34,32,31,30,18,6,5,4)] <- 102   #oligotrophic
long[long%in%c(54,53)] <- 103   #oligotrophic
long[long%in%c(3,2,1)] <- 104   #oligotrophic

long[!(long%in%c(100,101,102,103,104))] <- NA

names <- c('oligotrophic','tropical','temperate','polar south','polar north')

image.plot(long)



########################################
load('data/I.rdata')
I <- I*(1E6)*(1/86400)
load('data/PAR.rdata')
PAR <- PAR*(1E6)*(1/86400)

load('data/MLD.rdata')
load('data/KD.rdata')

par(mfrow=c(3,2))
k <-1
for(i in 100:104){
  tmp <- I
  tmp[long!=i] <- NA
  hist(tmp,xlim=c(0,1000),breaks=100,main='')
  mtext(names[k]); k = k+1
  abline(v=quantile(tmp,probs=0.95,na.rm=TRUE))
}

par(mfrow=c(3,2))
for(i in 100:104){
  tmp <- PAR
  tmp[long!=i] <- NA
  hist(tmp,xlim=c(0,1000))
}

