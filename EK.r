library(readxl)
library(sp)
library(rgdal)

d <- read_excel('data/PE_MAPP_2020_HAB_GKU_ADD-DATA_14062021.xlsx',col_types='numeric')
d <- d[complete.cases(cbind(d$LON,d$LAT)) & d$LAT>-90 & d$LON > -180,]

##--SPATIAL POINT-IN-POLYGON--##################################
dd <- data.frame(LON=d$LON,LAT=d$LAT)

coordinates(dd) <- ~LON + LAT

longshp <- readOGR("data/longhurst_v4_2010/Longhurst_world_v4_2010.shp")

proj4string(dd) <- sp::CRS("+proj=longlat +datum=WGS84")
proj4string(longshp) <- sp::CRS("+proj=longlat +datum=WGS84")

sp::over(dd,longshp)

##--ASSIGN PROVINCE NUMBERS--#############################################
##--not working: province numbers appear to be inconsistent between Camila and spreadsheet
# d$PROVNUM[d$PROVNUM%in%c(38,37,35,23,10,7)] <- 100   #oligotrophic
# d$PROVNUM[d$PROVNUM%in%c(40,39,22,9,8,41)] <- 101    #tropical
# d$PROVNUM[d$PROVNUM%in%c(52,51,34,32,31,30,18,6,5,4)] <- 102   #oligotrophic
# d$PROVNUM[d$PROVNUM%in%c(54,53)] <- 103   #oligotrophic
# d$PROVNUM[d$PROVNUM%in%c(3,2,1)] <- 104   #oligotrophic

d <- d[d$EK<1000,]  #remove a couple hugely anomolous points

# dnorth <- d[d$LAT>0 & d$MONTH%in%c(6,7,8),]
# dsouth <- d[d$LAT<0 & d$MONTH%in%c(12,1,2),]  

##--LATITUDINALLY DEFINED BINS--#############################
dpnorth   <- d[d$LAT >  45,]
dsubnorth <- d[d$LAT <  45 & d$LAT >  25,]
dtrop     <- d[d$LAT <  25 & d$LAT > -25,]
dsubsouth <- d[d$LAT > -45 & d$LAT < -25,]
dpsouth   <- d[d$LAT < -45,              ]

par(mfrow=c(3,2))

par(mfrow=c(1,1))
boxplot(dpnorth$EK[dpnorth$MONTH%in%c(6,7,8)],
          dsubnorth$EK[dsubnorth$MONTH%in%c(6,7,8)],
          dtrop$EK,
          dsubsouth$EK[dsubsouth$MONTH%in%c(12,1,2)],
          dpsouth$EK[dpsouth$MONTH%in%c(12,1,2)])


hist(dpnorth$EK[dpnorth$MONTH%in%c(6,7,8)])
hist(dsubnorth$EK[dsubnorth$MONTH%in%c(6,7,8)])
hist(dtrop$EK)
hist(dsubsouth$EK[dsubsouth$MONTH%in%c(12,1,2)])
hist(dpsouth$EK[dpsouth$MONTH%in%c(12,1,2)])

names <- c('oligotrophic','tropical','temperate','polar south','polar north')


##--PLOT ALL ACCORDING TO PROVINCE NUMBER--##########################
par(mfrow=c(3,2))
k <- 1
for(i in 100:104){
  hist(d$EK[d$PROVNUM==i],breaks=seq(0,1000,20),xlim=c(0,1000),main='')
  abline(v=quantile(d$EK[d$PROVNUM==i],probs=0.25,na.rm=TRUE))
  mtext(names[k])
  k <- k+1
}

##--PLOT NORTHERN SAMPLES ACCORDING TO PROVINCE NUMBER--#################
##--province numbers are inconsistent
par(mfrow=c(3,2))
k <- 1
for(i in 100:104){
  hist(dnorth$EK[dnorth$PROVNUM==i],breaks=seq(0,1000,20),xlim=c(0,1000),main='')
  abline(v=quantile(dnorth$EK[dnorth$PROVNUM==i],probs=0.25,na.rm=TRUE))
  mtext(names[k])
  k <- k+1
}
