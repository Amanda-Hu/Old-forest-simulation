library(rgeos)
library(sp)
library(data.table)

coord <- read.csv("inputs/coords.csv",header = T)
coord <- SpatialPoints(coord[,2:3])

load("C:/Users/minunno/Documents/research/extarctWeather/inputs/CurrClim.rdata")
climID <- apply(gDistance(finGrid, coord, byid=TRUE), 1, which.min)

climIDunique <- unique(climID)
newClimIDs <- match(climID,climIDunique)
nclimIDs <- length(climIDunique)
clims = dat[id %in% climID]

clims[,climID:=match(id,climIDunique)]
PAR <- TAir <- Precip <- VPD <- CO2 <- 
  matrix(length(climIDunique), nclimIDs , length(clims[climID==1,PAR]))
for(i in newClimIDs){
  PAR[i, ] <- clims[climID==i]$PAR
  TAir[i, ] <- clims[climID==i]$TAir
  Precip[i, ] <- clims[climID==i]$Precip
  VPD[i, ] <- clims[climID==i]$VPD
  CO2[i, ] <- clims[climID==i]$CO2
}
  
save(newClimIDs,PAR,TAir,Precip,VPD,CO2,file="inputs/weather.rdata")
