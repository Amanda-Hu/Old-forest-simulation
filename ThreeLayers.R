setwd("F:/UH-work/PREBAS simulation/Old Forest")
library(data.table)
library(Rprebasso)
library(ggplot2)

coord <- read.csv("inputs/coords.csv",header = T)
load("inputs/weather.rdata")
initVarX <- data.table(read.csv("Inputs_threelayer/initVar.csv",header = T))

## match siteID in different files ##initVarx$siteID=JO101,coor$siteID=JO101,siteinfo=JO101,obsData=JO101
sites <- as.character(unique(initVarX$siteID)) ##27
siteX <- which(coord[,1] %in% initVarX$siteID) ## lines that can match with initVarX$siteID
siteX
nSites <- length(sites)
initVar <- array(NA,dim=c(nSites,7,3)) ## how does the 7 and 3 comes-- 7 variables and 3 layers (form initVarX)
initVar
for(i in 1:nSites){
  initVar[i,,] <- as.matrix(initVarX[siteID==sites[i]][,2:4])
} ## for siteID in initVarX = sites, variables, variables 1-7, layers 3
initVar[is.na(initVar)] <- 0. ##apply NA in initVar 0
initVar[,2,] <- 1 ## change the initVar[,2,] which is age to 1
siteInfo <- read.csv("Inputs_threelayer/siteInfo.csv",header = T)
siteInfo$climID <- newClimIDs[siteX] ## where does newClimIDs come from

obsData <- data.table(read.csv("Inputs_threelayer/obsData.csv",header = T))
obsData[,SimYear:=SimYear- min(SimYear),by=SiteID] ## change the age identical with invitVar
obsData
obsData$SiteID <- match(obsData$SiteID,siteInfo$siteID) #match returns a vector of the positions of (first) matches of its first argument in its second.
siteInfo$siteID <- 1:nSites # change siteIfo$siteID to number as well, but is is certain that the sequential numbers match with obsData?

nYears <- obsData[,max(SimYear),by=SiteID]$V1
initPrebas <- InitMultiSite(nYearsMS = nYears,
                            siteInfo = siteInfo,
                            multiInitVar = initVar,
                            defaultThin = 0.,
                            ClCut = 0.,
                            PAR=PAR,
                            TAir = TAir,
                            CO2 = CO2,
                            VPD = VPD,
                            Precip = Precip
                            )

#initPrebas$pCROBAS[17,] <- initPrebas$pCROBAS[17,]  ##what's the point of this?
modOut <- multiPrebas(initPrebas)
obsX <- cbind(obsData[,c(1,3,4,2)],1)

simData <- modOut$multiOut[as.matrix(obsX)]
obsData[,mods:=simData] ## what does:= mean here :addd colomn
save(obsData,modOut,file="output/modOut.rdata")
varXs <- unique(obsData$nvar)  ## what is nvar: the variables' ID code in prebas
for( i in varXs){
  print(ggplot(obsData[nvar==i],aes(x=mods,y=Value)) + geom_point()+ggtitle(varNames[i]))
}

