library(data.table)
library(Rprebasso)
library(ggplot2)
library(ggpubr)

#three layers output
#load("output/modOut.rdata")
varXs <- unique(obsData$nvar)
nSites <- dim(modOut$multiOut)[1]
###proc data for single plots
simData <- data.table()
for(siteID in 1:nSites){
  for(varID in varXs){
    for(layer in 1:3) simData <- rbind(simData,cbind(modOut$multiOut[siteID,,c(7,varID),layer,1],layer,siteID,varID),use.names=F)
  }
}

setnames(simData,c("SimYear","Value","layerID","SiteID","nvar"))
simData$dataSource <- "sim"
obsX <- obsData[,1:5]
obsX$dataSource <- "obs"
allData <- rbind(simData,obsX)
allData$layerID <- factor(allData$layerID)

pdf("output/plots_threelayers.pdf")
for( i in varXs){
  print(ggplot(obsData[nvar==i],aes(x=mods,y=Value)) +xlim(0.0001,NA)+ geom_point()+ggtitle(varNames[i]))
}  ### add xlim(0.001,NA) to remove the x=o points

plotX <- list()
for(siteID in 1:nSites){
  for(varID in varXs){
    plotX[[varID]] <- ggplot(allData[SiteID==siteID & nvar==varID & dataSource=="sim"],
                             aes(x=SimYear,y=Value,col=layerID)) +
      geom_line()+ggtitle(paste0(varNames[varID],"; site ",siteID)) + 
      geom_point(allData[SiteID==siteID & nvar==varID & dataSource=="obs"],
                 mapping = aes(x=SimYear,y=Value,col=layerID)) 
  }
  print(ggarrange(plotX[[11]],plotX[[12]],plotX[[13]],
                  plotX[[14]],plotX[[30]],plotX[[17]],common.legend = T))
}
dev.off()
allData[dataSource=="sim"]


## Multilaters
#load("output/modOut_multi.rdata")
varXs <- unique(obsData$nvar)
nSites <- dim(modOut_multi$multiOut)[1]
###proc data for single plots
simData <- data.table()
for(siteID in 1:nSites){
  for(varID in varXs){
    for(layer in 1:11) simData <- rbind(simData,cbind(modOut_multi$multiOut[siteID,,c(7,varID),layer,1],layer,siteID,varID),use.names=F)
  }
}

setnames(simData,c("SimYear","Value","layerID","SiteID","nvar"))
simData$dataSource <- "sim"
obsX <- obsData[,1:5]
obsX$dataSource <- "obs"
allData <- rbind(simData,obsX)
allData$layerID <- factor(allData$layerID)

pdf("output/plots_multilayers.pdf")
for( i in varXs){
  print(ggplot(obsData[nvar==i],aes(x=mods,y=Value)) +xlim(0.0001,NA)+ geom_point()+ggtitle(varNames[i]))
}  ### add xlim(0.001,NA) to remove the x=o points

plotX <- list()
for(siteID in 1:nSites){
  for(varID in varXs){
    plotX[[varID]] <- ggplot(allData[SiteID==siteID & nvar==varID & dataSource=="sim"],
                             aes(x=SimYear,y=Value,col=layerID)) +
      geom_line()+ggtitle(paste0(varNames[varID],"; site ",siteID)) + 
      geom_point(allData[SiteID==siteID & nvar==varID & dataSource=="obs"],
                 mapping = aes(x=SimYear,y=Value,col=layerID)) 
  }
  print(ggarrange(plotX[[11]],plotX[[12]],plotX[[13]],
                  plotX[[14]],plotX[[30]],plotX[[17]],common.legend = T))
}
dev.off()
allData[dataSource=="sim"]