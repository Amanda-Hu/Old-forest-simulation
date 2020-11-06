## Amanda, 02.11.2020

## Data analysis for Lumes sites
## Estimating initial variables for the PREBAS model simulations (multisite application)
## and plotting time-series of observed forest characteristics

rm(list = ls())
#install.packages("siplab")
#install.packages("plotrix")
#install.packages("gtools")
require(data.table)
require(siplab)
require(plotrix)
require(spatstat)
require(gtools)

setwd("F:/UH-work/PREBAS simulation/Raw data")
tr = read.csv("lumes_remeasured_20201105.csv")
tr=tr[-which(tr$dbh==0),]

# A function to subset the data based on the measurement time

getDataRows = function(time){
  if (time==1){
    datarows = tr[tr$year<2000,] # First
    datarows <- datarows[order(datarows$site,datarows$tree,datarows$year),]
  }
  else if (time==2){  # Second
    tr_2006 = tr[tr$year==2006,]
    tr_2007 = tr[tr$year==2007,]
    tr_2014 = tr[tr$year==2014,]
    datarows = rbind(tr_2006,tr_2007,tr_2014)
    datarows <- datarows[order(datarows$site,datarows$tree,datarows$year),]
  }
  else if (time==3) { # Third
    datarows = tr[tr$year==2019,]
    datarows <- datarows[order(datarows$site,datarows$tree,datarows$year),]
  }
  return(datarows)
} # end of function


###########################################
# 1. Define layers for PREBAS simulation

# Divide the data into 3 classes: small, medium and large trees
# layer values: small = 0-20 cm, medium = 20-30 cm, large = above 30 cm
# Note: do the classification based on the first year!!

tr_1990 = getDataRows(1)
tr_0607 = getDataRows(2)
tr_2019 = getDataRows(3)
tr_2019 = tr_2019[-which(tr_2019$log==TRUE),]
length(tr_2019[which(tr_2019$dead==TRUE),])
table(tr_2019$log)
table(tr_2019$dead)
table(tr_2019$snag)

# Define the layers based on the first measurement (dbh.x)

# Subset the first measurement and define the layer values for them.
# Then assign the same layer values for the same trees in the 2nd and 3rd measurement.
# Finally combine the tree subsets, including all living trees.

# Define the layers based on the first measurement (dbh.x)
# All 27 sites still included
trees.all.2019 = merge(tr_1990,tr_2019,by=c("site","tree"))
trees.all.2019$dbh.x[is.na(trees.all.2019$dbh.x)]=0

# Multiple layers, 5 cm intervals
#diameters = seq(min(trees.all.2006$dbh.x),max(trees.all.2006$dbh.x)+5,5)
diameters = seq(10,max(trees.all.2019$dbh.x)+5,5)

getLayer = function(dbh,diameters){
  i = 1
  while(diameters[i]<dbh){
    i = i+1
  }
  return(i)
} # end of function

layer = c()
for (i in 1:nrow(trees.all.2019)){
  layer[i]=getLayer(trees.all.2019$dbh.x[i], diameters)
}

# First year, set the layer value based on the 1st year
trees.all.2019$layer.x <- as.factor(layer)
# Second year, set the layer value based on the 1st year
trees.all.2019$layer.y <- as.factor(layer)

tr_S1 <- with(trees.all.2019, rbind(data.frame(site = site,
                                                tree = tree,
                                                latitude = latitude.x,
                                                longitude = longitude.x,
                                                angle = angle.x,
                                                radius = radius.x,
                                                site_area = site_area.x,
                                                site_type = site_type.x,
                                                species = species.x,
                                                year = year.x,
                                                dbh = dbh.x,
                                                ba = ba.x,
                                                dead = dead.x,
                                               snag=snag.x,
                                               log=log.x,
                                                height = height.x,
                                                crown_height = crown_height.x,
                                                crown_width = crown_width.x,
                                                category = category.x, 
                                                layer = layer.x),
                                     
                                     data.frame(site = site,
                                                tree = tree,
                                                latitude = latitude.y,
                                                longitude = longitude.y,
                                                angle = angle.y,
                                                radius = radius.y,
                                                site_area = site_area.y,
                                                site_type = site_type.y,
                                                species = species.y,
                                                year = year.y,
                                                dbh = dbh.y,
                                                ba = ba.y,
                                                dead = dead.y,
                                                snag=snag.y,
                                                log=log.y,
                                                height = height.y,
                                                crown_height = crown_height.y,
                                                crown_width = crown_width.y,
                                                category = category.y, 
                                                layer = layer.y)))

tr_S1 <- tr_S1[order(tr_S1$site,tr_S1$tree,tr_S1$year),]
# unique(tr_S1$site) #27 sites

# Add the second measurement to the dataset tr_S1 containing the all trees between 1990s and 2019

trees.all.2006 = merge(tr_0607, tr_S1[tr_S1$year==2019,], by=c("site","tree"), all=TRUE)
# Save VA210
site_VA210 = trees.all.2006[trees.all.2006$site=="VA210",]
site_VA210$year.x <- 2006

# Filter out dead trees and empty rows
#trees.all.2006 = trees.all.2006[trees.all.2006$dead==FALSE,]
trees.all.2006 = trees.all.2006[!apply(is.na(trees.all.2006[trees.all.2006$latitude.y,]),1,all),] ##apply(,1,), here 1 indicate rows #sum(is.na(trees.all.2006$latitude.y))

# Add site VA210 to the dataset, beacause no measurement of VA210 in 0607
trees.all.2006 = rbind(trees.all.2006, site_VA210)

tr_S2 <- with(trees.all.2006, rbind(data.frame(site = site,
                                                tree = tree,
                                                latitude = latitude.x,
                                                longitude = longitude.x,
                                                angle = angle.x,
                                                radius = radius.x,
                                                site_area = site_area.x,
                                                site_type = site_type.x,
                                                species = species.x,
                                                year = year.x,
                                                dbh = dbh.x,
                                                ba = ba.x,
                                               dead = dead.x,
                                               snag=snag.x,
                                               log=log.x,
                                                height = height.x,
                                                crown_height = crown_height.x,
                                                crown_width = crown_width.x,
                                                category = category.x,
                                                layer = layer),
                                     
                                     data.frame(site = site,
                                                tree = tree,
                                                latitude = latitude.y,
                                                longitude = longitude.y,
                                                angle = angle.y,
                                                radius = radius.y,
                                                site_area = site_area.y,
                                                site_type = site_type.y,
                                                species = species.y,
                                                year = year.y,
                                                dbh = dbh.y,
                                                ba = ba.y,
                                                dead = dead.y,
                                                snag=snag.y,
                                                log=log.y,
                                                height = height.y,
                                                crown_height = crown_height.y,
                                                crown_width = crown_width.y,
                                                category = category.y,
                                                layer = layer)))

tr_S2 <- tr_S2[order(tr_S2$site,tr_S2$tree,tr_S2$year),]

tr <- rbind(tr_S1[tr_S1$year<2000,],tr_S2)
tr <- tr[order(tr$site,tr$tree,tr$year),]

# Note: this version conclude all the trees (living and dead,log) throughout the study period.

# Test that the number of trees matches between years
site.name = as.character(na.exclude(unique(tr$site)))

for (i in 1:length(site.name)){
  treedata1 = tr[tr$site==site.name[i]&tr$year<2000,]$tree
  #print(site.name[i])
  #print(length(treedata1))
  treedata2 = tr[tr$site==site.name[i]&tr$year==2019,]$tree
  treedata3 = tr[tr$site==site.name[i]&tr$year>2000&tr$year<2019,]$tree
  #print(length(treedata2))
  if(length(treedata1)!=length(treedata2)) 
    print(site.name[i])
  if(length(treedata1)!=length(treedata3)) 
    print(site.name[i])
  if(length(treedata2)!=length(treedata3)) 
    print(site.name[i])
}

# 2. Hegyi's competition index (original script by Otto Saikkonen)

# The index is calculated separately for each measurement year

#Create empty lists for measurement times, sites and PPP values
allsites = list()
site.s = list() # ppp calculated separately for each site
ppp = list()  # a point pattern object 

# t refers to measurement number
# i refers to site number

for (t in 1:3){
  subdata = getDataRows(1)
  site.name = as.character(na.exclude(unique(subdata$site)))
  subdata = getDataRows(t)
  for (i in 1:length(site.name)){
    if (t==2&i==22) next
    # t=2,i=22 is the site V210, no measurement
    # Subset rows with NA or empty values
    subdata = subdata[!apply(is.na(subdata),1,all),]
    site.s[[i]] = subdata[subdata$site==site.name[i],]
    # Calculate site area and radius
    A = site.s[[i]]$site_area[1]  # select only the first item
    r <- sqrt(A/pi)
    # Note: marks are dbh in meters
    # Calculate the x and y coordinates of each tree
    site.s[[i]]$x <- cos(site.s[[i]]$angle*(pi/180))*site.s[[i]]$radius/100
    site.s[[i]]$y <- sin(site.s[[i]]$angle*(pi/180))*site.s[[i]]$radius/100 
    # Calculate the tree map
    ppp[[i]] <- ppp(site.s[[i]]$x,site.s[[i]]$y, window = disc(r,c(0,0)), marks=site.s[[i]]$dbh/100)
    # Calculate Hegyi's index
    hegyi <- pairwise(ppp[[i]], maxR = 6, kernel = powers_ker, kerpar = list(pi=1,pj=1,pr=1,smark=1))
    #plot(ppp[[i]], main = site.name[i])
    marks <- hegyi$marks
    site.s[[i]]$hegindex <- marks$cindex
  }
  allsites[[t]] = rbindlist(site.s)
}
allyears = rbindlist(allsites)

tr = allyears[order(allyears$site,allyears$tree,allyears$year),]

#Change Inf to NA values, so that lm works
tr$hegindex[which(is.nan(tr$hegindex))]= NA
#tr=tr[-which(tr$hegindex==Inf),]
tr$hegindex[which(tr$hegindex==Inf)] = NA

site.name = unique(tr$site)

# Remove duplicated rows
tr = unique(tr)

# Subset data to spruce (S), pine (P) and deciduous (D) trees
#tr_S <- tr_living[tr_living$species == 2,]
#tr_P <- tr_living[tr_living$species == 1,]
#tr_D <- tr_living[as.numeric(tr_living$species)>2,]
tr_S <- tr[tr$species == 2,]

# What is category?

# 3. Height (h), crown base height (hc) and crown width (cw) estimates for spruce

site.s = list()

#redefine log to be zero when x <= 0, to make sure the for loop run
log <- function(x) ifelse(x <= 0, NA, base::log(x))
for (i in 1:length(site.name)){
  site.s[[i]] = tr_S[tr_S$site==site.name[i],]
  h.lm = lm(site.s[[i]]$height ~ log(site.s[[i]]$dbh))
  site.s[[i]]$h_model = predict(h.lm, site.s[[i]])/10
  #hc.lm = lm(site.s[[i]]$crown_height ~ log(site.s[[i]]$dbh))
  
  # TO BE UPDATED!
  # Model crown length instead of crown base height?
  hc.lm = lm(site.s[[i]]$crown_height ~ site.s[[i]]$h_model+site.s[[i]]$hegindex)
  site.s[[i]]$hc_model = predict(hc.lm, site.s[[i]])/10

} # end of the loop
tr_S = rbindlist(site.s)

tr_S$cw_model = NA

# Crown width was measured only in the third time
subdata = getDataRows(3)
site.name = as.character(na.exclude(unique(subdata$site)))

site.s = list()
for (i in 1:length(site.name)){
  site.s[[i]] = subdata[subdata$site==site.name[i],]
  # Model based on the 3rd measurement time
  site.s[[i]]$hegindex[is.na(site.s[[i]]$hegindex)] = 0
  cw.lm = lm(site.s[[i]]$crown_width ~ site.s[[i]]$dbh + site.s[[i]]$hegindex)
  # Prediction for whole data
  site.s[[i]] = tr_S[tr_S$site==site.name[i],]
  site.s[[i]]$cw_model = predict(cw.lm, site.s[[i]])
  # change the 28 element to dataframe for rbindlist function
  #site.s[[28]]=as.data.frame(site.s[[28]])
  
 } # end of the loop
tr_S3 = rbindlist(site.s,fill = T)
tr_S = tr_S3
# 27 sites

# Estimating crown base height is difficult, would a nonlinear approach be better?

###########################################

# The cross-sectional area at crown base Ac
# Transform cm2 to m2
phi_s = 1.43
#tr_S$Ac = (tr_S$ba - 2*phi_s)/10^4
tr_S$Ac = (tr_S$ba/(2*phi_s-1))/10^4

# Crown length Lc
tr_S$Lc = tr_S$h_model - tr_S$hc_model 

# Crown ratio r
tr_S$crown_r = tr_S$Lc/tr_S$h_model

# Form coefficients for spruce
rho_s = 351
rho_b = 590
phi_s = 1.43
phi_c = 0.43
phi_b = 1.75
gamma_b = 0.50
b = 0.48
eta_s = 817.5
#eta_sb = 0.91
eta_sb=1.58

# Assign phi_s for different layers; how can we divide the trees into age groups?

# Young
# Middle-aged
# Mature
# Old

# Alternative from Table 6, k=8 (Hu et al.) for old trees
#beta_1 = 0.372
#beta_A = 0.372
#beta_0 = 0.004
#my_ij = 0
#e = 1.175
#phi_s = (beta_1+beta_A)*(1+tr_S$crown_r)/2*tr_S$crown_r+beta_0+my_ij+e

# Above- and belowground dry biomass W_ab (kg)
# Equation 8 and Table 5 (Hu et al. 2020 manuscript)
# See specific parameters for old-growth forests from the paper!!

tr_S$W_ab = tr_S$Lc*tr_S$Ac*((rho_s*phi_s*(1-tr_S$crown_r))/tr_S$crown_r+
                               rho_s*phi_c+
                               rho_b*phi_b*gamma_b*tr_S$Lc^(b-1)*eta_sb+
                               eta_s/tr_S$Lc)

# Stem volume V (above and below the crown base) (m3)
#tr_S$V = phi_c*tr_S$Ac*tr_S$Hc+phi_s*tr_S$Ac*tr_S$hc_model

# Only the stem below crown base accounted for??
# Alternative model in Table 6, k=7 (Hu et al.)
tr_S$Vs = phi_s*tr_S$Ac*tr_S$hc_model

# Stem dry biomass W_stem (kg)
tr_S$Ws = rho_s*tr_S$Vs
#######################################################################
# Calculate stand level estimates of BA and W_ab etc.

# 1. Split the data by site, year and diameter class (layer) to calculate PREBAS input information

# One site in each iteration
site.name = as.character(na.exclude(unique(tr_S$site)))

### PREBAS initial data for the multisite simulation

# initVar: Age, H, DBH, BA, Hc, Ac by layer
# Add a column siteID and make a unique file including all sites

output = list()
outpath = "F:/UH-work/PREBAS simulation/Raw data/output for prebas/"
nLayers = length(diameters)

for (i in 1:length(site.name)){
  latitude <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("year")], function(TR) {
    return(TR$latitude[1])
  })))
  longitude <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("year")], function(TR) {
    return(TR$longitude[1])
  })))
  D <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(mean(TR[TR$dead=="FALSE"]$dbh))
  })))
  H <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(mean(TR[TR$dead=="FALSE"]$h_model))
  })))
  BA <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(sum(TR$ba[TR$dead=="FALSE"]/TR$site_area))
  })))
  Hc <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(mean(TR[TR$dead=="FALSE"]$hc_model))
  })))
  Ac <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(mean(TR[TR$dead=="FALSE"]$Ac))
  })))
  V <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(sum(TR[TR$dead=="FALSE"]$Vs*10^4/TR[TR$dead=="FALSE"]$site_area))
  })))
  W_stem <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(sum(TR[TR$dead=="FALSE"]$Ws*10^4*0.5/TR[TR$dead=="FALSE"]$site_area))
  }))) 
  W_ab <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(sum(TR[TR$dead=="FALSE"]$W_ab*10^4*0.5/TR[TR$dead=="FALSE"]$site_area))
  })))
  
  nvar = length(c("speciesID","age","H","DBH","BA","Hc","Ac"))
  siteID = rep(site.name[i],nvar)
  species.ID = rep(2,nLayers)
  Age = rep(1,nLayers)
  output[[i]] = cbind(as.data.frame(rbind(species.ID, Age, t(H)[1,], t(D)[1,], t(BA)[1,], t(Hc)[1,], t(Ac)[1,])),siteID)
  output[[i]] = as.data.frame(output[[i]])
  colnames(output[[i]]) = c(c(1:nLayers),"siteID")
  rownames(output[[i]]) = c("speciesID","age","H","DBH","BA","Hc","Ac")
  
}# End

output = rbindlist(output)
initVar = cbind(rep(c("speciesID","age","H","DBH","BA","Hc","Ac"),length(site.name)),output)
write.csv(initVar, file = paste(outpath,"initVar",".csv", sep=""),row.names=F)

# In PA7 and VA403 Hc>H in layer 1 !?

# siteInfo: siteID, climID, siteType, SWinit, CWinit, SOGinit, Sinit, 
# nLayers, nSpecies, soildepth, effective field capacity, permanent wilting point

# To Be Updated
SWinit = 160
GWinit = 0
SOGinit = 0
Sinit = 20
nLayers = length(diameters)
nSpecies = 3
soildepth = 413
effective.field.capacity = 0.45
permanent.wilting.point = 0.118

output = list()

for (i in 1:length(site.name)){
  siteID = site.name[i]
  climID = i
  siteType <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("year")], function(TR) {
    return(TR$site_type[1])
  })))
  output[[i]] = cbind(siteID, climID, t(siteType)[1], SWinit, GWinit, SOGinit, Sinit, nLayers, nSpecies, soildepth,
                      effective.field.capacity, permanent.wilting.point)
  output[[i]] = as.data.frame(output[[i]])
  names(output[[i]]) = c("siteID","climID","siteType", "SWinit", "GWinit", "SOGinit", "Sinit", "nLayers",
                         "nSpecies", "soildepth","effective field capacity", "permanent wilting point")
  #output[[i]] = cbind(siteID, t(longitude)[1], t(latitude)[1])
  #output[[i]] = as.data.frame(output[[i]])
  #names(output[[i]])=c("siteID","lon","lat")
}
site.info = rbindlist(output)
write.csv(site.info, file = paste(outpath,"siteInfo",".csv", sep=""), row.names=F)
#coords = rbindlist(output)
#write.csv(coords, file=paste(outpath, "coords.csv", sep=""), row.names = F)

# obsData: SiteID, layerID, SimYear, nvar, Value, sd

obs.data = list()

for (i in 1:length(site.name)){
  D <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(mean(TR[TR$dead=="FALSE"]$dbh))
  })))
  H <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(mean(TR[TR$dead=="FALSE"]$h_model))
  })))
  BA <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(sum(TR[TR$dead=="FALSE"]$ba/TR[TR$dead=="FALSE"]$site_area))
  })))
  Hc <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(mean(TR[TR$dead=="FALSE"]$hc_model))
  })))
  N <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(length(TR[TR$dead=="FALSE"]$tree)/TR[TR$dead=="FALSE"]$site_area[1]*10^4)
  })))
  V <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(sum(TR[TR$dead=="FALSE"]$Vs*10^4/TR[TR$dead=="FALSE"]$site_area))
  })))
  W_stem <- as.data.frame(cbind(by(tr_S[tr_S$site==site.name[i],], tr_S[tr_S$site==site.name[i],][,c("layer","year")], function(TR) {
    return(sum(TR[TR$dead=="FALSE"]$Ws*10^4*0.5/TR[TR$dead=="FALSE"]$site_area))
  }))) 
  
  layer = c(1:nLayers)
  year = as.vector(as.numeric(colnames(H))-min(tr_S$year)+1)
  #var = t(matrix(c(c(11:14),30,31),nrow=6,ncol=3))
  var = t(matrix(c(c(11:14),17,30,31),nrow=7,ncol=nLayers))
  sd = c(0.5,0.5,0.5) # To be updated
  siteID = site.name[i]
  
  h = list()
  d = list()
  ba = list()
  hc = list()
  n = list()
  v = list()
  wstem = list()
  
  # Number of years
  n.year=unique(tr_S[tr_S$site==site.name[i],]$year)
  
  for (j in 1:length(n.year)){
    #for (j in 1:3){
    h[[j]] = as.data.frame(cbind(siteID,layer,year[j],var[,1],H[,j],sd[j]))
    d[[j]] = as.data.frame(cbind(siteID,layer,year[j],var[,2],D[,j],sd[j]))
    ba[[j]] = as.data.frame(cbind(siteID,layer,year[j],var[,3],BA[,j],sd[j]))
    hc[[j]] = as.data.frame(cbind(siteID,layer,year[j],var[,4],Hc[,j],sd[j]))
    n[[j]] = as.data.frame(cbind(siteID,layer,year[j],var[,5],N[,j],sd[j]))
    v[[j]] = as.data.frame(cbind(siteID,layer,year[j],var[,6],V[,j],sd[j]))
    wstem[[j]] = as.data.frame(cbind(siteID,layer,year[j],var[,7],W_stem[,j],sd[j]))
  }
  h.all = rbindlist(h)
  d.all = rbindlist(d)
  ba.all = rbindlist(ba)
  hc.all = rbindlist(hc)
  n.all = rbindlist(n)
  v.all = rbindlist(v)
  wstem.all = rbindlist(wstem)
  obs.data[[i]] = as.data.frame(rbind(h.all,d.all,ba.all,hc.all,n.all,v.all,wstem.all))
  colnames(obs.data[[i]]) = c("SiteID","layerID","SimYear","nvar","Value","sd")
  obs.data[[i]] = obs.data[[i]][order(obs.data[[i]]$nvar, mixedsort(obs.data[[i]]$layerID),obs.data[[i]]$SimYear),]
}# End

obs.data = rbindlist(obs.data)

write.csv(obs.data, file = paste(outpath,"obsData",".csv", sep=""),row.names=F)

