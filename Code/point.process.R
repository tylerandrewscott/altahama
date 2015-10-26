rm(list=ls())
require(foreign)
require(plyr)
require(dplyr)
require(rgdal)
require(sp)
require(rgeos)
require(maptools)
require(ggplot2)
require(reshape2)
library(RcppRoll)
library(devtools)
library(RCurl)
library(gdata)
require(proj4)
library(lubridate)
require(RODBC)
require(gridExtra)
require(lattice)
require(splancs)
require(fields)
library(raster)
library(shapefiles)
library(rasterVis)  # raster visualisation
library(rWBclimate)
library(stargazer)
library(texreg)
library(xtable)
library(magrittr)
library(ihs)

library(spdep)
library(INLA)

counties = readOGR('Shapefiles','coastalcounties')
nca.site = read.csv('Input/nca_siteinformationdata_narschallenge.csv')
nca.site = nca.site[nca.site$SITE_ID != '',]
site.spat =  SpatialPointsDataFrame(coords=cbind(nca.site$LON_DD,nca.site$LAT_DD),data=nca.site,proj4string = CRS("+proj=longlat"))
which.county = over(spTransform(site.spat,CRSobj = CRS(proj4string(counties))),counties)
nca.site = cbind(nca.site,which.county)

head(nca.site)
head(nca.chem)

nca.chem = read.csv('Input/nca_waterchemdata_narschallenge.csv')
names(nca.chem)[grep('Light.trans',names(nca.chem))] = 'Light.transmittance'
nca.chem <- nca.chem %>% mutate(chlor.std = as.vector(scale(Chla.ug.L)),nit.std = as.vector(scale(DIN.mg.N.L)), pho.std = as.vector(scale(DIN.mg.N.L)), 
                                do.std = as.vector(scale(DO.mg.L)), 
                                trans.std = as.vector(scale(Light.transmittance))) 

nca.chem$chem.index = rowSums(nca.chem[c('chlor.std','nit.std','pho.std','do.std','trans.std')],na.rm=T) / rowSums(!is.na(nca.chem[c('chlor.std','nit.std','pho.std','do.std','trans.std')]))

nca.chem = join(nca.chem,nca.site)

head(nca.chem)








test
plot(test)

mutate(chem.index  = rowSums(chlor.std,nit.std,pho.std,do.std,trans.std,na.rm=T))

unique(nca.chem$chem.index)
nca.chem$chem.index
tail(nca.chem)
as.vector(nca.chem$chlor.std)


nca.chem = join(nca.chem,nca.site)



           

rowm

ggplot(nca.chem,aes(x=LON_DD,y=LAT_DD))+geom_point()

table(nca.chem$CHG_PERIOD)
table(nca.chem$SAMPYEAR)

