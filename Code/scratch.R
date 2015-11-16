getwd()

library(mosaic)
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
library(tidyr)
library(spdep)
library(INLA)


library(Hmisc)

sort(list.files('Input/'))

install.packages('Hmisc')
library(Hmisc)
cw04 <- mdb.get('Input/cwns2004db.mdb',tables=TRUE)

install.packages('RODBC')
library(RODBC)
# Road Density

cw04 <- odbcConnect('Input/cwns2004db.mdb')

rddall <- odbcConnectAccess("n:/products/Allparks/Roads_RDDALL.mdb")

sqlTables(rddall)
rdd.raw <- sqlFetch(rddall,"Roads_rddall") # a raw table
rdd.qry <- sqlFetch(rddall,"qry_Roads_RDDALL") # a saved query or view
rdd.qry.final <- sqlFetch(rddall,"qryroadsRDDALL_FINAL") # a saved query or view

dev2001.nlcd = read.dbf('Input/dev_county_2001.dbf')
dev2001.nlcd = dev2001.nlcd$dbf


comp.2001  = net.change.df %>% filter(FromYear == 2001)

dev2001.nlcd$FIPS = dev2001.nlcd$GEOID

test = join(net.change.df,dev2001.nlcd)
test$nlcd.prop.dev = test$MEAN * 100

test[(test$Prop.Developed-test$nlcd.prop.dev)>30,]



(test$Prop.Developed-test$nlcd.prop.dev)[(test$Prop.Developed-test$nlcd.prop.dev)<0]


plot({test$Prop.Developed-test$nlcd.prop.dev}~test$AREA)
head(test)


class(dev2001.nlcd)



dev2001.nlcd$GEOID[dev2001.nlcd$GEOID %in% net.change.df$FIPS==FALSE]

names(dev2001.nlcd)
head(dev2001.nlcd)

head(cov2001.noaa)


head(dev2001.nlcd)

dev1992 = raster('Shapefiles/tf_dev_1992')

