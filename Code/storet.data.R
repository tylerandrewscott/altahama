#rm(list=ls())
library(plyr)
library(dplyr)

library(rgdal)
library(sp)
library(maptools)
library(rgeos)


counties = readOGR('Shapefiles','coastalcounties')


wholecounties = unionSpatialPolygons(counties,ifelse(as.numeric(as.character(counties$INTPTLON)) < (-100), 1,2),threshold = 100)

stations = read.delim('LargeInput/station.csv',sep=',')

stations = stations %>% filter(!is.na(LatitudeMeasure), !is.na(LongitudeMeasure),LongitudeMeasure!='NA',LatitudeMeasure!='NA')

stations$MonitoringLocationTypeName = as.character(stations$MonitoringLocationTypeName)
stations$MonitoringLocationTypeName[grep('Ocean',stations$MonitoringLocationTypeName)] = 'Ocean'
stations$MonitoringLocationTypeName[grep('Estuar',stations$MonitoringLocationTypeName)] = 'Estuary'
stations$MonitoringLocationTypeName[grep('Wetland',stations$MonitoringLocationTypeName)] = 'Wetland'
stations$MonitoringLocationTypeName[grep('Lake',stations$MonitoringLocationTypeName)] = 'Lake'
stations$MonitoringLocationTypeName[grep('River|Stream|stream|river',stations$MonitoringLocationTypeName)] = 'Stream/River'
stations$MonitoringLocationTypeName[grep('Pond',stations$MonitoringLocationTypeName)] = 'Pond'
stations$MonitoringLocationTypeName[grep('Canal',stations$MonitoringLocationTypeName)] = 'Canal'
stations = stations[grep('Floodwater|Site-Land',stations$MonitoringLocationTypeName,invert=T),]

stations = stations[stations$StateCode %in% 
                      c(2, 3, 7, 64, 14, 84, 86, 67, 89, 
                        68, 71, 70, 72, 74, 78, 79) == FALSE,]


stations = stations[grep('Guam|Puerto Rico|Samoa|Marshall Islands',stations$MonitoringLocationName,invert=T),]
stations = stations[grep('Guam|Samoa|Puerto Rico|Marshall Islands|Northern Mariana Islands',stations$OrganizationFormalName,invert=T),]
stations = stations[stations$HUCEightDigitCode %in% c(22010000,22020000,22030000,21020002,21020001) == FALSE,]
stations = stations[stations$HorizontalCoordinateReferenceSystemDatumName != 'GUAM',]
stations$LongitudeMeasure = -(abs(stations$LongitudeMeasure))
stations = stations[stations$LongitudeMeasure<(-5),]
switch.ll = stations[stations$LatitudeMeasure<0,]
stations = stations[stations$LatitudeMeasure>0,]
switch.ll = switch.ll %>% mutate(lat2 = abs(LongitudeMeasure),long2 = -abs(LatitudeMeasure)) %>%
  mutate(LongitudeMeasure = long2, LatitudeMeasure = lat2)
switch.ll$LongitudeMeasure[switch.ll$lat2 > 50] =  -abs(switch.ll$lat2[switch.ll$lat2 > 50])
switch.ll$LatitudeMeasure[switch.ll$lat2 > 50] =  abs(switch.ll$long2[switch.ll$lat2 > 50])
stations = switch.ll %>%dplyr::select(-long2,-lat2) %>% join(stations,.,type='full')

stations = stations[(abs(stations$LongitudeMeasure)<52 & abs(stations$LatitudeMeasure)<52) == FALSE ,]

switch.ll = stations[stations$LongitudeMeasure> -40,]
stations = stations[stations$LongitudeMeasure< -40,]
switch.ll = switch.ll %>% mutate(lat2 = abs(LongitudeMeasure),long2 = -abs(LatitudeMeasure)) %>%
  mutate(LongitudeMeasure = long2, LatitudeMeasure = lat2) %>%
 dplyr::select(-long2,-lat2) %>% join(stations,.,type='full')

stations = stations[stations$LongitudeMeasure< -60,]

stations = stations[(stations$LatitudeMeasure> 60 & stations$LongitudeMeasure>-120) == FALSE,]
stations = stations[(stations$LatitudeMeasure< 30 & stations$LongitudeMeasure<(-119) & stations$LongitudeMeasure>(-122))==FALSE ,]

stations = stations[stations$StateCode %in% c(32,4, 5, 8, 16, 19, 20, 21, 29, 30, 31, 32, 35, 38, 40, 
                                   46, 47, 49, 54, 56) == FALSE,]

station.spdf = SpatialPointsDataFrame(coords= cbind(stations$LongitudeMeasure,stations$LatitudeMeasure),
                                      data = stations, proj4string = CRS(proj4string(counties)))

is.over = over(station.spdf,wholecounties)

is.over[station.spdf@data$MonitoringLocationTypeName %in% c('Ocean','Estuary') & 
          station.spdf@data$LongitudeMeasure<(-10)] = 1 

is.over[station.spdf@data$MonitoringLocationTypeName %in% c('Ocean','Estuary') & 
          station.spdf@data$LongitudeMeasure>(-100)] = 2

station.spdf = station.spdf[!is.na(is.over),] 

is.over.county = over(station.spdf,coastcounty)
is.over.county$MonitoringLocationIdentifier = station.spdf@data$MonitoringLocationIdentifier
station.spdf@data = join(station.spdf@data,is.over.county)

# wq.atl = read.delim('LargeInput/wq_atl.tsv',sep = '\t')
# wq.atl = wq.atl %>%dplyr::select(OrganizationIdentifier,OrganizationFormalName,
#                            ActivityIdentifier,ActivityMediaName,
#                            ActivityStartDate,ActivityDepthHeightMeasure.MeasureValue,
#                            ActivityStartTime.Time,
#                            ActivityDepthHeightMeasure.MeasureUnitCode,
#                            MonitoringLocationIdentifier,CharacteristicName,ResultMeasureValue,
#                            ResultMeasure.MeasureUnitCode,USGSPCode)
# wq.atl = wq.atl[wq.atl$MonitoringLocationIdentifier %in% station.spdf@data$MonitoringLocationIdentifier,]
# write.csv(wq.atl,'LargeInput/wq.atl.scrubbed.csv')
# 
# 
# wq.ne = read.delim('LargeInput/wq_ne.tsv',sep = '\t')
# wq.ne = wq.ne %>%dplyr::select(OrganizationIdentifier,OrganizationFormalName,
#                            ActivityIdentifier,ActivityMediaName,
#                            ActivityStartDate,ActivityDepthHeightMeasure.MeasureValue,
#                            ActivityStartTime.Time,
#                            ActivityDepthHeightMeasure.MeasureUnitCode,
#                            MonitoringLocationIdentifier,CharacteristicName,ResultMeasureValue,
#                            ResultMeasure.MeasureUnitCode,USGSPCode)
# wq.ne = wq.ne[wq.ne$MonitoringLocationIdentifier %in% station.spdf@data$MonitoringLocationIdentifier,]
# write.csv(wq.ne,'LargeInput/wq.ne.scrubbed.csv')
# 
# wq.gulf = read.delim('LargeInput/wq_gulf.tsv',sep = '\t')
# wq.gulf = wq.gulf %>%dplyr::select(OrganizationIdentifier,OrganizationFormalName,
#                            ActivityIdentifier,ActivityMediaName,
#                            ActivityStartDate,ActivityDepthHeightMeasure.MeasureValue,
#                            ActivityStartTime.Time,
#                            ActivityDepthHeightMeasure.MeasureUnitCode,
#                            MonitoringLocationIdentifier,CharacteristicName,ResultMeasureValue,
#                            ResultMeasure.MeasureUnitCode,USGSPCode)
# wq.gulf = wq.gulf[wq.gulf$MonitoringLocationIdentifier %in% station.spdf@data$MonitoringLocationIdentifier,]
# write.csv(wq.gulf,'LargeInput/wq.gulf.scrubbed.csv')
# 
# 
# wq.mw = read.delim('LargeInput/wq_mw.tsv',sep = '\t')
# wq.mw = wq.mw %>%dplyr::select(OrganizationIdentifier,OrganizationFormalName,
#                            ActivityIdentifier,ActivityMediaName,
#                            ActivityStartDate,ActivityDepthHeightMeasure.MeasureValue,
#                            ActivityStartTime.Time,
#                            ActivityDepthHeightMeasure.MeasureUnitCode,
#                            MonitoringLocationIdentifier,CharacteristicName,ResultMeasureValue,
#                            ResultMeasure.MeasureUnitCode,USGSPCode)
# wq.mw = wq.mw[wq.mw$MonitoringLocationIdentifier %in% station.spdf@data$MonitoringLocationIdentifier,]
# write.csv(wq.mw,'LargeInput/wq.mw.scrubbed.csv')
# 
# wq.west = read.delim('LargeInput/wq_west.tsv',sep = '\t')
# wq.west = wq.west %>%dplyr::select(OrganizationIdentifier,OrganizationFormalName,
#                            ActivityIdentifier,ActivityMediaName,
#                            ActivityStartDate,ActivityDepthHeightMeasure.MeasureValue,
#                            ActivityStartTime.Time,
#                            ActivityDepthHeightMeasure.MeasureUnitCode,
#                            MonitoringLocationIdentifier,CharacteristicName,ResultMeasureValue,
#                            ResultMeasure.MeasureUnitCode,USGSPCode)
# wq.west = wq.west[wq.west$MonitoringLocationIdentifier %in% station.spdf@data$MonitoringLocationIdentifier,]
# write.csv(wq.west,'LargeInput/wq.west.scrubbed.csv')
# 


wq.west = read.csv('LargeInput/wq.west.scrubbed.csv')
wq.atl = read.csv('LargeInput/wq.atl.scrubbed.csv')
wq.ne = read.csv('LargeInput/wq.ne.scrubbed.csv')
wq.gulf = read.csv('LargeInput/wq.gulf.scrubbed.csv')
wq.mw = read.csv('LargeInput/wq.mw.scrubbed.csv')
wq.all = join_all(list(wq.west,wq.atl,wq.ne,wq.gulf,wq.mw),type='full')

wq.all = wq.all %>% mutate(ActivityStartDate = ymd(ActivityStartDate),
                  ResultMeasureValue = as.numeric(as.character(ResultMeasureValue)),
                  Year = year(ActivityStartDate)) %>%
  filter(!is.na(ResultMeasureValue)) %>% 
  mutate(UQ.Place.Date = paste(MonitoringLocationIdentifier,ActivityStartDate,sep='.'),
                           Day.Value = ((((ActivityStartDate - min(ActivityStartDate)) / 60) / 60) / 24)) %>%
dplyr::select(-X,-ActivityDepthHeightMeasure.MeasureValue,-ActivityDepthHeightMeasure.MeasureUnitCode)

wq.all = join(wq.all,station.spdf@data)


head(wq.all)
write.table(wq.all,'LargeInput/wq.all.scrubbed.txt',sep='\t')
write.csv(wq.all,'LargeInput/wq.all.scrubbed.csv')


#wq.all = read.csv('LargeInput/wq.all.scrubbed.csv')

library(lubridate)
library(INLA)

wq.all.do = wq.all %>% filter(CharacteristicName=='Dissolved oxygen (DO)') %>%
  mutate(ResultMeasureValue = as.numeric(as.character(ResultMeasureValue))) %>%
  filter(!is.na(ResultMeasureValue),ResultMeasureValue>0) %>% 
  mutate(sqrtResultMeasureValue = sqrt(ResultMeasureValue))

###Step 1) Prepare the mesh
mesh <- inla.mesh.2d(cbind(wq.all.do$LongitudeMeasure,wq.all.do$LatitudeMeasure), 
                     max.edge=c(100, 1000), cut=.05)

plot(mesh)

plot(mesh,asp=1)
###Step 2) Define the SPDE
spde <- inla.spde2.matern(mesh)
###Step 3) Create observation matrix $A$ for each year
table(repl <- test$Year-1998)#year index from 1:4
dim(A <- inla.spde.make.A(mesh, repl=repl, loc=cbind(test$LongitudeMeasure,test$LatitudeMeasure)))
###Step 4) Create index and stack data
ind <- inla.spde.make.index(name='s', n.spde=spde$n.spde, n.repl=length(unique(test$Year)))

# y is Greenland shark bycatch (counts)
# covariates are specified by data[,c(4:7)]
stke <- inla.stack(data=list(y=test$sqrtResultMeasureValue), 
                   tag='est', A=list(A, 1), 
                   effects=list(ind, 
                                data.frame(b0=1, test)))


###Step 5) Specify the INLA formula 
form <-  y ~ 0 + b0 + LatitudeMeasure + LongitudeMeasure + 
  f(MonitoringLocationTypeName,model='iid') + 
  f(Day.Value, model='rw2') + #inla.group reduce the number to unique values
  f(s, model=spde, replicate=s.repl) 

###Step 6) Call INLA
rl.nb2.gr <- inla(form, family='gaussian', 
                  data=inla.stack.data(stke), 
                  control.compute=list(dic=TRUE), verbose=T,
                  # control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
                  control.predictor=list(A=inla.stack.A(stke)))## Run the model remotely


#test = join(test,station.spdf@data)






tt = join(test,which.county.over)
tt = tt[tt$MonitoringLocationTypeName %in% c('Estuary','Stream/River'),]


test = join(tt,net.change.df)






















































####XXX

###Step 1) Prepare the mesh
mesh <- inla.mesh.2d(cbind(test$LongitudeMeasure, test$LatitudeMeasure), max.edge=c(1000, 100000), cut=.3)
#mesh <- inla.mesh.2d(boundary=county.bdry, cutoff=1, max.edge=c(20,200))
plot(mesh,asp=1)
###Step 2) Define the SPDE
spde <- inla.spde2.matern(mesh)
###Step 3) Create observation matrix $A$ for each year
dim(A <- inla.spde.make.A(mesh,  loc=cbind(test$LongitudeMeasure, test$LatitudeMeasure)))
###Step 4) Create index and stack data
ind <- inla.spde.make.index(name='s', n.spde=spde$n.spde)

# y is Greenland shark bycatch (counts)
# covariates are specified by data[,c(4:7)]
stke <- inla.stack(data=list(y=test$ResultMeasureValue), 
                   tag='est', A=list(A, 1), 
                   effects=list(ind, 
                                data.frame(test)))
n.data = length(test$ResultMeasureValue)

#model settings
correctionfactor = 10
pintercept = 0.001
DIC = TRUE
WAIC = TRUE
CPO = TRUE    

form.do <- y ~ -1 + LatitudeMeasure + LongitudeMeasure + f(MonitoringLocationTypeName,model='iid') + 
  f(Day.Value,model='rw2') + f(s, model=spde)
  #f(SAMPYEAR,model='iid') +
  #f(state.abb,model='iid') +
 # f(s, model=spde, extraconstr = list(A = as.matrix(t(Q)%*%A), e= rep(0,n.cov)))

mod.do<- inla(form.do, family='gaussian', 
                    data=inla.stack.data(stke), verbose=TRUE,
                    control.compute=list(dic=DIC,waic=WAIC,cpo=CPO), 
                    #control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
                    control.predictor=list(A=inla.stack.A(stke)),
              control.fixed= list(prec.intercept = pintercept,correlation.matrix = TRUE,
                                        expand.factor.strategy='model.matrix'),
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))

summary(mod)






##################################
### MAP of random field mean and sd (Figure 6)
##################################
library(lattice)

proj<-inla.mesh.projector(mesh,xlim=range(test$LongitudeMeasure), ylim=range(test$LongitudeMeasure))
str(proj)#matrix 100 x 100
dat = as.data.frame(proj$lattice$loc)
dim(dat)#10000rows
table(gr1$year)

ndat<-data.frame(sapply(dat, rep.int, times=1))
# dim(ndat)#110000, replicated 4 times for each year
ndat$year<-rep(1:1,each=10000)
table(ind$s.repl)#476 spde nodes every year
length(mod.do$summary.random$s$mean)#1904/4

xmeans <- list()
xsd <- list()
for (j in 1:1) {
  xmeans[[j]] <- inla.mesh.project(proj,mod.do$summary.random$s$mean[ind$s.repl==j])
  xsd[[j]] <- inla.mesh.project(proj,mod.do$summary.random$s$sd[ind$s.repl==j])
}


test = test[test$LongitudeMeasure>(-140),]
plot(test$LatitudeMeasure~test$LongitudeMeasure)
mesh = inla.mesh.create.helper(
  points=cbind(test$LongitudeMeasure,test$LatitudeMeasure), #points.domain=sic.borders,
  #offset=c(5, 20), 
  max.edge=c(1000,1000))
mesh <- inla.mesh.2d(cbind(test$LongitudeMeasure, test$LatitudeMeasure),
                     max.edge=c(1000, 100000), cut=.3)
#plot(mesh)
spde = inla.spde2.matern(mesh=mesh)

field.indices = inla.spde.make.index("field", n.spde=mesh$n)

n_days = length(unique(test$Day.Value))
A.est = inla.spde.make.A(mesh,
                         loc=as.matrix(cbind(test$LongitudeMeasure, test$LatitudeMeasure)),
                         group=test$Day.Value,
                         n.group=n_days
                         )



stack.est = inla.stack(data = list(y=test$ResultMeasureValue),
                       A = list(A.est, 1),
                       effects = list(c(field.indices, list(Intercept=1)),
                                      list(Latitude=test$LatitudeMeasure,LongitudeMeasure=test$LongitudeMeasure)),
                       tag="est")


ggplot(test[test$LongitudeMeasure>-140,]) + geom_point(aes(x=LongitudeMeasure,y=LatitudeMeasure,colour=log(ResultMeasureValue)),pch=19) + scale_colour_distiller(type='div')

summary(test$ResultMeasureValue[test$ResultMeasureValue<50])

plot(test$LatitudeMeasure~test$LongitudeMeasure)

table(test$ResultMeasureValue<0)

densityplot(sqrt(test$ResultMeasureValue))



table(test$ResultMeasureValue>10)
ndat <- data.frame(ndat, unlist(xmeans),unlist(xsd)) 
head(ndat)
ndat$year <- factor(ndat$year+2007)
colnames(ndat)[4:5]<-c("mean","sd")


my.at <- seq(-3500,0,500)
levelplot(mean ~  V1*V2 | year, data=ndat,xlab='', ylab='',
          col.regions=gray.colors(16,start=1,end=0),scale=list(draw=FALSE),par.strip.text=list(cex=2),strip = strip.custom(bg="white")) 



  latticeExtra::layer(sp::sp.polygons(ca.km,fill="black"))



+rasterVis::contourplot(r.km, labels=list(cex=0.5), at=my.at,col="black")



# pdf("Figures/MeanGRF.pdf")
my.at <- seq(-3500,0,500)
levelplot(mean ~  V1*V2 | year, data=ndat,xlab='', ylab='',
          col.regions=gray.colors(16,start=1,end=0),scale=list(draw=FALSE),par.strip.text=list(cex=2),strip = strip.custom(bg="white")) +  latticeExtra::layer(sp::sp.polygons(ca.km,fill="black"))+rasterVis::contourplot(r.km, labels=list(cex=0.5), at=my.at,col="black")
# dev.off()

# pdf("Figures/SDGRF.pdf")
levelplot(sd ~  V1*V2 | year, data=ndat,xlab='', ylab='',
          col.regions=gray.colors(16,start=1,end=0),scale=list(draw=FALSE),par.strip.text=list(cex=2),strip = strip.custom(bg="white")) +  latticeExtra::layer(sp::sp.polygons(ca.km,fill="black"))+rasterVis::contourplot(r.km, labels=list(cex=0.5), at=my.at,col="black")
# dev.off()

##########################
### PREDICTIONS
##########################
dim(bgrid <- read.table('Data/gridpred.txt', header=TRUE)) #Grid data
head(bgrid,2)

par(mfrow=c(1,1))
with(data, plot(X, Y, asp=1))
points(bgrid$x, bgrid$y, col=gray(.7), pch=4, cex=0.01)

#*Grid locations* We used our final model to predict expected bycatch in areas 
#neigbouring observed fishing hauls. However, since the fishery expended 
#spatially, the spatial fishing pattern changed over time with some years with 
#no fishing in some areas of our domain (e.g., northeast cluster at 71 degrees 
#was not fished in 2008 and 2009). For these area-time, we did not predict
#bycatch. However, note that it would be possible to do so (assuming some
#average fishing conditions (duration, Ngillnets, TC.tspp), but the spatial
#random effect would be very close to zero and with large variance. We selected
#nearest neighbour grid points less than 30km from our fishing hauls
#using nndistF function from [splancs].

library(splancs) 
iig <- sapply(2008:2011, function(y) {
  ii.y <- which(data$year==y)
  d <- nndistF(cbind(data$X[ii.y], data$Y[ii.y]), cbind(bgrid$x, bgrid$y))
  which(d<30)
})
str(iig)

# *Prediction of Greenland halibut (TC.tspp)* Since Greenland halibut catch (kg)
# are driven by unknown variable, we also interpolated TC.tspp over the grid for
# each year. The interpolation was done using a weigthed mean, where the weights
# are proportional to exp(-dist/a), where 'a' is equal to 0.5km, that is inversely
# proportional to the total grid distance (1x1km).

library(geoR)
itc <- lapply(1:4, function(y) {
  ii.y <- which(data$year==((2008:2011)[y]))
  d <- loccoords(cbind(bgrid$x, bgrid$y)[iig[[y]],], 
                 cbind(data$X, data$Y)[ii.y,]) 
  w <- exp(-d/0.5)
  drop((w/rowSums(w))%*%(data$TC.tspp[ii.y]))
})
sapply(itc, summary)
summary(data$TC)




drop(A5pts3%*%res5$summary.random$i$mean)


dim(A5pts3 <- inla.spde.make.A(mesh, loc=cbind(test$LongitudeMeasure,test$LatitudeMeasure)))

stk5p.rf <- inla.stack(data=list(resp=NA), A=list(A5pts3),
                       effects=list(i=1:spde$n.spde,LatitudeMeasure = test$LatitudeMeasure,LongitudeMeasure = test$LongitudeMeasure,
                                    Day.Value = test$Day.Value,MonitoringLocationTypeName= test$MonitoringLocationTypeName), tag='prd5r')

stk.jp <- inla.stack(stke, stk5p.rf)



res5p <- inla(y ~ -1 + LatitudeMeasure + LongitudeMeasure + f(MonitoringLocationTypeName,model='iid') +  f(Day.Value,model='rw2') + f(s, model=spde),
              data=inla.stack.data(stk.jp),
              control.predictor=list(A=inla.stack.A(stk.jp), compute=TRUE),
              control.compute=list(dic=DIC,waic=WAIC,cpo=CPO), 
              #control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
              control.fixed= list(prec.intercept = pintercept,correlation.matrix = TRUE,
                                  expand.factor.strategy='model.matrix'),
              control.inla = list(
                correct = TRUE,
                correct.factor = correctionfactor))




pgrid0 <- inla.mesh.projector(mesh, xlim=0:1, ylim=0:1, dims=c(101,101))

mod.do$summary.fix

prd0.m <- inla.mesh.project(pgrid0, mod.do$summary.random)
prd0.s <- inla.mesh.project(pgrid0, res5$summary.ran$i$s)










table(year(test$ActivityStartDate))
ttt = data.frame(table(as.character(test$MonitoringLocationIdentifier)))

table(duplicated(paste(test$LongitudeMeasure,test$LatitudeMeasure)))

plot(test$LatitudeMeasure~test$LongitudeMeasure)
plot(coastcounty,add=TRUE)
table(duplicated())



dev1992 = read.dbf('../duckabush/dev_county_1992.dbf')
dev2001 = read.dbf('../duckabush/dev_county_2001.dbf')

dev2001 = dev2001$dbf
dev2001$Perc.Dev = dev2001$MEAN * 100

head(dev2001)

uscounties = readOGR('../duckabush/','tl_2013_us_county')
dev2001$FIPS = paste0(uscounties@data$STATEFP,dev2001$COUNTYFP)


uscounties
plot(uscounties)




head(dev2001)
c2001 = net.change.df[net.change.df$FromYear==2001,]

head(c2001)

head(dev1992)






table(wq.west$CharacteristicName)





# Supplementary Materials: Applying Bayesian spatio-temporal models to fisheries
# bycatch in the Canadian Arctic R codes for the final model, inferences, and
# predictions

### Please refer to the INLA website: www.r-inla.org for additional information
rm(list = ls(all = TRUE))
##################################
### MAP of BAFFIN BAY (Figure 1a)
##################################
library(sp)
library(maptools)
library(spatstat)
library(rgdal)
library(reshape2)
library(spdep)
library(maps)
library(mapdata)
library(marmap)

setwd('H:/fish')

# load datasets
data <- read.table('Data/fackdata.txt', header=TRUE) #Greenland shark bycatch data
eez <- readOGR(dsn="Data/EEZ", layer='Atleez') # Canadian eez boundary


MPAlat<-c(68.25,68.25,67.25,67.25,68.25)
MPAlong<-c(-58.551306,-60.5,-60.5,-57.8425,-58.551306)
mpa<-cbind(MPAlong,MPAlat)

ca<-map("worldHires", "Canada", fill=TRUE, col="transparent", plot=FALSE)# in package "mapdata"
IDs <- sapply(strsplit(ca$names, ":"), function(x) x[1])
ca.sp <- map2SpatialPolygons(ca, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

Greenland<-map("worldHires", "Greenland", fill=TRUE, col="transparent", plot=FALSE)##in package "mapdata"
IDs <- sapply(strsplit(Greenland$names, ":"), function(x) x[1])
GR.sp <- map2SpatialPolygons(Greenland, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

iso<-read.bathy("Data/etopo1_bedrock.xyz",sep=" ") # read.bathy {marmap} data from NOAA: http://maps.ngdc.noaa.gov/viewers/wcs-client/

r1 <- as.raster(iso) # {marmap}
newproj <- "+proj=longlat +datum=WGS84"
# Note: package issue with marmap & raster package. Need to make sure that the raster package is not uploaded before using the as.raster() command from marmap

library(lattice)
library(raster)
r2 <- projectRaster(r1,crs=newproj)

data.sp<-data
coordinates(data.sp) <- ~longitude+latitude

# pdf(file="Figures/BaffinBayMap.pdf")
plot(ca.sp,xlim=c(-65,-55),ylim=c(66,71),col="lightgrey",axes=T,border="grey")# Canada land #,xaxt="n",yaxt="n"
plot(GR.sp,xlim=c(-65,-55),ylim=c(66,71),col="lightgrey",axes=T,border="grey",add=T) # Greenland land
contour(r2,levels=c(-500,-1000,-1500),col="black",add=T) #bathymetry
plot(eez,col="black",lty=4,add=T,lwd=2) # Canada's Eclusive Economic Zone
lines(mpa) # marine protected area zone
points(data.sp,pch=20) # fishing locations
# dev.off()

##########################
### SPDE and INLA approach
##########################

# save(rl.nb2.gr,file="outputs/rl.nb2.gr.Rdata")



##########################
### RESULTS 
##########################
#Extract parameters of the random field
rf.gr <- inla.spde2.result(rl.nb2.gr, 's', spde) 
c(mean=inla.emarginal(function(x) x, rf.gr$marginals.range[[1]]), 
  q=inla.hpdmarginal(0.95, rf.gr$marginals.range[[1]]))[c(2,1,3)]

#Summary table of the posterior distributions of all parameters (fixed and random)
tables <- t(sapply(c(rl.nb2.gr$marginals.fix, 
                     rl.nb2.gr$marginals.hy[1:2], 
                     rf.gr$marginals.range[1]), function(m) 
                       c(mean=inla.emarginal(function(x) x, m), 
                         lim=inla.hpdmarginal(0.95, m)))) 
round(tables, 4)

# Relative effect of the fixed effects
# Duration in hours and decimal minutes
exp(rl.nb2.gr$summary.fix[2,1])  
# Ngillnets - number of gillnet panels per haul
exp(rl.nb2.gr$summary.fix[3,1])  
# TC.tspp - Greenland halibut cacth weight (in metric tonnes)
exp(rl.nb2.gr$summary.fix[4,1])

##################################
### MAP of random field mean and sd (Figure 6)
##################################
library(lattice)
head(gr1)
proj<-inla.mesh.projector(mesh,xlim=range(test$LongitudeMeasure), ylim=range(test$LatitudeMeasure))
str(proj)#matrix 100 x 100
dat = as.data.frame(proj$lattice$loc)
dim(dat)#10000rows
table(test$Year)

ndat<-data.frame(sapply(dat, rep.int, times=length(unique(test$Year))))
# dim(ndat)#110000, replicated 4 times for each year
ndat$year<-rep(1:3,each=10000)
table(ind$s.repl)#476 spde nodes every year
length(rl.nb2.gr$summary.random$s$mean)#1904/4

xmeans <- list()
xsd <- list()
for (j in 1:3) {
  xmeans[[j]] <- inla.mesh.project(proj,rl.nb2.gr$summary.random$s$mean[ind$s.repl==j])
  xsd[[j]] <- inla.mesh.project(proj,rl.nb2.gr$summary.random$s$sd[ind$s.repl==j])
}

ndat <- data.frame(ndat, unlist(xmeans),unlist(xsd)) 
head(ndat)
ndat$year <- factor(ndat$year+1998)
colnames(ndat)[4:5]<-c("mean","sd")

# pdf("Figures/MeanGRF.pdf")
my.at <- seq(-3500,0,500)
levelplot(mean ~  V1*V2 | year, data=ndat,xlab='', ylab='',
         # col.regions=gray.colors(16,start=1,end=0),
          scale=list(draw=FALSE),par.strip.text=list(cex=2),strip = strip.custom(bg="white")) 

levelplot(sd ~  V1*V2 | year, data=ndat,xlab='', ylab='',
          #col.regions=gray.colors(16,start=1,end=0),
          scale=list(draw=FALSE),par.strip.text=list(cex=2),strip = strip.custom(bg="white")) 
  



  latticeExtra::layer(sp::sp.polygons(ca.km,fill="black"))+
  rasterVis::contourplot(r.km, labels=list(cex=0.5), at=my.at,col="black")

  
tapply(test$ResultMeasureValue,test$MonitoringLocationTypeName,mean)
table(test$MonitoringLocationTypeName)

gray.colors(16,start=1,end=0)
?gray.colors
palette(rainbow(6))
+ 
  
  latticeExtra::layer(sp::sp.polygons(ca.km,fill="black"))+
  rasterVis::contourplot(r.km, labels=list(cex=0.5), at=my.at,col="black")
# dev.off()

# pdf("Figures/SDGRF.pdf")

# dev.off()


##########################
### PREDICTIONS
##########################
dim(bgrid <- read.table('Data/gridpred.txt', header=TRUE)) #Grid data
head(bgrid,2)

par(mfrow=c(1,1))
with(data, plot(X, Y, asp=1))
points(bgrid$x, bgrid$y, col=gray(.7), pch=4, cex=0.01)

#*Grid locations* We used our final model to predict expected bycatch in areas 
#neigbouring observed fishing hauls. However, since the fishery expended 
#spatially, the spatial fishing pattern changed over time with some years with 
#no fishing in some areas of our domain (e.g., northeast cluster at 71 degrees 
#was not fished in 2008 and 2009). For these area-time, we did not predict
#bycatch. However, note that it would be possible to do so (assuming some
#average fishing conditions (duration, Ngillnets, TC.tspp), but the spatial
#random effect would be very close to zero and with large variance. We selected
#nearest neighbour grid points less than 30km from our fishing hauls
#using nndistF function from [splancs].

library(splancs) 
iig <- sapply(2008:2011, function(y) {
  ii.y <- which(data$year==y)
  d <- nndistF(cbind(data$X[ii.y], data$Y[ii.y]), cbind(bgrid$x, bgrid$y))
  which(d<30)
})
str(iig)

# *Prediction of Greenland halibut (TC.tspp)* Since Greenland halibut catch (kg)
# are driven by unknown variable, we also interpolated TC.tspp over the grid for
# each year. The interpolation was done using a weigthed mean, where the weights
# are proportional to exp(-dist/a), where 'a' is equal to 0.5km, that is inversely
# proportional to the total grid distance (1x1km).

library(geoR)
itc <- lapply(1:4, function(y) {
  ii.y <- which(data$year==((2008:2011)[y]))
  d <- loccoords(cbind(bgrid$x, bgrid$y)[iig[[y]],], 
                 cbind(data$X, data$Y)[ii.y,]) 
  w <- exp(-d/0.5)
  drop((w/rowSums(w))%*%(data$TC.tspp[ii.y]))
})
sapply(itc, summary)
summary(data$TC)

### Building prediction stacks
### for each year with
###  duration=15.94, Ngillnets=43.44
###  bathymetry over grid and TC.tspp interpolated
y.stk.p <- lapply(1:4, function(y) {
  cat('year', (2008:2011)[y], '\n')
  Ap <- inla.spde.make.A(mesh, cbind(
    bgrid$x[iig[[y]]], bgrid$y[iig[[y]]]), repl=y, n.repl=4)
  inla.stack(data=list(gr=NA), 
             tag=paste('prd', y, sep=''), A=list(Ap, 1), 
             effects=list(ind, 
                          data.frame(b0=1, bathymetry=bgrid$bathymetry[iig[[y]]],
                                     duration=15.98, Ngillnets=43.44,
                                     TC.tspp=itc[[y]])))
})

### we can join all stack data into one and run it all at once, 
### but this is expensive. So we use each prediction stack per year.
yprd <- lapply(y.stk.p, function(s) {
  stk <- inla.stack(stke, s) 
  res <- inla(form, family='zeroinflatednbinomial2', 
              data=inla.stack.data(stk), quantiles=NULL, 
              control.predictor=list(A=inla.stack.A(stk), compute=TRUE, quantiles=NULL), 
              control.compute=list(return.marginals=FALSE), 
              inla.call='remote')
  print(res$cpu)
  ii <- inla.stack.index(stk, names(s$effects$index))$data 
  res$summary.linear.pred[ii,]
})

### join all into data.frame
jprd <- Reduce('rbind', lapply(1:4, function(y) data.frame(
  x=bgrid$x[iig[[y]]], y=bgrid$y[iig[[y]]], lpred=yprd[[y]]$mean, year=(2008:2011)[y])))

write.table(jprd, 'jprd.txt', row.names=FALSE) #save predictions

##################################
### MAPs of Prediction (Figure 7)
##################################
#read the prediction outputs
jprd <- read.table('jprd.txt',header=T)
jprd.y <- split(jprd, jprd$year)
str(jprd.y)

proj <- "+proj=utm +zone=19 +ellps=GRS80 +units=km +no_defs" #old projection
res0 <- lapply(jprd.y, function(x) SpatialPoints(cbind(x$x, x$y), CRS(proj)))
##res0 <- sp:::as.SpatialPolygons.SpatialPixels(SpatialPixels(pts))##, data.frame(pred=exp(jprd$lpred)))
##plot(SpatialPixels(pts))

newproj <- "+proj=longlat +zone=19 +ellps=GRS80" # lat/long
res <- lapply(1:length(res0), function(i)
  SpatialPointsDataFrame(spTransform(res0[[i]], CRS(newproj)),
                         data.frame(pred=exp(jprd.y[[i]]$lpred))))

q <- c(0, 1, 3, 5, 10, 30)
leg <- paste(paste(c('<', q[2:4], '>'), colapse='', sep=''),
             c('', rep('-', 3), ''),
             paste(c(q[2:5], q[5]), colapse='', sep=''), sep='')

# jpeg("Figures/predictions.jpeg")
par(mfrow=c(2,2), mar=c(2,2,0.5, 0.5), mgp=c(2,1,0))#mgp:affect the margin line for the axis
for (i in 1:4) {
  if (i != 3){
    plot(ca.sp,xlim=c(-65,-55),ylim=c(66,71),col="white",axes=T,border="lightgrey",xaxt="n",yaxt="n")# Canada land
    plot(GR.sp,xlim=c(-65,-55),ylim=c(66,71),col="white",axes=T,border="grey",add=T) # Greenland land
    contour(r1,levels=c(-500,-1000,-1500),col="black",add=T) #bathymetry
    plot(eez,col="black",lty=4,add=T,lwd=2) # Canada's Eclusive Economic Zone
    lines(mpa) # marine protected area zone
    ctest<-c("#C0BCBC","#7F7777","#585353","#373535","#0D0C0C")
    plot(res[[i]], add=T, col=ctest[findInterval(res[[i]]$pred, q)], cex=0.1, pch=19)
    #     legend(-50.5, 71, leg, fill=ctest)
    text(-65,66,labels=eval(i+2007),font=2,cex=1.2)
  }
  else {
    plot(ca.sp,xlim=c(-65,-55),ylim=c(66,71),col="white",axes=T,border="grey")# to remove x axis # Canada land
    plot(GR.sp,xlim=c(-65,-55),ylim=c(66,71),col="white",axes=T,border="grey",add=T) # Greenland land
    contour(r1,levels=c(-500,-1000,-1500),col="black",add=T) #bathymetry
    plot(eez,col="black",lty=4,add=T,lwd=2) # Canada's Eclusive Economic Zone
    lines(mpa) # marine protected area zone
    plot(res[[i]], add=T, col=ctest[findInterval(res[[i]]$pred, q)], cex=0.1, pch=19)
    text(-65,66,labels=eval(i+2007),font=2,cex=1.2)
    legend(-57, 71, leg, fill=ctest,bg="white")
  }
}
# dev.off()






