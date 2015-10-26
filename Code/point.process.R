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
nca.site$CFIPS[nchar(nca.site$CFIPS)==4] = paste0(0,nca.site$CFIPS[nchar(nca.site$CFIPS)==4])



nca.chem = read.csv('Input/nca_waterchemdata_narschallenge.csv')
names(nca.chem)[grep('Light.trans',names(nca.chem))] = 'Light.transmittance'
nca.chem <- nca.chem %>% mutate(chlor.std = as.vector(scale(Chla.ug.L)),nit.std = as.vector(scale(DIN.mg.N.L)), pho.std = as.vector(scale(DIN.mg.N.L)), 
                                do.std = as.vector(scale(DO.mg.L)), 
                                trans.std = as.vector(scale(Light.transmittance))) 

nca.chem$chem.index = rowSums(nca.chem[c('chlor.std','nit.std','pho.std','do.std','trans.std')],na.rm=T) / rowSums(!is.na(nca.chem[c('chlor.std','nit.std','pho.std','do.std','trans.std')]))


nca.chem$CFIPS = nca.site$CFIPS[match(nca.chem$SITE_ID,nca.site$SITE_ID)]
nca.chem$county.name = nca.site$NAME[match(nca.chem$SITE_ID,nca.site$SITE_ID)]
nca.chem$state.abb = nca.site$PSTL_CODE[match(nca.chem$SITE_ID,nca.site$SITE_ID)]

nca.chem$county.name[is.na(nca.chem$CFIPS) & nca.chem$ESTUARY=='Apalachee Bay'] = 'Wakulla'
nca.chem$CFIPS[is.na(nca.chem$CFIPS) & nca.chem$ESTUARY=='Apalachee Bay'] = 12129

nca.chem$county.name[is.na(nca.chem$CFIPS) & nca.chem$ESTUARY=='Florida Bay'] = 'Monroe'
nca.chem$CFIPS[is.na(nca.chem$CFIPS) & nca.chem$ESTUARY=='Florida Bay'] = 12087


county.pop.1990 = county.pop.1990 %>% dplyr::select(-X1990B)
long.pop.1990 = gather(county.pop.1990,Year,Population,-FIPS,-Name)
long.pop.1990$Year = gsub('X','',long.pop.1990$Year)
long.pop.1990$FIPS[nchar(long.pop.1990$FIPS)==4] = paste0('0',long.pop.1990$FIPS[nchar(long.pop.1990$FIPS)==4])



nca.chem$county.pop =   long.pop.1990$Population[match(paste(nca.chem$CFIPS,nca.chem$SAMPYEAR),paste(long.pop.1990$FIPS,long.pop.1990$Year))]

county.pop.2000$FIPS[nchar(county.pop.2000$FIPS)==4] = paste0('0',county.pop.2000$FIPS[nchar(county.pop.2000$FIPS)==4])
nca.chem$county.pop[is.na(nca.chem$county.pop)] =  county.pop.2000$Pop.2000[match(nca.chem$CFIPS[is.na(nca.chem$county.pop)],county.pop.2000$FIPS)]
nca.chem$county.pop = as.numeric(nca.chem$county.pop)

nca.chem$state.gdp.pc.1k = focal.change.df$state.gdp.pc.1k[match(nca.chem$state.abb,focal.change.df$StateAbbrev)]

lc96 = read.csv('Input/LandCover1996.csv')
lc01 = read.csv('Input/LandCover2001.csv')
lc06 = read.csv('Input/LandCover2006.csv')
lc10 = read.csv('Input/LandCover2010.csv')
lc = join_all(list(lc96,lc01,lc06,lc10),type='full')

lc$CFIPS = lc$FIPS
lc$CFIPS[nchar(lc$CFIPS)==4] = paste0(0,lc$CFIPS[nchar(lc$CFIPS)==4])

temp = lc %>% filter(Year == 1996) %>% group_by(CFIPS) %>% summarise(TotalArea = sum(SquareMiles)) %>% as.data.frame(.)
nca.chem$TotalArea = temp$TotalArea[match(nca.chem$CFIPS,temp$CFIPS)]

temp = lc %>% filter(LCName %in% grep('Cultivated|Pasture',uq.classes,value=TRUE)) %>% group_by(Year,CFIPS) %>% summarise(ag.area = sum(SquareMiles)) %>% as.data.frame(.)
nca.chem$ag.area = NA
for (i in 1:nrow(nca.chem))
{
  t = temp[temp$Year<nca.chem$SAMPYEAR[i],]  
  nca.chem$ag.area[i] <- t$ag.area[match(nca.chem$CFIPS[i],t$CFIPS)]
}

temp = lc %>% filter(LCName %in% grep('Developed',uq.classes,value=TRUE)) %>% group_by(Year,CFIPS) %>% summarise(dev.area = sum(SquareMiles)) %>% as.data.frame(.)
nca.chem$dev.area = NA
for (i in 1:nrow(nca.chem))
{
  t = temp[temp$Year<nca.chem$SAMPYEAR[i],]  
  nca.chem$dev.area[i] <- t$dev.area[match(nca.chem$CFIPS[i],t$CFIPS)]
}

temp = lc %>% filter(LCName %in% grep('Forest',uq.classes,value=TRUE)) %>% group_by(Year,CFIPS) %>% summarise(forst.area = sum(SquareMiles)) %>% as.data.frame(.)
nca.chem$forst.area = NA
for (i in 1:nrow(nca.chem))
{
  t = temp[temp$Year<nca.chem$SAMPYEAR[i],]  
  nca.chem$forst.area[i] <- t$forst.area[match(nca.chem$CFIPS[i],t$CFIPS)]
}


nca.chem$county.ag.prop = nca.chem$ag.area/nca.chem$TotalArea
nca.chem$county.forst.prop = nca.chem$forst.area/nca.chem$TotalArea
nca.chem$county.dev.prop = nca.chem$dev.area/nca.chem$TotalArea

nca.chem$Cond.Approval.Active = ifelse(nca.chem$SAMPYEAR > year(cnp.history$Conditional[match(nca.chem$state.abb,cnp.history$state.abb)]),1,0)
nca.chem$Full.Approval.Active = ifelse(nca.chem$SAMPYEAR > year(cnp.history$Full[match(nca.chem$state.abb,cnp.history$state.abb)]),1,0)
nca.chem$Cond.Approval.Active[is.na(nca.chem$Cond.Approval.Active)] = 0
nca.chem$Full.Approval.Active[is.na(nca.chem$Full.Approval.Active)] = 0



nca.chem$State.Name = state.ref$state.name[match(nca.chem$state.abb,state.ref$state.abb)]

nca.chem  = join(nca.chem,plan.attributes)

nca.chem$Coord.Institution.Active <- ifelse(year(nca.chem$Coord.Institution)<=nca.chem$SAMPYEAR,1,0)
nca.chem$Coord.Institution.Active[is.na(nca.chem$Coord.Institution)] <- 0 

nca.chem$Coord.Spec.Responsibilities.Active <- ifelse(year(nca.chem$Coord.Spec.Responsibilities)<=nca.chem$SAMPYEAR,1,0)
nca.chem$Coord.Spec.Responsibilities.Active[is.na(nca.chem$Coord.Spec.Responsibilities)] <- 0 

nca.chem$Coord.Formal.Agreements.Active <- ifelse(year(nca.chem$Coord.Formal.Agreements)<=nca.chem$SAMPYEAR,1,0)
nca.chem$Coord.Formal.Agreements.Active[is.na(nca.chem$Coord.Formal.Agreements)] <- 0 


nca.chem$Part.Input.Active <- ifelse(year(nca.chem$Part.Input)<=nca.chem$SAMPYEAR,1,0)
nca.chem$Part.Input.Active[is.na(nca.chem$Part.Input)] <- 0 

nca.chem$Part.Advisory.Active <- ifelse(year(nca.chem$Part.Advisory)<=nca.chem$SAMPYEAR,1,0)
nca.chem$Part.Advisory.Active[is.na(nca.chem$Part.Advisory)] <- 0 

nca.chem$Part.Outreach.Active <- ifelse(year(nca.chem$Part.Outreach)<=nca.chem$SAMPYEAR,1,0)
nca.chem$Part.Outreach.Active[is.na(nca.chem$Part.Outreach)] <- 0 


nca.chem$abs.year = nca.chem$SAMPYEAR - 1998

nca.chem$county.pop = nca.chem$county.pop / 100000


#bound1 <- inla.nonconvex.hull(cbind(nca.chem$LON_DD,nca.chem$LAT_DD),convex=-.05)
#m10 <- inla.mesh.2d(boundary=bound1, cutoff=.1, max.edge=c(20,200))
#plot(m10)


### Water Quality Models

wholecounties = unionSpatialPolygons(counties,ifelse(as.numeric(as.character(counties$INTPTLON)) < (-100), 1,2),threshold = 100)
county.bdry <- inla.sp2segment(wholecounties)
m10 <- inla.mesh.2d(boundary=county.bdry, cutoff=.1, max.edge=c(20,200))
#plot(m10)

spde.a <- inla.spde2.matern(m10)
nca.chem = nca.chem %>% filter(!is.na(county.pop),!is.na(county.forst.prop),!is.na(county.ag.prop),!is.na(nca.chem$abs.year))

# Model 1: constant spatial effect
A.1 <- inla.spde.make.A(m10, 
                        loc=cbind(nca.chem$LON_DD,nca.chem$LAT_DD))
ind.1 <- inla.spde.make.index('s', m10$n)
n.data = length(nca.chem$chem.index)
X = cbind(rep(1,n.data), nca.chem$county.pop,
          nca.chem$county.forst.prop,
          nca.chem$county.dev.prop,
          nca.chem$county.ag.prop,
          nca.chem$state.gdp.pc.1k,
          nca.chem$abs.year)

stk.1 <- inla.stack(data=list(y=nca.chem$chem.index), A=list(A.1,1),
                    effects=list(ind.1, 
                                 list(data.frame(b0=1,nca.chem))))


#covars$YEARS.ACTIVE)
n.cov = ncol(X)
Q = qr.Q(qr(X))

#model settings
correctionfactor = 10
pintercept = 0.001
DIC = TRUE
WAIC = TRUE
CPO = TRUE




form.cond.wq <- y ~ 0 + county.forst.prop + county.ag.prop + county.dev.prop + county.pop + abs.year + state.gdp.pc.1k + 
  Cond.Approval.Active + 
  f(SAMPYEAR,model='iid') +
  f(state.abb,model='iid') +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.cov)))


mod.cond.wq <- inla(form.cond, family='gaussian',
                    data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), 
                                           compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))


form.full.wq <- y ~ 0 + county.forst.prop + county.ag.prop + county.dev.prop + county.pop + abs.year + 
  Full.Approval.Active + 
  f(SAMPYEAR,model='iid') +
  f(state.abb,model='iid') +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.cov)))

mod.full.wq <- inla(form.full, family='gaussian',
                    data=inla.stack.data(stk.1),
                    control.predictor=list(A=inla.stack.A(stk.1), 
                                           compute=TRUE),
                    #  control.inla=list(strategy='laplace'), 
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))

form.collab.wq <- y ~ 0 + county.forst.prop + county.ag.prop + county.dev.prop + county.pop + abs.year + 
  Full.Approval.Active + 
  as.factor(Part.Outreach.Active)+
  as.factor(Part.Advisory.Active)+
  as.factor(Part.Input.Active) +
  as.factor(Part.Outreach.Active):as.factor(Part.Advisory.Active) +
  as.factor(Part.Input.Active):as.factor(Part.Advisory.Active) +
  as.factor(Part.Outreach.Active):as.factor(Part.Input.Active) +
  as.factor(Coord.Formal.Agreements.Active) + 
  as.factor(Coord.Spec.Responsibilities.Active) + 
  as.factor(Coord.Institution.Active)+
  as.factor(Coord.Formal.Agreements.Active):as.factor(Coord.Spec.Responsibilities.Active) + 
  as.factor(Coord.Institution.Active):as.factor(Coord.Spec.Responsibilities.Active)+
  as.factor(Coord.Institution.Active):as.factor(Coord.Formal.Agreements.Active)+
  f(SAMPYEAR,model='iid') +
  f(state.abb,model='iid') +
  f(s, model=spde.a, extraconstr = list(A = as.matrix(t(Q)%*%A.1), e= rep(0,n.cov)))

mod.collab.wq <- inla(form.collab.wq, family='gaussian',
                      data=inla.stack.data(stk.1),
                      control.predictor=list(A=inla.stack.A(stk.1), 
                                             compute=TRUE),
                      #  control.inla=list(strategy='laplace'), 
                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                      verbose=T,
                      control.inla = list(
                        correct = TRUE,
                        correct.factor = correctionfactor))

