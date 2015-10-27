rm(list=ls())

onscreen = TRUE

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


state.ref = data.frame(state.abb,state.name)

nca.chem$county.name[is.na(nca.chem$CFIPS) & nca.chem$ESTUARY=='Apalachee Bay'] = 'Wakulla'
nca.chem$CFIPS[is.na(nca.chem$CFIPS) & nca.chem$ESTUARY=='Apalachee Bay'] = 12129

nca.chem$county.name[is.na(nca.chem$CFIPS) & nca.chem$ESTUARY=='Florida Bay'] = 'Monroe'
nca.chem$CFIPS[is.na(nca.chem$CFIPS) & nca.chem$ESTUARY=='Florida Bay'] = 12087


#ADd county population change
county.pop.2011 = read.csv('Input/county.acs.2011.csv',skip=1)
colnames(county.pop.2011) <- c('Id','FIPS','Geog','Pop.2011','Drop');county.pop.2011 <- dplyr::select(county.pop.2011,-Drop,-Id)


county.pop.2009 = read.csv('Input/county.acs.2009.csv',skip=1)
colnames(county.pop.2009) <- c('Id','FIPS','Geog','Pop.2009','Drop');county.pop.2009 <- dplyr::select(county.pop.2009,-Drop,-Id)

county.pop.2000 = read.csv('Input/county.census.2000.csv',skip=1)
colnames(county.pop.2000) <- c('Id','FIPS','Geog','Pop.2000','Drop')
county.pop.2000 <- dplyr::select(county.pop.2000,-Drop,-Id)

county.pop.1990 = read.delim('Input/county.census.1990.2000.txt',sep=',',header=T)
names <- colnames(county.pop.1990)[-1]
county.pop.1990 <- county.pop.1990[,-ncol(county.pop.1990)]
colnames(county.pop.1990) <- names
county.pop.1996 = county.pop.1990 %>% dplyr::select(FIPS,X1996,Name) %>% dplyr::rename(Pop.1996 = X1996,Geog = Name)



county.pop.1990 = county.pop.1990 %>% dplyr::select(-X1990B)
long.pop.1990 = gather(county.pop.1990,Year,Population,-FIPS,-Name)
long.pop.1990$Year = gsub('X','',long.pop.1990$Year)
long.pop.1990$FIPS[nchar(long.pop.1990$FIPS)==4] = paste0('0',long.pop.1990$FIPS[nchar(long.pop.1990$FIPS)==4])



nca.chem$county.pop =   long.pop.1990$Population[match(paste(nca.chem$CFIPS,nca.chem$SAMPYEAR),paste(long.pop.1990$FIPS,long.pop.1990$Year))]

county.pop.2000$FIPS[nchar(county.pop.2000$FIPS)==4] = paste0('0',county.pop.2000$FIPS[nchar(county.pop.2000$FIPS)==4])
nca.chem$county.pop[is.na(nca.chem$county.pop)] =  county.pop.2000$Pop.2000[match(nca.chem$CFIPS[is.na(nca.chem$county.pop)],county.pop.2000$FIPS)]
nca.chem$county.pop = as.numeric(nca.chem$county.pop)


state.gdp <- read.csv('Input/state.gdp.pc.csv')
state.gdp = state.gdp[state.gdp$GeoName %in% state.ref$state.name,]
state.gdp <- state.gdp %>% dplyr::select(-IndustryId,-IndustryClassification,-Description,-GeoFIPS,-ComponentId,-ComponentName,-Region)

state.gdp.by.year <- state.gdp %>% gather(Year,GDPpc,-GeoName) %>% mutate(Year  = as.numeric(gsub('X','',Year))) %>% rename(State = GeoName)

state.gdp.by.year$State = as.character(state.gdp.by.year$State)

nca.chem$state.gdp.pc.1k =  state.gdp.by.year$GDPpc[match(paste(nca.chem$state.abb,nca.chem$SAMPYEAR),
                                                           paste(state.ref$state.abb[match(state.gdp.by.year$State,state.ref$state.name)],state.gdp.by.year$Year))]/1000

lc96 = read.csv('Input/LandCover1996.csv')
lc01 = read.csv('Input/LandCover2001.csv')
lc06 = read.csv('Input/LandCover2006.csv')
lc10 = read.csv('Input/LandCover2010.csv')
lc = join_all(list(lc96,lc01,lc06,lc10),type='full')

uq.classes = unique(lc$LCName)

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

cnp.history = fetchGoogle("https://docs.google.com/spreadsheets/d/1dbSJRtuSah56zBwjk-YySYwJ09ryURrofbJ6Nne5rv0/pub?output=csv")
cnp.history <- cnp.history %>% mutate(Conditional = mdy(Conditional),Conditional.Year = year(Conditional),Full = mdy(Full),Full.Year =year(Full),state.name = State)
cnp.history <- join(cnp.history,state.ref)


plan.attributes = fetchGoogle("https://docs.google.com/spreadsheets/d/18R3pvK_RcajdhxPEnQIEq9ZJFFr392uMHaHLWNA5yek/pub?output=csv")

plan.attributes = plan.attributes %>% mutate(Coord.Institution = mdy(Coord.Institution),
                                             Coord.Formal.Agreements = mdy(Coord.Formal.Agreements),
                                             Coord.Spec.Responsibilities = mdy(Coord.Spec.Responsibilities),
                                             Part.Input = mdy(Part.Input),
                                             Part.Advisory = mdy(Part.Advisory),
                                             Part.Outreach = mdy(Part.Outreach))

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

nca.chem$state.abb = as.character(nca.chem$state.abb)
# 
# nca.chem <- nca.chem %>% mutate(Full.Approval.Active = as.character(Full.Approval.Active) ,
#                     Cond.Approval.Active = as.character(Cond.Approval.Active) ,
#                     Coord.Formal.Agreements.Active = as.character(Coord.Formal.Agreements.Active),
#                     Coord.Spec.Responsibilities.Active = as.character(Coord.Spec.Responsibilities.Active),
#                      Coord.Institution.Active = as.character(Coord.Institution.Active),
#                     Part.Outreach.Active = as.character(Part.Outreach.Active),
#                     Part.Input.Active = as.character(Part.Input.Active),
#                      Part.Advisory.Active = as.character(Part.Advisory.Active))

                    
                     # as.factor(Coord.Formal.Agreements.Active) + 
  #as.factor(Coord.Spec.Responsibilities.Active) + 
  # as.factor(Coord.Institution.Active)+
counties@data$CFIPS[nchar(as.character(counties@data$CFIPS))==4] = paste0(0,counties@data$CFIPS[nchar(as.character(counties@data$CFIPS))==4])
wholecounties = unionSpatialPolygons(counties,ifelse(as.numeric(as.character(counties$INTPTLON)) < (-100), 1,2),threshold = 100)
west = wholecounties[1]
east = wholecounties[2]
wcounty.bdry <- inla.sp2segment(wholecounties)
uscounty = readOGR(dsn='Shapefiles',layer='countylum')
uscounty= uscounty[uscounty@data$FIPS %in% counties@data$CFIPS ==FALSE,]
spydf_counties <- gBuffer(uscounty, byid=TRUE, width=0)
drop.counties = unionSpatialPolygons(spydf_counties,IDs = rep(1,length(uscounty)),10000)


###Step 1) Prepare the mesh
mesh <- inla.mesh.2d(cbind(nca.chem$LON_DD, nca.chem$LAT_DD), max.edge=c(1000, 1000), cut=.2)
#mesh <- inla.mesh.2d(boundary=county.bdry, cutoff=1, max.edge=c(20,200))
plot(mesh,asp=1)
###Step 2) Define the SPDE
spde <- inla.spde2.matern(mesh)
###Step 3) Create observation matrix $A$ for each year
dim(A <- inla.spde.make.A(mesh,  loc=cbind(nca.chem$LON_DD, nca.chem$LAT_DD)))
###Step 4) Create index and stack data
ind <- inla.spde.make.index(name='s', n.spde=spde$n.spde)

# y is Greenland shark bycatch (counts)
# covariates are specified by data[,c(4:7)]
stke <- inla.stack(data=list(y=nca.chem$chem.index), 
                   tag='est', A=list(A, 1), 
                   effects=list(ind, 
                                data.frame(nca.chem)))

n.data = length(nca.chem$chem.index)
X = cbind(rep(1,n.data), nca.chem$county.pop,
          nca.chem$county.forst.prop,
          nca.chem$county.dev.prop,
          nca.chem$county.ag.prop,
          nca.chem$state.gdp.pc.1k,
          nca.chem$abs.year)

#covars$YEARS.ACTIVE)
n.cov = ncol(X)
Q = qr.Q(qr(X))


#model settings
correctionfactor = 10
pintercept = 0.001
DIC = TRUE
WAIC = TRUE
CPO = TRUE    

form.cond.wq <- y ~ -1 + county.forst.prop + county.ag.prop + county.dev.prop + county.pop + abs.year + 
  Cond.Approval.Active+
  f(SAMPYEAR,model='iid') +
  f(state.abb,model='iid') +
  f(s, model=spde, extraconstr = list(A = as.matrix(t(Q)%*%A), e= rep(0,n.cov)))

form.full.wq <- y ~ -1 + county.forst.prop + county.ag.prop + county.dev.prop + 
  county.pop + abs.year + 
  Full.Approval.Active+
f(SAMPYEAR,model='iid') +
  f(state.abb,model='iid') +
  f(s, model=spde, extraconstr = list(A = as.matrix(t(Q)%*%A), e= rep(0,n.cov)))

form.both.wq <- y ~ -1 + county.forst.prop + county.ag.prop + county.dev.prop + county.pop + abs.year + 
  Cond.Approval.Active +
  Full.Approval.Active + 
  f(SAMPYEAR,model='iid') +
  f(state.abb,model='iid') +
  f(s, model=spde, extraconstr = list(A = as.matrix(t(Q)%*%A), e= rep(0,n.cov)))

mod.cond.wq <- inla(form.cond.wq, family='gaussian', 
                    data=inla.stack.data(stke), verbose=TRUE,
                    control.compute=list(dic=DIC,waic=WAIC,cpo=CPO), 
                    #control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
                    control.predictor=list(A=inla.stack.A(stke)),
                    control.fixed= list(prec.intercept = pintercept,correlation.matrix = TRUE,
                                        expand.factor.strategy='model.matrix'),
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))

mod.full.wq <- inla(form.full.wq, family='gaussian', 
             data=inla.stack.data(stke), verbose=TRUE,
             control.compute=list(dic=DIC,waic=WAIC,cpo=CPO), 
             #control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
             control.predictor=list(A=inla.stack.A(stke)),
             control.fixed= list(prec.intercept = pintercept,correlation.matrix = TRUE,
                                 expand.factor.strategy='model.matrix'),
             control.inla = list(
               correct = TRUE,
               correct.factor = correctionfactor))

mod.both.wq <- inla(form.both.wq, family='gaussian', 
                    data=inla.stack.data(stke), verbose=TRUE,
                    control.compute=list(dic=DIC,waic=WAIC,cpo=CPO), 
                    #control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
                    control.predictor=list(A=inla.stack.A(stke)),
                    control.fixed= list(prec.intercept = pintercept,correlation.matrix = TRUE,
                                        expand.factor.strategy='model.matrix'),
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))



form.collab.part.wq <- y ~ -1 + county.forst.prop + county.ag.prop + county.dev.prop + county.pop + abs.year + 
  Full.Approval.Active + 
  Part.Outreach.Active+
  Part.Advisory.Active+
  Part.Input.Active +
  f(SAMPYEAR,model='iid') +
  f(state.abb,model='iid') +
  f(s, model=spde, extraconstr = list(A = as.matrix(t(Q)%*%A), e= rep(0,n.cov)))

mod.collab.part.wq <- inla(form.collab.part.wq,family='gaussian', 
                           data=inla.stack.data(stke), verbose=TRUE,
                           control.compute=list(dic=DIC,waic=WAIC,cpo=CPO), 
                           #control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
                           control.predictor=list(A=inla.stack.A(stke)),
                           control.fixed= list(prec.intercept = pintercept,correlation.matrix = TRUE,
                                               expand.factor.strategy='model.matrix'),
                           control.inla = list(
                             correct = TRUE,
                             correct.factor = correctionfactor))


form.collab.coord.wq <- y ~ -1 + county.forst.prop + county.ag.prop + county.dev.prop + county.pop + abs.year + 
  Full.Approval.Active+ 
  Coord.Formal.Agreements.Active+ 
  Coord.Spec.Responsibilities.Active + 
  Coord.Institution.Active+
  f(SAMPYEAR,model='iid') +
  f(state.abb,model='iid') +
  f(s, model=spde, extraconstr = list(A = as.matrix(t(Q)%*%A), e= rep(0,n.cov)))

mod.collab.coord.wq <- inla(form.collab.coord.wq,family='gaussian', 
                           data=inla.stack.data(stke), verbose=TRUE,
                           control.compute=list(dic=DIC,waic=WAIC,cpo=CPO), 
                           #control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
                           control.predictor=list(A=inla.stack.A(stke)),
                           control.fixed= list(prec.intercept = pintercept,correlation.matrix = TRUE,
                                               expand.factor.strategy='model.matrix'),
                           control.inla = list(
                             correct = TRUE,
                             correct.factor = correctionfactor))

tex.cond.wq <- texreg::createTexreg(
  coef.names = mod.cond.wq$names.fixed,
  coef = mod.cond.wq$summary.lincomb.derived$mean,
  ci.low = mod.cond.wq$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.cond.wq$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.cond.wq$dic$dic,mod.cond.wq$waic$waic))

tex.full.wq <- texreg::createTexreg(
  coef.names = mod.full.wq$names.fixed,
  coef = mod.full.wq$summary.lincomb.derived$mean,
  ci.low = mod.full.wq$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.full.wq$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.full.wq$dic$dic,mod.full.wq$waic$waic))

tex.both.wq <- texreg::createTexreg(
  coef.names = mod.both.wq$names.fixed,
  coef = mod.both.wq$summary.lincomb.derived$mean,
  ci.low = mod.both.wq$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.both.wq$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.both.wq$dic$dic,mod.both.wq$waic$waic))

if(onscreen)
{
  htmlreg(l=list(tex.cond.wq,tex.full.wq,tex.both.wq),leading.zero=TRUE,
          omit.coef = c('b0'),ci.test = 0,digits = 3,
          custom.model.names = c('Conditional','Full','Conditional:Full'),
          file = 'Output/Version1/wq.approval.table.html')}

if(!onscreen)
{
  htmlreg(l=list(tex.cond.wq,tex.full.wq,tex.both.wq),leading.zero=TRUE,
          omit.coef = c('b0'),ci.test = 0,digits = 3,
          custom.model.names = c('Conditional','Full','Conditional:Full'),
          file = '../Output/Version1/wq.approval.table.html')}


tex.collab.coord.wq <- texreg::createTexreg(
  coef.names = mod.collab.coord.wq$names.fixed,
  coef = mod.collab.coord.wq$summary.lincomb.derived$mean,
  ci.low = mod.collab.coord.wq$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.collab.coord.wq$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.collab.coord.wq$dic$dic,mod.collab.coord.wq$waic$waic))

tex.collab.part.wq <- texreg::createTexreg(
  coef.names = mod.collab.part.wq$names.fixed,
  coef = mod.collab.part.wq$summary.lincomb.derived$mean,
  ci.low = mod.collab.part.wq$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.collab.part.wq$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.collab.part.wq$dic$dic,mod.collab.part.wq$waic$waic))

if(!onscreen)
{
  htmlreg(l=list(tex.collab.coord.wq,tex.collab.part.wq),leading.zero=TRUE,
          omit.coef = c('b0'),ci.test = 0,digits = 3,
          custom.model.names = c('Coordination','Participation'),
          file = '../Output/Version1/wq.collab.table.html')}

if(onscreen)
{
  htmlreg(l=list(tex.collab.coord.wq,tex.collab.part.wq),leading.zero=TRUE,
          omit.coef = c('b0'),ci.test = 0,digits = 3,
          custom.model.names = c('Coordination','Participation'),
          file = 'Output/Version1/wq.collab.table.html')}



