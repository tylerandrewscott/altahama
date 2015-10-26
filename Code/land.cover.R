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


change.file.names = paste0('Input/',grep('LandCoverChange',list.files('Input/'),value=TRUE))
data.list = lapply(as.list(change.file.names),read.csv)
change.df = join_all(data.list,type='full')
rm(data.list)

uq.classes = union(unique(change.df$toClass),unique(change.df$fromClass))

focal.change.df <- change.df %>% 
  filter(toClass %in% grep('Developed|Cultivated|Pasture',uq.classes,value=TRUE)) %>% 
  filter(fromClass %in% grep('Grass|Forest|Estuar|Wetland|Water|Palustrine',uq.classes,value=TRUE)) %>% 
  filter(fromClass != toClass) %>% 
  filter(ToYear-FromYear<=5) %>%
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  #mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) %>%
  mutate(uq = paste(FIPS,FromYear,ToYear,sep='.'))

good.focal.change.df <- change.df %>% 
  filter(fromClass %in% grep('Developed|Cultivated|Pasture',uq.classes,value=TRUE)) %>% 
  filter(toClass %in% grep('Grass|Forest|Estuar|Wetland|Water|Palustrine',uq.classes,value=TRUE)) %>% 
  filter(fromClass != toClass) %>% 
  filter(ToYear-FromYear<=5) %>%
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  # mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) %>%
  mutate(uq = paste(FIPS,FromYear,ToYear,sep='.'))


focal.change.df$TotalChange <- focal.change.df$TotalChange - good.focal.change.df$TotalChange[match(focal.change.df$uq,good.focal.change.df$uq)]

total.file.names = paste0('Input/',grep('LandCover[[:digit:]]',list.files('Input/'),value=TRUE))
data.list = lapply(as.list(total.file.names),read.csv)
total.df = join_all(data.list,type='full')

area.df = total.df %>% group_by(Year,FIPS,StateAbbrev,CountyName) %>% summarise(Area = sum(SquareMiles)) %>% dplyr::select(-Year)

area.df <- area.df %>% data.frame(.) %>% dplyr::select(-Year) %>% mutate(uq = paste(FIPS,CountyName)) %>% filter(!duplicated(uq)) %>% dplyr::select(-uq)

focal.change.df <- join(focal.change.df,area.df)


focal.change.df$PercentChange <- 100*  (focal.change.df$TotalChange / focal.change.df$Area)




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

county.pops = join_all(list(county.pop.1996,county.pop.2000,county.pop.2009,county.pop.2011),type='full',by='FIPS')
county.pops$FIPS[nchar(county.pops$FIPS)==4] = paste0(0,county.pops$FIPS[nchar(county.pops$FIPS)==4])


county.pops <- county.pops %>% rename(Pop.2006 = Pop.2009, Pop.2001 = Pop.2000) %>% dplyr::select(-Geog)

library(tidyr)
county.pops.long <- county.pops %>% gather(Year,Pop,Pop.1996:Pop.2011) %>% mutate(FromYear = as.numeric(gsub('Pop.','',Year)),ToYear = FromYear + 5) 
county.pops.long$ToPop <- (county.pops.long$Pop[match(paste(county.pops.long$FIPS,county.pops.long$ToYear),paste(county.pops.long$FIPS,county.pops.long$FromYear))])
county.pops.long$FromPop <- county.pops.long$Pop
county.pops.long$Perc.Change.Pop <- (county.pops.long$Pop[match(paste(county.pops.long$FIPS,county.pops.long$ToYear),paste(county.pops.long$FIPS,county.pops.long$FromYear))]- county.pops.long$Pop)/
  county.pops.long$Pop
county.pops.long$Perc.Change.Pop = county.pops.long$Perc.Change.Pop*100
county.pops.long$FromYear[county.pops.long$FromYear == 2011] <- 2010
county.pops.long$ToYear[county.pops.long$ToYear == 2011] <- 2010

county.pops.long <- county.pops.long %>% dplyr::select(-Pop,-Year)

focal.change.df <-join(focal.change.df,county.pops.long)

state.ref = data.frame(state.abb,state.name)
focal.change.df$State.Name = as.character(state.ref$state.name[match(focal.change.df$StateAbbrev,state.ref$state.abb)])

state.gdp <- read.csv('Input/state.gdp.pc.csv')
state.gdp = state.gdp[state.gdp$GeoName %in% state.ref$state.name,]
state.gdp <- state.gdp %>% dplyr::select(-IndustryId,-IndustryClassification,-Description,-GeoFIPS,-ComponentId,-ComponentName,-Region)

state.gdp.by.year <- state.gdp %>% gather(Year,GDPpc,-GeoName) %>% mutate(Year  = as.numeric(gsub('X','',Year))) %>% rename(State = GeoName)

state.gdp.by.year$State = as.character(state.gdp.by.year$State)

focal.change.df$state.gdp.pc = NA
for (i in 1:nrow(focal.change.df))
{
  focal.change.df$state.gdp.pc[i] =
  state.gdp.by.year %>% dplyr::filter(State == focal.change.df$State.Name[i],
                                      Year >= focal.change.df$FromYear[i],
                                      Year <= focal.change.df$ToYear[i]) %>%
  summarise(mean(GDPpc,na.rm=T)) %>% as.numeric(.)
}

focal.change.df = focal.change.df %>% filter(CountyName != 'District of Columbia')

#change in gdp per capita across period
#Note: 1997 is first year of new data from bls, so assigne 2007 data to 2006:
break.1997 <- state.gdp.by.year[state.gdp.by.year$Year==1997,]

break.1997$Year <- 1996

state.gdp.by.year <- join(state.gdp.by.year,break.1997,type='full')

focal.change.df$Perc.Change.State.GDP <-  100 * (state.gdp.by.year$GDPpc[match(paste(focal.change.df$State.Name,focal.change.df$ToYear),paste(state.gdp.by.year$State,state.gdp.by.year$Year))] - 
           state.gdp.by.year$GDPpc[match(paste(focal.change.df$State.Name,focal.change.df$FromYear),paste(state.gdp.by.year$State,state.gdp.by.year$Year))])/state.gdp.by.year$GDPpc[match(paste(focal.change.df$State.Name,focal.change.df$FromYear),paste(state.gdp.by.year$State,state.gdp.by.year$Year))]

library(RCurl)
library(mosaic)
library(lubridate)


cnp.history = fetchGoogle("https://docs.google.com/spreadsheets/d/1dbSJRtuSah56zBwjk-YySYwJ09ryURrofbJ6Nne5rv0/pub?output=csv")
cnp.history <- cnp.history %>% mutate(Conditional = mdy(Conditional),Conditional.Year = year(Conditional),Full = mdy(Full),Full.Year =year(Full),state.name = State)
cnp.history <- join(cnp.history,state.ref)

focal.change.df$Conditional.Year <- cnp.history$Conditional.Year[match(focal.change.df$StateAbbrev,cnp.history$state.abb)]
focal.change.df$Full.Year <- cnp.history$Full.Year[match(focal.change.df$StateAbbrev,cnp.history$state.abb)]

focal.change.df$Full.Approval.Active <- ifelse(focal.change.df$Full.Year<=focal.change.df$FromYear,1,0)
focal.change.df$Cond.Approval.Active <- ifelse(focal.change.df$Conditional.Year<=focal.change.df$FromYear,1,0)
focal.change.df$Full.Approval.Active[is.na(focal.change.df$Full.Approval.Active)] <- 0 
focal.change.df$Cond.Approval.Active[is.na(focal.change.df$Cond.Approval.Active)] <- 0 


plan.attributes = fetchGoogle("https://docs.google.com/spreadsheets/d/18R3pvK_RcajdhxPEnQIEq9ZJFFr392uMHaHLWNA5yek/pub?output=csv")

plan.attributes = plan.attributes %>% mutate(Coord.Institution = mdy(Coord.Institution),
Coord.Formal.Agreements = mdy(Coord.Formal.Agreements),
Coord.Spec.Responsibilities = mdy(Coord.Spec.Responsibilities),
Part.Input = mdy(Part.Input),
Part.Advisory = mdy(Part.Advisory),
Part.Outreach = mdy(Part.Outreach))

focal.change.df = join(focal.change.df,plan.attributes)

focal.change.df$Coord.Institution.Active <- ifelse(year(focal.change.df$Coord.Institution)<=focal.change.df$FromYear,1,0)
focal.change.df$Coord.Institution.Active[is.na(focal.change.df$Coord.Institution)] <- 0 

focal.change.df$Coord.Spec.Responsibilities.Active <- ifelse(year(focal.change.df$Coord.Spec.Responsibilities)<=focal.change.df$FromYear,1,0)
focal.change.df$Coord.Spec.Responsibilities.Active[is.na(focal.change.df$Coord.Spec.Responsibilities)] <- 0 

focal.change.df$Coord.Formal.Agreements.Active <- ifelse(year(focal.change.df$Coord.Formal.Agreements)<=focal.change.df$FromYear,1,0)
focal.change.df$Coord.Formal.Agreements.Active[is.na(focal.change.df$Coord.Formal.Agreements)] <- 0 


focal.change.df$Part.Input.Active <- ifelse(year(focal.change.df$Part.Input)<=focal.change.df$FromYear,1,0)
focal.change.df$Part.Input.Active[is.na(focal.change.df$Part.Input)] <- 0 

focal.change.df$Part.Advisory.Active <- ifelse(year(focal.change.df$Part.Advisory)<=focal.change.df$FromYear,1,0)
focal.change.df$Part.Advisory.Active[is.na(focal.change.df$Part.Advisory)] <- 0 

focal.change.df$Part.Outreach.Active <- ifelse(year(focal.change.df$Part.Outreach)<=focal.change.df$FromYear,1,0)
focal.change.df$Part.Outreach.Active[is.na(focal.change.df$Part.Outreach)] <- 0 

focal.change.df$Area100sqm <- focal.change.df$Area/100

focal.change.df$TotalChangePerYear = focal.change.df$TotalChange/(focal.change.df$ToYear-focal.change.df$FromYear)

focal.change.df <- focal.change.df %>% filter(StateAbbrev!='DC') %>% mutate(FIPS = as.character(FIPS),ToPop10k = ToPop/10000,state.gdp.pc.1k = state.gdp.pc/1000)

us.county = readOGR(dsn = '../duckabush/','tl_2013_us_county')
us.county = us.county[as.numeric(as.character(us.county$STATEFP))<=56,]
us.county = us.county[as.numeric(as.character(us.county$STATEFP))%in% c(2,3,14,15)==FALSE,]
us.county$CFIPS = as.numeric(as.character(paste0(us.county$STATEFP,us.county$COUNTYFP)))
focal.change.df$CFIPS <- as.numeric(focal.change.df$FIPS)
us.county = us.county[us.county$CFIPS %in% focal.change.df$CFIPS,]

row.names(us.county) <- as.character(seq(1,nrow(us.county)))
us.county@data$rows <- row.names(us.county)
county.nb <- poly2nb(us.county,queen=FALSE)
nb2INLA("county.adj", county.nb)
graph = inla.read.graph(filename='county.adj')
county.ADJ = 'county.adj'

focal.change.df$rowID <-  as.numeric(as.character(row.names(us.county)[match(focal.change.df$CFIPS, us.county@data$CFIPS)]))


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

### Land Cover Chqnge Models

form.full.lc <- TotalChange ~ 1 + Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + ToPop10k + 
  as.factor(Full.Approval.Active) + f(ToYear,model = 'iid') + 
  f(rowID,model='bym',graph = county.ADJ)

mod.full.lc <- inla(form.full.lc,family='gaussian',,data = focal.change.df,
                 control.compute=list(dic=TRUE,waic=TRUE))

form.cond.lc <- TotalChange ~ 1 + Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + ToPop10k + 
  as.factor(Cond.Approval.Active) + f(ToYear,model = 'iid') + 
  f(rowID,model='bym',graph = county.ADJ)

mod.cond.lc <- inla(form.cond.lc,family='gaussian',data = focal.change.df,
                 control.compute=list(dic=TRUE,waic=TRUE))


form.collab.lc <- TotalChange ~ 1 + Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + ToPop10k + 
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
   + f(ToYear,model = 'iid') + 
  f(rowID,model='bym',graph = county.ADJ)

mod.collab.lc <- inla(form.collab.lc,family='gaussian',data = focal.change.df,
                   control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                   control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                   verbose=T,
                   control.inla = list(
                     correct = TRUE,
                     correct.factor = correctionfactor))


library(texreg)




#focal.change.df$CFIPS[nchar(focal.change.df$CFIPS)==4] <- paste0(0,focal.change.df$CFIPS[nchar(focal.change.df$CFIPS)==4])
#row.names(us.county) <- seq(1,length())






table(nchar(focal.change.df$CFIPS))


focal.change.df$CFIPS[is.na(match(focal.change.df$CFIPS, us.county@data$CFIPS))]

sort(us.county@data$CFIPS)




test = nb2mat(county.nb, style="B")

adjmat<-as(nb2mat(county.nb, style="B"), "dgTMatrix") #Binary adjacency matrix


form.test <- TotalChange ~ 1 + #Perc.Change.Pop + Perc.Change.State.GDP +
  f(CFIPS,model='bym',graph = county.ADJ)






county.ADJ = 'county.adj'



#Load libraries


#Load data from a shapefile included in the spdep package
nc.sids <- readShapePoly(system.file("etc/shapes/sids.shp", package="spdep")[1])

#Create adjacency matrix
nc.nb <- poly2nb(nc.sids)

?poly2nb
#Compute expted number of cases
nc.sids$EXP<-nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74)

#Compute proportion of non-white births
nc.sids$NWPROP<-nc.sids$NWBIR74/nc.sids$BIR74

#Convert the adjacency matrix into a file in the INLA format
nb2INLA("nc.adj", nc.nb)

#Create areas IDs to match the values in nc.adj
nc.sids$ID<-1:100
m1<-inla(SID74~NWPROP+f(nc.sids$ID, model="besag", graph="nc.adj"),
         family="poisson", E=nc.sids$EXP, data=as.data.frame(nc.sids),
         control.predictor=list(compute=TRUE))

#Alternatively, a sparse matrix can be used
adjmat<-as(nb2mat(nc.nb, style="B"), "dgTMatrix") #Binary adjacency matrix
m2<-inla(SID74~NWPROP+f(nc.sids$ID, model="besag", graph=adjmat),
         family="poisson", E=nc.sids$EXP, data=as.data.frame(nc.sids),
         control.predictor=list(compute=TRUE))


#Get realtive risk estimates
nc.sids$RR1<-m1$summary.fitted.values[,1]
nc.sids$RR2<-m2$summary.fitted.values[,1]

#Display relative risk estimates to show that both examples fit the same model
spplot(nc.sids, c("RR1", "RR2"))








mod <- lm(TotalChange ~  Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + ToPop10k + as.character(FIPS) + as.factor(ToYear) + State,data = focal.change.df)


mod1 <- lmer(TotalChange ~  Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + ToPop10k + (1|FIPS) + (1|ToYear) ,data = focal.change.df)
mod2 <- lmer(TotalChange ~  Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + ToPop10k + 
               as.factor(Cond.Approval.Active) + 
               (1|FIPS) + (1|ToYear) ,data = focal.change.df)
mod3 <- lmer(TotalChange ~  Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + ToPop10k + 
               as.factor(Full.Approval.Active) + 
               (1|FIPS) + (1|ToYear) ,data = focal.change.df)
as.factor(Full.Approval.Active) + as.factor(Cond.Approval.Active):as.factor(Full.Approval.Active) + 
BIC(mod1,mod2,mod3)




summary(mod2)
summary(mod3)



table(focal.change.df$Full.Approval.Active,focal.change.df$Cond.Approval.Active)

summary(mod2)
names(focal.change.df)
focal.change.df$State
BIC(mod)


table(focal.change.df$Full.Approval.Active)
str(focal.change.df)


round(focal.change.df$ToPop10k,2)



focal.change.df$Area


sum(is.na(focal.change.df$TotalChange))
sum(is.na(focal.change.df$FIPS))
sum(is.na(focal.change.df$Perc.Change.State.GDP))

focal.change.df[is.na(focal.change.df$Perc.Change.State.GDP),]

focal.change.df[focal.change.df$FIPS==9015,]

?summarise

library(lme4)

mod <- lmer(TotalChange ~ Perc.Change.Pop + Area100sqm + ToYear + Cond.Approval.Active + Full.Approval.Active + 
              Cond.Approval.Active:Full.Approval.Active - 1 + 
              (1|FIPS) + (1|StateAbbrev),data = focal.change.df)


table(focal.change.df$FIPS)
dim(focal.change.df)







hist(focal.change.df$TotalChange.ihs)
hist(focal.change.df$TotalChange)

summary(focal.change.df$TotalChange)
summary(focal.change.df$TotalChange.ihs)

ggplot(focal.change.df,aes(x=as.factor(ToYear),y=TotalChange.ihs)) + geom_boxplot() + theme_bw()

test = focal.change.df$TotalChange

summary(test)

hist(log(test + sqrt((test^test)+1)))


(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) 

hist(log(test + 0.01))
lmer(TotalChange.ihs)







