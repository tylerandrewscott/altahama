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

focal.change.df$FIPS[nchar(as.character(focal.change.df$FIPS))==4] = paste0(0,focal.change.df$FIPS[nchar(as.character(focal.change.df$FIPS))==4]) 
total.file.names = paste0('Input/',grep('LandCover[[:digit:]]',list.files('Input/'),value=TRUE))
data.list = lapply(as.list(total.file.names),read.csv)
total.df = join_all(data.list,type='full')

area.df = total.df %>% group_by(Year,FIPS,StateAbbrev,CountyName) %>% summarise(Area = sum(SquareMiles)) %>% dplyr::select(-Year)

area.df <- area.df %>% data.frame(.) %>% dplyr::select(-Year) %>% mutate(uq = paste(FIPS,CountyName)) %>% filter(!duplicated(uq)) %>% dplyr::select(-uq)
area.df$FIPS[nchar(as.character(area.df$FIPS))==4] = paste0(0,area.df$FIPS[nchar(as.character(area.df$FIPS))==4])

focal.change.df <- join(focal.change.df,area.df)


focal.change.df$PercentChange <- 100*  (focal.change.df$TotalChange / focal.change.df$Area)

library(tidyr)


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



focal.change.df <- focal.change.df %>% filter(StateAbbrev!='DC') %>% mutate(FIPS = as.character(FIPS),ToPop10k = ToPop/10000,FromPop10k = FromPop/10000,state.gdp.pc.1k = state.gdp.pc/1000)

us.county = readOGR(dsn = '../duckabush/','tl_2013_us_county')
us.county = us.county[as.numeric(as.character(us.county$STATEFP))<=56,]
us.county = us.county[as.numeric(as.character(us.county$STATEFP))%in% c(2,3,14,15)==FALSE,]
us.county$CFIPS = as.character(paste0(us.county$STATEFP,us.county$COUNTYFP))
us.county$CFIPS[nchar(as.character(us.county$CFIPS))==4] = paste0(0,us.county$CFIPS[nchar(as.character(us.county$CFIPS))==4])
focal.change.df$CFIPS <- as.character(focal.change.df$FIPS)

us.county = us.county[us.county$CFIPS %in% focal.change.df$CFIPS,]



row.names(us.county) <- as.character(seq(1,nrow(us.county)))
us.county@data$rows <- row.names(us.county)
county.nb <- poly2nb(us.county,queen=FALSE)
nb2INLA("county.adj", county.nb)
graph = inla.read.graph(filename='county.adj')
county.ADJ = 'county.adj'

focal.change.df$rowID <-  as.numeric(as.character(row.names(us.county)[match(focal.change.df$CFIPS, us.county@data$CFIPS)]))
focal.change.df$PercentChange = focal.change.df$TotalChange/focal.change.df$Area



bea.mw = read.csv('Input/bea_mw_income.csv')
bea.atl = read.csv('Input/bea_atl_income.csv')
bea.tex = read.csv('Input/bea_tex_inc.csv')
bea.west = read.csv('Input/bea_west_income.csv')
bea.gulf = read.csv('Input/bea_gulf_income.csv')
bea.ne = read.csv('Input/bea_ne_income.csv')
bea.va = read.csv('Input/bea_va_inc.csv')
bea = join_all(list(bea.mw,bea.atl,bea.tex,bea.west,bea.ne,bea.va,bea.gulf),type='full')

beal = gather(bea,Year,Value,-GeoFips,-GeoName,-LineCode,-Description)
beal$Year = gsub('X','',beal$Year)
beal = beal[grep('capita',beal$Description),]
beal$GeoFips[nchar(beal$GeoFips)==4] = paste0(0,beal$GeoFips[nchar(beal$GeoFips)==4])
beal$uq = paste(beal$GeoFips,beal$Year)
focal.change.df$From.County.Per.Capita = beal$Value[match(paste(focal.change.df$CFIPS,focal.change.df$FromYear),beal$uq)]
focal.change.df$To.County.Per.Capita = beal$Value[match(paste(focal.change.df$CFIPS,focal.change.df$ToYear),beal$uq)]

sort(focal.change.df$CFIPS[is.na(focal.change.df$From.County.Per.Capita)])

unique(beal$GeoFips[as.numeric(beal$GeoFips)<=51830&as.numeric(beal$GeoFips>=51059)])

  beal$GeoFips[grep('Spots',beal$GeoName)]

focal.change.df$From.County.Per.Capita = NA
for (i in 1:nrow(focal.change.df))
{

  
  
}




#model settings
correctionfactor = 10
pintercept = 0.001
DIC = TRUE
WAIC = TRUE
CPO = TRUE
### Land Cover Chqnge Models

form.full.lc <- TotalChangePerYear ~ 1 + Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + FromPop10k + 
  as.factor(Full.Approval.Active) + f(ToYear,model = 'iid') + 
  f(rowID,model='bym',graph = county.ADJ)

mod.full.lc <- inla(form.full.lc,family='gaussian',data = focal.change.df,
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))

form.cond.lc <- TotalChangePerYear ~ 1 + Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + FromPop10k + 
  as.factor(Cond.Approval.Active) + f(ToYear,model = 'iid') + 
  f(rowID,model='bym',graph = county.ADJ)

mod.cond.lc <- inla(form.cond.lc,family='gaussian',data = focal.change.df,
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))

form.both.lc <- TotalChangePerYear ~ 1 + Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + FromPop10k + 
  as.factor(Cond.Approval.Active) + as.factor(Full.Approval.Active) + f(ToYear,model = 'iid') + 
  f(rowID,model='bym',graph = county.ADJ)

mod.both.lc <- inla(form.both.lc,family='gaussian',data = focal.change.df,
                    control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                    control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                    verbose=T,
                    control.inla = list(
                      correct = TRUE,
                      correct.factor = correctionfactor))

form.collab.coord.lc <- TotalChangePerYear ~ 1 + Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + FromPop10k + 
  as.factor(Full.Approval.Active) + 
  as.factor(Coord.Formal.Agreements.Active) + 
  as.factor(Coord.Spec.Responsibilities.Active) + 
  as.factor(Coord.Institution.Active)+
  #as.factor(Coord.Formal.Agreements.Active):as.factor(Coord.Spec.Responsibilities.Active) + 
  #as.factor(Coord.Institution.Active):as.factor(Coord.Spec.Responsibilities.Active)+
  #as.factor(Coord.Institution.Active):as.factor(Coord.Formal.Agreements.Active)+
    f(ToYear,model = 'iid') + 
  f(rowID,model='bym',graph = county.ADJ)



mod.collab.coord.lc <- inla(form.collab.coord.lc,family='gaussian',data = focal.change.df,
                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                      verbose=T,
                      control.inla = list(
                        correct = TRUE,
                        correct.factor = correctionfactor))

form.collab.part.lc <- TotalChangePerYear ~ 1 + Perc.Change.Pop + Perc.Change.State.GDP + state.gdp.pc.1k + Area100sqm + FromPop10k + 
  as.factor(Full.Approval.Active) + 
  as.factor(Part.Outreach.Active)+
  as.factor(Part.Advisory.Active)+
  as.factor(Part.Input.Active) +
  #  as.factor(Part.Outreach.Active):as.factor(Part.Advisory.Active) +
  #  as.factor(Part.Input.Active):as.factor(Part.Advisory.Active) +
  #  as.factor(Part.Outreach.Active):as.factor(Part.Input.Active) +
  f(ToYear,model = 'iid') + 
  f(rowID,model='bym',graph = county.ADJ)


mod.collab.part.lc <- inla(form.collab.part.lc,family='gaussian',data = focal.change.df,
                           control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                           control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                           verbose=T,
                           control.inla = list(
                             correct = TRUE,
                             correct.factor = correctionfactor))
library(texreg)

tex.cond.lc <- texreg::createTexreg(
  coef.names = mod.cond.lc$names.fixed,
  coef = mod.cond.lc$summary.lincomb.derived$mean,
  ci.low = mod.cond.lc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.cond.lc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.cond.lc$dic$dic,mod.cond.lc$waic$waic))

tex.full.lc <- texreg::createTexreg(
  coef.names = mod.full.lc$names.fixed,
  coef = mod.full.lc$summary.lincomb.derived$mean,
  ci.low = mod.full.lc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.full.lc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.full.lc$dic$dic,mod.full.lc$waic$waic))

tex.both.lc <- texreg::createTexreg(
  coef.names = mod.both.lc$names.fixed,
  coef = mod.both.lc$summary.lincomb.derived$mean,
  ci.low = mod.both.lc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.both.lc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.both.lc$dic$dic,mod.both.lc$waic$waic))

if(onscreen)
{
htmlreg(l=list(tex.cond.lc,tex.full.lc,tex.both.lc),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Conditional','Full','Conditional:Full'),
        file = 'Output/Version1/lc.approval.table.html')}

if(!onscreen)
{
  htmlreg(l=list(tex.cond.lc,tex.full.lc,tex.both.lc),leading.zero=TRUE,
          omit.coef = c('b0'),ci.test = 0,digits = 3,
          custom.model.names = c('Conditional','Full','Conditional:Full'),
          file = '../Output/Version1/lc.approval.table.html')}


tex.collab.coord.lc <- texreg::createTexreg(
  coef.names = mod.collab.coord.lc$names.fixed,
  coef = mod.collab.coord.lc$summary.lincomb.derived$mean,
  ci.low = mod.collab.coord.lc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.collab.coord.lc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.collab.coord.lc$dic$dic,mod.collab.coord.lc$waic$waic))

tex.collab.part.lc <- texreg::createTexreg(
  coef.names = mod.collab.part.lc$names.fixed,
  coef = mod.collab.part.lc$summary.lincomb.derived$mean,
  ci.low = mod.collab.part.lc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.collab.part.lc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.collab.part.lc$dic$dic,mod.collab.part.lc$waic$waic))

if(!onscreen)
{
htmlreg(l=list(tex.collab.coord.lc,tex.collab.part.lc),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Coordination','Participation'),
        file = '../Output/Version1/lc.collab.table.html')}

if(onscreen)
{
  htmlreg(l=list(tex.collab.coord.lc,tex.collab.part.lc),leading.zero=TRUE,
          omit.coef = c('b0'),ci.test = 0,digits = 3,
          custom.model.names = c('Coordination','Participation'),
          file = 'Output/Version1/lc.collab.table.html')}


summary(mod.both.lc)


