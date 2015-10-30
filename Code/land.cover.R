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
  filter(toClass %in% grep('Developed',uq.classes,value=TRUE)) %>% 
  filter(fromClass %in% grep('Grass|Forest|Estuar|Wetland|Water|Palustrine|Shrub',uq.classes,value=TRUE)) %>% 
  filter(fromClass != toClass, ToYear-FromYear<=5) %>% 
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  #mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) %>%
  mutate(uq = paste(FIPS,FromYear,ToYear,sep='.'))

good.focal.change.df <- change.df %>% 
  filter(fromClass %in% grep('Developed|Bare',uq.classes,value=TRUE)) %>% 
  filter(toClass %in% grep('Developed',uq.classes,value=TRUE)) %>% 
  filter(fromClass != toClass) %>% 
  filter(ToYear-FromYear<=5) %>%
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  # mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) %>%
  mutate(uq = paste(FIPS,FromYear,ToYear,sep='.'))

undev.focal.change.df <- change.df %>% 
  filter(fromClass %in% grep('Developed|Bare',uq.classes,value=TRUE)) %>% 
  filter(toClass %in% grep('Forest|Wetl|Grass|Water|Estuar|Shrub|Palustrine',uq.classes,value=TRUE)) %>% 
  filter(fromClass != toClass) %>% 
  filter(ToYear-FromYear<=5) %>%
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  # mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) %>%
  mutate(uq = paste(FIPS,FromYear,ToYear,sep='.'))


focal.change.df$TotalChange.Wild.To.Dev <- focal.change.df$TotalChange  

focal.change.df$TotalChange.Dev.To.Dev <-  good.focal.change.df$TotalChange[match(focal.change.df$uq,good.focal.change.df$uq)]
focal.change.df$Prop.Dev.Wild.To.Dev = focal.change.df$TotalChange.Wild.To.Dev/(focal.change.df$TotalChange.Wild.To.Dev + focal.change.df$TotalChange.Dev.To.Dev) 
focal.change.df$Prop.Dev.Wild.To.Dev[is.na(focal.change.df$Prop.Dev.Wild.To.Dev)] = 0 
focal.change.df$TotalChange.Dev.To.Other <-  undev.focal.change.df$TotalChange[match(focal.change.df$uq,undev.focal.change.df$uq)]

focal.change.df$Net.Developed = focal.change.df$TotalChange.Wild.To.Dev - focal.change.df$TotalChange.Dev.To.Other


deforest.focal.change.df <- change.df %>% 
  filter(fromClass %in% c('Mixed Forest','Deciduous Forest','Evergreen Forest')) %>% 
  filter(toClass %in% grep('Developed|Cultivated|Pastur|Bare',uq.classes,value=TRUE)) %>% 
  filter(fromClass != toClass) %>% 
  filter(ToYear-FromYear<=5) %>%
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  # mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) %>%
  mutate(uq = paste(FIPS,FromYear,ToYear,sep='.'))

reforest.focal.change.df <- change.df %>% 
  filter(toClass %in% c('Mixed Forest','Deciduous Forest','Evergreen Forest')) %>% 
  filter(fromClass %in% grep('Developed|Cultivated|Pastur|Bare',uq.classes,value=TRUE)) %>% 
  filter(fromClass != toClass) %>% 
  filter(ToYear-FromYear<=5) %>%
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  # mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) %>%
  mutate(uq = paste(FIPS,FromYear,ToYear,sep='.'))

focal.change.df$TotalChange.Forst.To.Other <-  deforest.focal.change.df$TotalChange[match(focal.change.df$uq,deforest.focal.change.df$uq)]
focal.change.df$TotalChange.Other.To.Forst<-  reforest.focal.change.df$TotalChange[match(focal.change.df$uq,reforest.focal.change.df$uq)]
focal.change.df$Net.Deforested = focal.change.df$TotalChange.Forst.To.Other - focal.change.df$TotalChange.Other.To.Forst

unwetlands.focal.change.df <- change.df %>% 
  filter(fromClass %in% grep('Wetland',uq.classes,value=TRUE)) %>% 
  filter(toClass %in% grep('Developed|Cultivated|Pastur|Bare',uq.classes,value=TRUE)) %>% 
  filter(fromClass != toClass) %>% 
  filter(ToYear-FromYear<=5) %>%
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  # mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) %>%
  mutate(uq = paste(FIPS,FromYear,ToYear,sep='.'))

rewetlands.focal.change.df <- change.df %>% 
  filter(toClass %in% grep('Wetland',uq.classes,value=TRUE)) %>% 
  filter(fromClass %in% grep('Developed|Cultivated|Pastur|Bare',uq.classes,value=TRUE)) %>% 
  filter(fromClass != toClass) %>% 
  filter(ToYear-FromYear<=5) %>%
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  # mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) %>%
  mutate(uq = paste(FIPS,FromYear,ToYear,sep='.'))

focal.change.df$TotalChange.Wetl.To.Other <-  unwetlands.focal.change.df$TotalChange[match(focal.change.df$uq,unwetlands.focal.change.df$uq)]
focal.change.df$TotalChange.Other.To.Wetl<-  rewetlands.focal.change.df$TotalChange[match(focal.change.df$uq,rewetlands.focal.change.df$uq)]
focal.change.df$Net.Wetlands = focal.change.df$TotalChange.Wetl.To.Other - focal.change.df$TotalChange.Other.To.Wetl


decultivate.focal.change.df <- change.df %>% 
  filter(fromClass %in% grep('Cultivat|Pastu',uq.classes,value=TRUE)) %>% 
  filter(toClass %in% grep('Forest|Wetl|Grass',uq.classes,value=TRUE)) %>%
  filter(fromClass != toClass) %>% 
  filter(ToYear-FromYear<=5) %>%
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  # mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) %>%
  mutate(uq = paste(FIPS,FromYear,ToYear,sep='.'))

recultivate.focal.change.df <- change.df %>% 
  filter(toClass %in% grep('Cultivat|Pastu',uq.classes,value=TRUE)) %>% 
  filter(fromClass != toClass) %>% 
  filter(ToYear-FromYear<=5) %>%
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  # mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) %>%
  mutate(uq = paste(FIPS,FromYear,ToYear,sep='.'))

focal.change.df$TotalChange.Cult.To.Other <-  decultivate.focal.change.df$TotalChange[match(focal.change.df$uq,decultivate.focal.change.df$uq)]
focal.change.df$TotalChange.Other.To.Cult <-  recultivate.focal.change.df$TotalChange[match(focal.change.df$uq,recultivate.focal.change.df$uq)]
focal.change.df$Net.Cultivated = focal.change.df$TotalChange.Other.To.Cult - focal.change.df$TotalChange.Cult.To.Other

focal.change.df$FIPS[nchar(as.character(focal.change.df$FIPS))==4] = paste0(0,focal.change.df$FIPS[nchar(as.character(focal.change.df$FIPS))==4])
focal.change.df$CFIPS = focal.change.df$FIPS

  lc96 = read.csv('Input/LandCover1996.csv')
  lc01 = read.csv('Input/LandCover2001.csv')
  lc06 = read.csv('Input/LandCover2006.csv')
  lc10 = read.csv('Input/LandCover2010.csv')
  lc = join_all(list(lc96,lc01,lc06,lc10),type='full')
  
  uq.classes = unique(lc$LCName)
  
  lc$CFIPS = lc$FIPS
  lc$CFIPS[nchar(lc$CFIPS)==4] = paste0(0,lc$CFIPS[nchar(lc$CFIPS)==4])
  lc = lc %>% dplyr::select(-CCAP_ClassID)
  
lc.sum = lc %>% spread(LCName,SquareMiles)  

lc.sum$Forested = lc.sum$`Mixed Forest`+lc.sum$`Deciduous Forest`+lc.sum$`Evergreen Forest`
lc.sum$Wetlands = rowSums(lc.sum[,grep('Wetlands',names(lc.sum))])
lc.sum$Ag = lc.sum$Cultivated+lc.sum$`Pasture-Hay`
lc.sum$Dev = rowSums(lc.sum[,grep('Developed',names(lc.sum))])

lc.sum = lc.sum %>% dplyr::select(CFIPS,Forested,Wetlands,Ag,Dev)
focal.change.df  = join(focal.change.df,lc.sum)

 
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

extra96 = state.gdp.by.year[state.gdp.by.year$Year==1997,]
extra96$Year = 1996
state.gdp.by.year = join(state.gdp.by.year,extra96,type='full')

state.gdp.by.year$State = as.character(state.gdp.by.year$State)


focal.change.df$To.Year.state.gdp.pc = state.gdp.by.year$GDPpc[match(paste(focal.change.df$State.Name,focal.change.df$ToYear),
paste(state.gdp.by.year$State,state.gdp.by.year$Year))]

focal.change.df$From.Year.state.gdp.pc = state.gdp.by.year$GDPpc[match(paste(focal.change.df$State.Name,focal.change.df$FromYear),
                                                                     paste(state.gdp.by.year$State,state.gdp.by.year$Year))]


focal.change.df = focal.change.df %>% filter(CountyName != 'District of Columbia')

focal.change.df$Perc.Change.State.GDP = 100 * (focal.change.df$To.Year.state.gdp.pc-focal.change.df$From.Year.state.gdp.pc)/focal.change.df$From.Year.state.gdp.pc 

library(RCurl)
library(mosaic)
library(lubridate)


locgov = read.csv('Input/COG_2012_ORG014_with_ann.csv')
locgov$GEO.id2[nchar(as.character(locgov$GEO.id2))==4] = paste0(0,locgov$GEO.id2[nchar(as.character(locgov$GEO.id2))==4])

focal.change.df$total.special.districts = locgov$total_special_purpose[match(focal.change.df$FIPS,locgov$GEO.id2)]
focal.change.df$total.munic =  locgov$municipal[match(focal.change.df$FIPS,locgov$GEO.id2)]
focal.change.df$sub.county =  locgov$total_subcounty[match(focal.change.df$FIPS,locgov$GEO.id2)]

focal.change.df$sub.county[is.na(focal.change.df$sub.county)] = 1
focal.change.df$total.munic[is.na(focal.change.df$sub.county)] = 1
focal.change.df$total.munic[is.na(focal.change.df$sub.county)] = 1


library(gdata)

#yvec = c('96','01','06','10')
#ec.list = lapply(paste0('Input/allhlcn',yvec,'.xlsx'),read.xls,sheet=1)

al96 = read.xls('Input/allhlcn96.xlsx',sheet=1)
al01 = read.xls('Input/allhlcn01.xlsx',sheet=1)
al06 = read.xls('Input/allhlcn06.xlsx',sheet=1)
al10 = read.xls('Input/allhlcn10.xlsx',sheet=1)

ec.all = join_all(list(al96,al01,al06,al10),type='full')

ec.nat = filter(ec.all,Industry =='Natural resources and mining')
ec.const = filter(ec.all,Industry=='Construction')
ec.leis = filter(ec.all,Industry=='Leisure and hospitality')
ec.man = filter(ec.all,Industry=='Manufacturing')


focal.change.df$FromYear.county.nat.emp = ec.nat$Annual.Average.Employment[match(paste(focal.change.df$FIPS,focal.change.df$FromYear),
                                                paste(ec.nat$Area.Code,ec.nat$Year))]
focal.change.df$FromYear.county.const.emp = ec.const$Annual.Average.Employment[match(paste(focal.change.df$FIPS,focal.change.df$FromYear),
                                                paste(ec.const$Area.Code,ec.const$Year))]
focal.change.df$FromYear.county.leis.emp = ec.leis$Annual.Average.Employment[match(paste(focal.change.df$FIPS,focal.change.df$FromYear),
                                                paste(ec.leis$Area.Code,ec.leis$Year))]
focal.change.df$FromYear.county.man.emp = ec.man$Annual.Average.Employment[match(paste(focal.change.df$FIPS,focal.change.df$FromYear),
                                                 paste(ec.man$Area.Code,ec.man$Year))]

focal.change.df$ToYear.county.nat.emp = ec.nat$Annual.Average.Employment[match(paste(focal.change.df$FIPS,focal.change.df$ToYear),
                                                                                 paste(ec.nat$Area.Code,ec.nat$Year))]
focal.change.df$ToYear.county.const.emp = ec.const$Annual.Average.Employment[match(paste(focal.change.df$FIPS,focal.change.df$ToYear),
                                                                                     paste(ec.const$Area.Code,ec.const$Year))]
focal.change.df$ToYear.county.leis.emp = ec.leis$Annual.Average.Employment[match(paste(focal.change.df$FIPS,focal.change.df$ToYear),
                                                                                   paste(ec.leis$Area.Code,ec.leis$Year))]
focal.change.df$ToYear.county.man.emp = ec.man$Annual.Average.Employment[match(paste(focal.change.df$FIPS,focal.change.df$ToYear),
                                                                                 paste(ec.man$Area.Code,ec.man$Year))]


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



focal.change.df <- focal.change.df %>% filter(StateAbbrev!='DC') %>% mutate(FIPS = as.character(FIPS),
                                                                            ToPop10k = ToPop/10000,FromPop10k = FromPop/10000,state.gdp.pc.1k = From.Year.state.gdp.pc/1000)

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
bea.va = read.csv('Input/bea_va_income.csv')
bea = join_all(list(bea.mw,bea.atl,bea.tex,bea.west,bea.ne,bea.va,bea.gulf),type='full')

beal = gather(bea,Year,Value,-GeoFips,-GeoName,-LineCode,-Description)
beal$Year = gsub('X','',beal$Year)
beal = beal[grep('capita',beal$Description),]
beal$GeoFips[nchar(beal$GeoFips)==4] = paste0(0,beal$GeoFips[nchar(beal$GeoFips)==4])
beal$uq = paste(beal$GeoFips,beal$Year)
focal.change.df$From.County.Per.Capita = beal$Value[match(paste(focal.change.df$CFIPS,focal.change.df$FromYear),beal$uq)]
focal.change.df$To.County.Per.Capita = beal$Value[match(paste(focal.change.df$CFIPS,focal.change.df$ToYear),beal$uq)]

focal.change.df$To.County.Per.Capita[is.na(focal.change.df$To.County.Per.Capita)] = focal.change.df$To.Year.state.gdp.pc[is.na(focal.change.df$To.County.Per.Capita)]
focal.change.df$From.County.Per.Capita[is.na(focal.change.df$From.County.Per.Capita)] = focal.change.df$From.Year.state.gdp.pc[is.na(focal.change.df$From.County.Per.Capita)]

focal.change.df$To.County.Per.Capita =   as.numeric(as.character(focal.change.df$To.County.Per.Capita))
focal.change.df$From.County.Per.Capita =   as.numeric(as.character(focal.change.df$From.County.Per.Capita))

focal.change.df$Change.County.Per.Capita = (focal.change.df$To.County.Per.Capita - focal.change.df$From.County.Per.Capita)/
  focal.change.df$From.County.Per.Capita

focal.change.df$ToYear.county.nat.emp = as.numeric(gsub(',','',as.character(focal.change.df$FromYear.county.nat.emp)))
focal.change.df$FromYear.county.nat.emp = as.numeric(gsub(',','',as.character(focal.change.df$FromYear.county.nat.emp)))


focal.change.df$ToYear.county.const.emp = as.numeric(gsub(',','',as.character(focal.change.df$FromYear.county.const.emp)))
focal.change.df$FromYear.county.const.emp = as.numeric(gsub(',','',as.character(focal.change.df$FromYear.county.const.emp)))

focal.change.df$Change.County.Per.Capita = focal.change.df$Change.County.Per.Capita*100
focal.change.df$FromPop10k = focal.change.df$FromPop/10000

focal.change.df$FromYear.county.const.emp = focal.change.df$FromYear.county.const.emp/1000
focal.change.df$FromYear.county.nat.emp = focal.change.df$FromYear.county.nat.emp/1000
focal.change.df$From.County.Per.Capita = focal.change.df$From.County.Per.Capita/10000


focal.change.df$Forested = focal.change.df$Forested/100
focal.change.df$Dev = focal.change.df$Dev/100
focal.change.df$Ag = focal.change.df$Ag/100



#model settings
correctionfactor = 10
pintercept = 0.001
DIC = TRUE
WAIC = TRUE
CPO = TRUE
### Land Cover Chqnge Models


form.approval.Developed = Net.Developed ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev + as.factor(Cond.Approval.Active) + as.factor(Full.Approval.Active) +
                        f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

form.approval.Cultivated = Net.Cultivated ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
  total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev + as.factor(Cond.Approval.Active) + as.factor(Full.Approval.Active) +
  f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)


form.approval.Deforested = Net.Deforested ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
  total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev + as.factor(Cond.Approval.Active) + as.factor(Full.Approval.Active) +
  f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

form.approval.Wetlands = Net.Wetlands ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
  total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev + as.factor(Cond.Approval.Active) + as.factor(Full.Approval.Active) +
  f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)



form.collab.coord.Wetlands = Net.Wetlands ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
  total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev +  as.factor(Full.Approval.Active) +
  as.factor(Coord.Formal.Agreements.Active) + 
  as.factor(Coord.Spec.Responsibilities.Active) + 
  as.factor(Coord.Institution.Active)+
  f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)


form.collab.coord.Deforested = Net.Deforested ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
  total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev +  as.factor(Full.Approval.Active) +
  as.factor(Coord.Formal.Agreements.Active) + 
  as.factor(Coord.Spec.Responsibilities.Active) + 
  as.factor(Coord.Institution.Active)+
  f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

form.collab.coord.Cultivated = Net.Cultivated ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
  total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev +  as.factor(Full.Approval.Active) +
  as.factor(Coord.Formal.Agreements.Active) + 
  as.factor(Coord.Spec.Responsibilities.Active) + 
  as.factor(Coord.Institution.Active)+
  f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)


form.collab.coord.Developed = Net.Developed ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
  total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev +  as.factor(Full.Approval.Active) +
  as.factor(Coord.Formal.Agreements.Active) + 
  as.factor(Coord.Spec.Responsibilities.Active) + 
  as.factor(Coord.Institution.Active)+
  f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)


form.collab.part.Wetlands = Net.Wetlands ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
  total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev +    as.factor(Full.Approval.Active) + 
  as.factor(Part.Outreach.Active)+
  as.factor(Part.Advisory.Active)+
  as.factor(Part.Input.Active) +
  f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)


form.collab.part.Deforested = Net.Deforested ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
  total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev +    as.factor(Full.Approval.Active) + 
  as.factor(Part.Outreach.Active)+
  as.factor(Part.Advisory.Active)+
  as.factor(Part.Input.Active) +
  f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

form.collab.part.Cultivated = Net.Cultivated ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
  total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev +   as.factor(Full.Approval.Active) + 
  as.factor(Part.Outreach.Active)+
  as.factor(Part.Advisory.Active)+
  as.factor(Part.Input.Active) +
  f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)


form.collab.part.Developed = Net.Developed ~ 1 + FromYear.county.const.emp+ FromYear.county.nat.emp+total.munic +
  total.special.districts +From.County.Per.Capita + Change.County.Per.Capita +Perc.Change.Pop + 
  Area100sqm + FromPop10k + Forested + Ag + Wetlands + Dev + as.factor(Full.Approval.Active) + 
  as.factor(Part.Outreach.Active)+
  as.factor(Part.Advisory.Active)+
  as.factor(Part.Input.Active) +
  f(ToYear,model = 'iid') + f(rowID,model='bym',graph = county.ADJ)




mod.lc.approval.Deforested<- inla(form.approval.Deforested,family='gaussian',data = focal.change.df,
                        control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                        control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                        verbose=T,
                        control.inla = list(
                          correct = TRUE,
                          correct.factor = correctionfactor))

mod.lc.approval.Wetlands<- inla(form.approval.Wetlands,family='gaussian',data = focal.change.df,
                                  control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                  control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                  verbose=T,
                                  control.inla = list(
                                    correct = TRUE,
                                    correct.factor = correctionfactor))

mod.lc.approval.Developed<- inla(form.approval.Developed,family='gaussian',data = focal.change.df,
                                  control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                  control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                  verbose=T,
                                  control.inla = list(
                                    correct = TRUE,
                                    correct.factor = correctionfactor))

mod.lc.approval.Cultivated<- inla(form.approval.Cultivated,family='gaussian',data = focal.change.df,
                                  control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                  control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                  verbose=T,
                                  control.inla = list(
                                    correct = TRUE,
                                    correct.factor = correctionfactor))


tex.lc.approval.Developed <- texreg::createTexreg(
  coef.names = mod.lc.approval.Developed$names.fixed,
  coef = mod.lc.approval.Developed$summary.lincomb.derived$mean,
  ci.low = mod.lc.approval.Developed$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.approval.Developed$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.approval.Developed$dic$dic,mod.lc.approval.Developed$waic$waic))



tex.lc.approval.Deforested <- texreg::createTexreg(
  coef.names = mod.lc.approval.Deforested$names.fixed,
  coef = mod.lc.approval.Deforested$summary.lincomb.derived$mean,
  ci.low = mod.lc.approval.Deforested$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.approval.Deforested$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.approval.Deforested$dic$dic,mod.lc.approval.Deforested$waic$waic))


tex.lc.approval.Wetlands <- texreg::createTexreg(
  coef.names = mod.lc.approval.Wetlands$names.fixed,
  coef = mod.lc.approval.Wetlands$summary.lincomb.derived$mean,
  ci.low = mod.lc.approval.Wetlands$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.approval.Wetlands$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.approval.Wetlands$dic$dic,mod.lc.approval.Wetlands$waic$waic))




tex.lc.approval.Cultivated <- texreg::createTexreg(
  coef.names = mod.lc.approval.Cultivated$names.fixed,
  coef = mod.lc.approval.Cultivated$summary.lincomb.derived$mean,
  ci.low = mod.lc.approval.Cultivated$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.approval.Cultivated$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.approval.Cultivated$dic$dic,mod.lc.approval.Cultivated$waic$waic))




mod.lc.collab.coord.Deforested<- inla(form.collab.coord.Deforested,family='gaussian',data = focal.change.df,
                                  control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                  control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                  verbose=T,
                                  control.inla = list(
                                    correct = TRUE,
                                    correct.factor = correctionfactor))

mod.lc.collab.coord.Wetlands<- inla(form.collab.coord.Wetlands,family='gaussian',data = focal.change.df,
                                control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                verbose=T,
                                control.inla = list(
                                  correct = TRUE,
                                  correct.factor = correctionfactor))

mod.lc.collab.coord.Developed<- inla(form.collab.coord.Developed,family='gaussian',data = focal.change.df,
                                 control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                 control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                 verbose=T,
                                 control.inla = list(
                                   correct = TRUE,
                                   correct.factor = correctionfactor))

mod.lc.collab.coord.Cultivated<- inla(form.collab.coord.Cultivated,family='gaussian',data = focal.change.df,
                                  control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                  control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                  verbose=T,
                                  control.inla = list(
                                    correct = TRUE,
                                    correct.factor = correctionfactor))


tex.lc.collab.coord.Deforested <- texreg::createTexreg(
  coef.names = mod.lc.collab.coord.Deforested$names.fixed,
  coef = mod.lc.collab.coord.Deforested$summary.lincomb.derived$mean,
  ci.low = mod.lc.collab.coord.Deforested$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.collab.coord.Deforested$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.collab.coord.Deforested$dic$dic,mod.lc.collab.coord.Deforested$waic$waic))


tex.lc.collab.coord.Developed <- texreg::createTexreg(
  coef.names = mod.lc.collab.coord.Developed$names.fixed,
  coef = mod.lc.collab.coord.Developed$summary.lincomb.derived$mean,
  ci.low = mod.lc.collab.coord.Developed$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.collab.coord.Developed$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.collab.coord.Developed$dic$dic,mod.lc.collab.coord.Developed$waic$waic))




tex.lc.collab.coord.Cultivated <- texreg::createTexreg(
  coef.names = mod.lc.collab.coord.Cultivated$names.fixed,
  coef = mod.lc.collab.coord.Cultivated$summary.lincomb.derived$mean,
  ci.low = mod.lc.collab.coord.Cultivated$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.collab.coord.Cultivated$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.collab.coord.Cultivated$dic$dic,mod.lc.collab.coord.Cultivated$waic$waic))



tex.lc.collab.coord.Wetlands <- texreg::createTexreg(
  coef.names = mod.lc.collab.coord.Wetlands$names.fixed,
  coef = mod.lc.collab.coord.Wetlands$summary.lincomb.derived$mean,
  ci.low = mod.lc.collab.coord.Wetlands$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.collab.coord.Wetlands$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.collab.coord.Wetlands$dic$dic,mod.lc.collab.coord.Wetlands$waic$waic))





mod.lc.collab.part.Deforested<- inla(form.collab.part.Deforested,family='gaussian',data = focal.change.df,
                                  control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                  control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                  verbose=T,
                                  control.inla = list(
                                    correct = TRUE,
                                    correct.factor = correctionfactor))

mod.lc.collab.part.Wetlands<- inla(form.collab.part.Wetlands,family='gaussian',data = focal.change.df,
                                control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                verbose=T,
                                control.inla = list(
                                  correct = TRUE,
                                  correct.factor = correctionfactor))

mod.lc.collab.part.Developed<- inla(form.collab.part.Developed,family='gaussian',data = focal.change.df,
                                 control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                 control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                 verbose=T,
                                 control.inla = list(
                                   correct = TRUE,
                                   correct.factor = correctionfactor))

mod.lc.collab.part.Cultivated<- inla(form.collab.part.Cultivated,family='gaussian',data = focal.change.df,
                                  control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                  control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                  verbose=T,
                                  control.inla = list(
                                    correct = TRUE,
                                    correct.factor = correctionfactor))


tex.lc.collab.part.Deforested <- texreg::createTexreg(
  coef.names = mod.lc.collab.part.Deforested$names.fixed,
  coef = mod.lc.collab.part.Deforested$summary.lincomb.derived$mean,
  ci.low = mod.lc.collab.part.Deforested$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.collab.part.Deforested$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.collab.part.Deforested$dic$dic,mod.lc.collab.part.Deforested$waic$waic))


tex.lc.collab.part.Wetlands <- texreg::createTexreg(
  coef.names = mod.lc.collab.part.Wetlands$names.fixed,
  coef = mod.lc.collab.part.Wetlands$summary.lincomb.derived$mean,
  ci.low = mod.lc.collab.part.Wetlands$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.collab.part.Wetlands$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.collab.part.Wetlands$dic$dic,mod.lc.collab.part.Wetlands$waic$waic))




tex.lc.collab.part.Cultivated <- texreg::createTexreg(
  coef.names = mod.lc.collab.part.Cultivated$names.fixed,
  coef = mod.lc.collab.part.Cultivated$summary.lincomb.derived$mean,
  ci.low = mod.lc.collab.part.Cultivated$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.collab.part.Cultivated$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.collab.part.Cultivated$dic$dic,mod.lc.collab.part.Cultivated$waic$waic))



tex.lc.collab.part.Developed <- texreg::createTexreg(
  coef.names = mod.lc.collab.part.Developed$names.fixed,
  coef = mod.lc.collab.part.Developed$summary.lincomb.derived$mean,
  ci.low = mod.lc.collab.part.Developed$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.lc.collab.part.Developed$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.lc.collab.part.Developed$dic$dic,mod.lc.collab.part.Developed$waic$waic))




library(texreg)

if(onscreen)
{
htmlreg(l=list(tex.lc.approval.Developed,tex.lc.approval.Deforested,tex.lc.approval.Wetlands,tex.lc.approval.Cultivated),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Developed','Deforestation','Wetlands','Cultivated'),
        file = 'Output/Version1/approval.lc.table.html')
htmlreg(l=list(tex.lc.collab.part.Developed,tex.lc.collab.part.Deforested,tex.lc.collab.part.Wetlands,tex.lc.collab.part.Cultivated),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Developed','Deforestation','Wetlands','Cultivated'),
        file = 'Output/Version1/part.lc.table.html')
htmlreg(l=list(tex.lc.collab.coord.Developed,tex.lc.collab.coord.Deforested,
               tex.lc.collab.coord.Wetlands,tex.lc.collab.coord.Cultivated),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Developed','Deforestation','Wetlands','Cultivated'),
        file = 'Output/Version1/coord.lc.table.html')
}


if(!onscreen)
{
  htmlreg(l=list(mod.lc.approval.Developed,mod.lc.approval.Deforested,mod.lc.approval.Wetlands,mod.lc.approval.Cultivated),leading.zero=TRUE,
          omit.coef = c('b0'),ci.test = 0,digits = 3,
          custom.model.names = c('Developed','Deforestation','Wetlands','Cultivated'),
          file = '../Output/Version1/approval.lc.table.html')}
htmlreg(l=list(mod.lc.collab.part.Developed,mod.lc.collab.part.Deforested,mod.lc.collab.part.Wetlands,mod.lc.collab.part.Cultivated),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Developed','Deforestation','Wetlands','Cultivated'),
        file = '../Output/Version1/part.lc.table.html')}
htmlreg(l=list(mod.lc.collab.coord.Developed,mod.lc.collab.coord.Deforested,mod.lc.collab.coord.Wetlands,mod.lc.collab.coord.Cultivated),leading.zero=TRUE,
        omit.coef = c('b0'),ci.test = 0,digits = 3,
        custom.model.names = c('Developed','Deforestation','Wetlands','Cultivated'),
        file = '../Output/Version1/coord.lc.table.html')}

