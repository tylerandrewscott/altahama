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

focal.change.df$FIPS[nchar(as.character(focal.change.df$FIPS))==4] = paste0(0,focal.change.df$FIPS[nchar(as.character(focal.change.df$FIPS))==4]) 
total.file.names = paste0('Input/',grep('LandCover[[:digit:]]',list.files('Input/'),value=TRUE))
data.list = lapply(as.list(total.file.names),read.csv)
total.df = join_all(data.list,type='full')

area.df = total.df %>% group_by(Year,FIPS,StateAbbrev,CountyName) %>% summarise(Area = sum(SquareMiles)) %>% dplyr::select(-Year)

area.df <- area.df %>% data.frame(.) %>% dplyr::select(-Year) %>% mutate(uq = paste(FIPS,CountyName)) %>% filter(!duplicated(uq)) %>% dplyr::select(-uq)

focal.change.df <- join(focal.change.df,area.df)


focal.change.df$PercentChange <- 100*  (focal.change.df$TotalChange / focal.change.df$Area)

library(tidyr)
bea.west = read.csv('Input/bea_west_income.csv') ; bea.west = bea.west[-c(min(grep('Legend',bea.west$GeoFips)):nrow(bea.west)),] 
bea.mw = read.csv('Input/bea_mw_income.csv') ; bea.mw = bea.mw[-c(min(grep('Legend',bea.mw$GeoFips)):nrow(bea.mw)),] 
bea.atl = read.csv('Input/bea_atl_income.csv') ; bea.atl = bea.atl[-c(min(grep('Legend',bea.atl$GeoFips)):nrow(bea.atl)),] )
bea.gulf = read.csv('Input/bea_gulf_income.csv') 

; bea.gulf = bea.gulf[-c(min(grep('Legend',bea.gulf$GeoFips)):nrow(bea.gulf)),] 
bea.ne = read.csv('Input/bea_ne_income.csv') ; bea.ne = bea.ne[-c(min(grep('Legend',bea.ne$GeoFips)):nrow(bea.ne)),] 

bea.all = join_all(list(bea.west,bea.atl,bea.mw,bea.gulf,bea.ne),type='full')
bea.long = gather(bea.all,Year,Value,-Description,-GeoFips,-GeoName,-LineCode)

bea.long$Year = as.numeric(as.character(gsub('X','',bea.long$Year)))


bea.long = bea.long[nchar(as.character(bea.long$GeoFips))<=5,]
bea.long$GeoFips = as.character(bea.long$GeoFips)

bea.long$GeoFips[nchar(bea.long$GeoFips)==4] = paste0(0,bea.long$GeoFips[nchar(bea.long$GeoFips)==4])


focal.change.df[focal.change.df$FIPS %in% bea.long$GeoFips==FALSE,]

focal.change.df[focal.change.df$CountyName=='Zapata',]
bea.long[bea.long$GeoName=='Zapata',]


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







