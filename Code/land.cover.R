rm(list=ls())

Use.Percent.Change = TRUE
onscreen = TRUE
obs.years = c(1996,2001,2006,2010)
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


change.file.names = paste0('Input/',grep('LandCoverChange',list.files('Input/'),value=TRUE))
data.list = lapply(as.list(change.file.names),read.csv)
change.df = join_all(data.list,type='full')



change.df = change.df  %>% filter(StateAbbrev != 'DC',StateAbbrev != 'VT',fromClass != toClass, ToYear-FromYear<=5) %>% dplyr::select(-CCAP_ClassID)

levels(change.df$fromClass) = levels(change.df$toClass) = c("Bare" , "Ag"        ,             "Forest"       ,        "Developed"      ,     "Wetland"      ,   
                                                            "Wetland"   ,  "Wetland"  ,   "Wetland"  ,"Forest"   ,            "Grassland"     ,                
                                                            "Developed"   ,    "Developed"    ,    "Developed"   ,  "Forest"   ,                NA     ,                     
                                                            "Wetland"      ,   "Wetland"   , "Wetland"  ,  "Wetland", "Ag"    ,               
                                                            "Scrub/Shrub"   ,    "Shore/Water"   ,        "Shore/Water"    ,      NA        , NA)

change.df  = change.df  %>% filter(!is.na(toClass),!is.na(fromClass)) 

change.df$FromTo = paste(change.df$fromClass,change.df$toClass,sep='.')

from.df = change.df %>% dplyr::group_by(FIPS,FromYear,ToYear,CountyName,StateAbbrev,fromClass)%>% dplyr::summarise(FromSquareMiles = sum(SquareMiles)) %>% dplyr::rename(Class = fromClass) 

to.df = change.df %>% dplyr::group_by(FIPS,FromYear,ToYear,CountyName,StateAbbrev,toClass) %>% dplyr::summarise(ToSquareMiles = sum(SquareMiles)) %>% dplyr::rename(Class = toClass)




net.change.df = join(to.df,from.df,type='full')

net.change.df = net.change.df %>% dplyr::mutate(NetChange = ToSquareMiles - FromSquareMiles,
                                                FIPS = ifelse(nchar(as.character(FIPS))==4,paste0(0,as.character(FIPS)),as.character(FIPS)))

net.change.df = net.change.df %>% dplyr::select(-ToSquareMiles,-FromSquareMiles) %>% dplyr::mutate(Class = paste0('Change.',Class)) %>% spread(Class,NetChange)


lc96 = read.csv('Input/LandCover1996.csv')
lc01 = read.csv('Input/LandCover2001.csv')
lc06 = read.csv('Input/LandCover2006.csv')
lc10 = read.csv('Input/LandCover2010.csv')
lc = join_all(list(lc96,lc01,lc06,lc10),type='full')

levels(lc$LCName) = c("Bare" ,       "Ag"        ,             "Forest"       ,        "Wetland"      ,     "Wetland"      ,   
                      "Wetland"   ,  "Wetland"  ,             "Forest"  ,         "Grassland"   ,            "Developed"     ,                
                      "Developed"   ,    "Developed"    ,    "Forest"   ,        "Developed"   ,             "Shore/Water"     ,                     
                      "Wetland"      ,   "Wetland"   ,       "Wetland"  ,       "Wetland",               "Ag"    ,               
                      "Scrub/Shrub"   ,    NA   ,               NA  ,           "Shore/Water"   )    


TotalLandCover = lc %>% filter(!is.na(LCName)) %>% dplyr::select(-CCAP_ClassID,-StateFIPS,-CountyName) %>% dplyr::mutate(FIPS = ifelse(nchar(as.character(FIPS))==4,paste0(0,as.character(FIPS)),as.character(FIPS))) %>% 
  dplyr::rename(CoverTotal = LCName) %>% group_by(FIPS,Year,CoverTotal) %>% dplyr::summarise(SquareMiles = sum(SquareMiles)) %>% dplyr::mutate(CoverTotal = paste0('Total.',CoverTotal)) %>%
  spread(CoverTotal,SquareMiles)


net.change.df$Year = net.change.df$FromYear 

net.change.df = net.change.df %>% dplyr::mutate(Year = FromYear) %>% join(.,TotalLandCover)

net.change.df =  TotalLandCover %>% gather(Type,Area,-FIPS,-Year) %>% dplyr::group_by(FIPS,Year) %>% dplyr::summarise(CountyArea = sum(Area)) %>% join(net.change.df,.) 


bea.mw = read.csv('Input/bea_mw_income.csv')
bea.atl = read.csv('Input/bea_atl_income.csv')
bea.tex = read.csv('Input/bea_tex_inc.csv')
bea.west = read.csv('Input/bea_west_income.csv')
bea.gulf = read.csv('Input/bea_gulf_income.csv')
bea.ne = read.csv('Input/bea_ne_income.csv')
bea.va = read.csv('Input/bea_va_income.csv')
bea = join_all(list(bea.mw,bea.atl,bea.tex,bea.west,bea.ne,bea.va,bea.gulf),type='full')

beal = bea %>% gather(Year,Value,-GeoFips,-GeoName,-LineCode,-Description)
beal$Year =  gsub('X','',beal$Year)
beal$Description = as.character(beal$Description)
beal$Description[grep('Population',beal$Description)] = 'Population'
beal$Description[grep('Per capita',beal$Description)] = 'Income.Per.Capita'
beal = beal %>% filter(Description %in% c('Population','Income.Per.Capita')) %>% dplyr::select(-LineCode) %>%
  dplyr::mutate(FIPS = ifelse(nchar(as.character(GeoFips))==4,paste0(0,as.character(GeoFips)),as.character(GeoFips))) %>% spread(.,Description,Value) %>%
  filter(Year %in% obs.years) %>% dplyr::mutate(ToYear = Year,FromYear = Year) %>% dplyr::select(-Year,-GeoFips,-GeoName) 


net.change.df$From.Population = beal$Population[match(paste0(net.change.df$FIPS,net.change.df$FromYear),paste0(beal$FIPS,beal$FromYear))]
net.change.df$To.Population = beal$Population[match(paste0(net.change.df$FIPS,net.change.df$ToYear),paste0(beal$FIPS,beal$ToYear))]
net.change.df$From.Per.Capita.Income = beal$Income.Per.Capita[match(paste0(net.change.df$FIPS,net.change.df$FromYear),paste0(beal$FIPS,beal$FromYear))]
net.change.df$To.Per.Capita.Income = beal$Income.Per.Capita[match(paste0(net.change.df$FIPS,net.change.df$ToYear),paste0(beal$FIPS,beal$ToYear))]


locgov = read.csv('Input/COG_2012_ORG014_with_ann.csv')
locgov$GEO.id2[nchar(as.character(locgov$GEO.id2))==4] = paste0(0,locgov$GEO.id2[nchar(as.character(locgov$GEO.id2))==4])

net.change.df = locgov %>%dplyr::rename(FIPS = GEO.id2) %>% dplyr::select(FIPS,total_govts,total_subcounty,municipal,special_districts,total_subcounty) %>% join(net.change.df,.)

al96 = openxlsx::read.xlsx('Input/allhlcn96.xlsx',sheet=1)
al01 = openxlsx::read.xlsx('Input/allhlcn01.xlsx',sheet=1)
al06 = openxlsx::read.xlsx('Input/allhlcn06.xlsx',sheet=1)
al10 = openxlsx::read.xlsx('Input/allhlcn10.xlsx',sheet=1)
ec.all = join_all(list(al96,al01,al06,al10),type='full')

ec.nat = ec.all %>% filter(Industry =='Natural resources and mining') %>% dplyr::select(Area.Code,Annual.Average.Employment,Annual.Total.Wages,Year) %>% 
  dplyr::rename(Annual.Average.Employ.NaturalRes= Annual.Average.Employment, Annual.TotalWages.NaturalRes= Annual.Total.Wages)


ec.const = filter(ec.all,Industry=='Construction')%>% dplyr::select(Area.Code,Annual.Average.Employment,Annual.Total.Wages,Year) %>% 
  dplyr::rename(Annual.Average.Employ.Const= Annual.Average.Employment, Annual.TotalWages.Const= Annual.Total.Wages)

ec.recreat = filter(ec.all,Industry=='Leisure and hospitality')%>% dplyr::select(Area.Code,Annual.Average.Employment,Annual.Total.Wages,Year) %>% 
  dplyr::rename(Annual.Average.Employ.Recreat= Annual.Average.Employment, Annual.TotalWages.Recreat= Annual.Total.Wages)

ec.man = filter(ec.all,Industry=='Manufacturing')%>% dplyr::select(Area.Code,Annual.Average.Employment,Annual.Total.Wages,Year) %>% 
  dplyr::rename(Annual.Average.Employ.Manuf = Annual.Average.Employment, Annual.TotalWages.Manuf = Annual.Total.Wages)

ec.industries = join_all(list(ec.nat,ec.const,ec.recreat,ec.man)) %>%dplyr::rename(FIPS = Area.Code)


net.change.df = join(net.change.df,ec.industries)

net.change.df = net.change.df %>% mutate(StateAbbrev = as.character(StateAbbrev)) 
  
  
  
state.ref = data.frame(state.abb,state.name)
cnp.history = fetchGoogle("https://docs.google.com/spreadsheets/d/1dbSJRtuSah56zBwjk-YySYwJ09ryURrofbJ6Nne5rv0/pub?output=csv")
cnp.history <- cnp.history %>% dplyr::mutate(Conditional = mdy(Conditional),
                                             Conditional.Year = year(Conditional),
                                             Full = mdy(Full),Full.Year =year(Full),state.name = State) %>% 
  dplyr::select(State,Conditional,Full,Full.Year,Conditional.Year) %>% dplyr::rename(state.name = State)
cnp.history <- cnp.history %>% join(.,state.ref) %>% dplyr::rename(StateAbbrev = state.abb) %>% filter(!is.na(StateAbbrev))

net.change.df = net.change.df %>% join(.,cnp.history) %>%
  dplyr::mutate(Full.Approval.Active = ifelse(is.na(Full.Year),0,ifelse(Full.Year<=FromYear,1,0)),
                Cond.Approval.Active = ifelse(is.na(Conditional.Year),0,ifelse(Conditional.Year<=FromYear,1,0)))


plan.attributes = fetchGoogle("https://docs.google.com/spreadsheets/d/18R3pvK_RcajdhxPEnQIEq9ZJFFr392uMHaHLWNA5yek/pub?output=csv")

plan.attributes = plan.attributes %>% dplyr::mutate(Coord.Institution = mdy(Coord.Institution),
                                                    Coord.Formal.Agreements = mdy(Coord.Formal.Agreements),
                                                    Coord.Spec.Responsibilities = mdy(Coord.Spec.Responsibilities),
                                                    Part.Input = mdy(Part.Input),
                                                    Part.Advisory = mdy(Part.Advisory),
                                                    Part.Outreach = mdy(Part.Outreach)) %>%
  dplyr::rename(state.name = State.Name)


net.change.df = join(net.change.df,plan.attributes) %>% dplyr::mutate(
  Coord.Institution.Active = ifelse(is.na(year(Coord.Institution)),0,ifelse(year(Coord.Institution)<=FromYear,1,0)),
  Coord.Spec.Responsibilities.Active = ifelse(is.na(year(Coord.Spec.Responsibilities)),0,ifelse(year(Coord.Spec.Responsibilities)<=FromYear,1,0)),
  Coord.Formal.Agreements.Active = ifelse(is.na(year(Coord.Formal.Agreements)),0,ifelse(year(Coord.Formal.Agreements)<=FromYear,1,0)),
  Part.Input.Active = ifelse(is.na(year(Part.Input)),0,ifelse(year(Part.Input)<=FromYear,1,0)),
  Part.Advisory.Active = ifelse(is.na(year(Part.Advisory)),0,ifelse(year(Part.Advisory)<=FromYear,1,0)),
  Part.Outreach.Active = ifelse(is.na(year(Part.Outreach)),0,ifelse(year(Part.Outreach)<=FromYear,1,0)))



focal = fetchGoogle("https://docs.google.com/spreadsheets/d/1j3AxfTyOhYimF0ea_Puctma5qYNgOzjfPxKuMT-zw6A/pub?output=csv")

cat.approv = focal %>% dplyr::select(-contains('Background'),-Measure.Source,-Sub.Category,-Measure.Num,-Sub.Topic,-Measure.Text,-Illinois) %>%
  gather(State,Approval.Date,-Measure.Name,-Topic)  %>% mutate(Approval.Year = year(mdy(Approval.Date))) %>% dplyr::select(-Approval.Date)

cats.approved = cat.approv %>% dplyr::select(-Measure.Name) %>% dplyr::group_by(Topic,State) %>% dplyr::mutate(Approval.Year  = ifelse(is.na(Approval.Year),2040,Approval.Year)) %>%
  dplyr::summarise(Year.Cat.Approved = max(Approval.Year)) %>% spread(Topic,Year.Cat.Approved) %>%  
  dplyr::rename(Approved.Developed = urban,Approved.Forest = forestry, Approved.Ag = agriculture) %>% 
dplyr::group_by(State) %>% dplyr::mutate(Approved.Wetland = max(hydromodification,wetlands.riparian)) %>%
  dplyr::select(-wetlands.riparian,-hydromodification)



net.change.df = net.change.df %>% dplyr::mutate(State = gsub(' ','',as.character(state.name))) %>% join(.,cats.approved) %>% 
  dplyr::mutate(Approval.Ag.Active = ifelse(FromYear >= Approved.Ag,1,0),
         Approval.Forest.Active = ifelse(FromYear >= Approved.Forest,1,0),
         Approval.Wetland.Active = ifelse(FromYear >= Approved.Wetland,1,0),
         Approval.Developed.Active = ifelse(FromYear >= Approved.Developed,1,0))


state.gdp <- read.csv('Input/state.gdp.pc.csv')
state.gdp <- state.gdp %>% dplyr::select(-IndustryId,-IndustryClassification,-Description,-GeoFIPS,-ComponentId,-ComponentName,-Region) %>%
  filter(GeoName != 'United States') %>%dplyr::rename(state.name = GeoName) %>% gather(Year,GDP,-state.name)
state.gdp$Year = gsub('X','',state.gdp$Year)

net.change.df = net.change.df %>% filter(StateAbbrev %in% c("VT",'IL') ==FALSE)

library(RCurl)
library(mosaic)
library(lubridate)
library(gdata)

# bea.mw = read.csv('Input/bea_mw_income.csv')
# bea.atl = read.csv('Input/bea_atl_income.csv')
# bea.tex = read.csv('Input/bea_tex_inc.csv')
# bea.west = read.csv('Input/bea_west_income.csv')
# bea.gulf = read.csv('Input/bea_gulf_income.csv')
# bea.ne = read.csv('Input/bea_ne_income.csv')
# bea.va = read.csv('Input/bea_va_income.csv')
# bea = join_all(list(bea.mw,bea.atl,bea.tex,bea.west,bea.ne,bea.va,bea.gulf),type='full')
# beal = gather(bea,Year,Value,-GeoFips,-GeoName,-LineCode,-Description)
# beal$Year = gsub('X','',beal$Year)
# beal = beal[grep('capita',beal$Description),]
# beal$GeoFips[nchar(beal$GeoFips)==4] = paste0(0,beal$GeoFips[nchar(beal$GeoFips)==4])
# beal$uq = paste(beal$GeoFips,beal$Year)

treat.indicator = net.change.df %>% dplyr::select(StateAbbrev,Full.Approval.Active) %>% 
  dplyr::group_by(StateAbbrev) %>%
  dplyr::summarise(amean = mean(Full.Approval.Active)) %>%
  dplyr::mutate(ever.approved = ifelse(amean>0,1,0)) %>% dplyr::select(-amean)


net.change.df = join(net.change.df,treat.indicator)



us.county = readOGR(dsn = '../duckabush/','tl_2013_us_county')
us.county = us.county[as.numeric(as.character(us.county$STATEFP))<=56,]
us.county = us.county[as.numeric(as.character(us.county$STATEFP))%in% c(2,3,14,15)==FALSE,]
us.county$CFIPS = as.character(paste0(us.county$STATEFP,us.county$COUNTYFP))
us.county$CFIPS[nchar(as.character(us.county$CFIPS))==4] = paste0(0,us.county$CFIPS[nchar(as.character(us.county$CFIPS))==4])
us.county = us.county[us.county$CFIPS %in% net.change.df$FIPS,]
row.names(us.county) <- as.character(seq(1,nrow(us.county)))
us.county@data$rows <- row.names(us.county)
county.nb <- poly2nb(us.county,queen=FALSE)
nb2INLA("county.adj", county.nb)
graph = inla.read.graph(filename='county.adj')
county.ADJ = 'county.adj'
us.county@data$FIPS = us.county@data$CFIPS

net.change.df$rowID <-  as.numeric(as.character(row.names(us.county)[match(net.change.df$FIPS, us.county@data$FIPS)]))

net.change.df$total_subcounty[is.na(net.change.df$total_subcounty)] = 0

net.change.df = net.change.df %>% 
  dplyr::mutate(Annual.Average.Employ.Const.1k = Annual.Average.Employ.Const/1000,
                Annual.Average.Employ.NaturalRes.1k = Annual.Average.Employ.NaturalRes/1000,
                From.Per.Capita.Income.1k = as.numeric(as.character(From.Per.Capita.Income)) / 1000,
                Perc.Change.Per.Capita.Income = 100 * ((as.numeric(as.character(To.Per.Capita.Income)) - 
                                                          as.numeric(as.character(From.Per.Capita.Income)))/as.numeric(as.character(From.Per.Capita.Income))),
                From.Population.Density.100pSqM =  (as.numeric(as.character(From.Population))/CountyArea)/100,
                Perc.Population.Change =  100 * ((as.numeric(as.character(To.Population)) - as.numeric(as.character(From.Population)))/
                                                   as.numeric(as.character(From.Population))),
                Area.100sqM = CountyArea/100,
                Prop.Forested = 100 * (Total.Forest/CountyArea),
                Prop.Wetland = 100 * (Total.Wetland/CountyArea),
                Prop.Developed = 100 * (Total.Developed/CountyArea),
                Prop.Cultivated = 100 * (Total.Ag/CountyArea),
                Perc.Change.Forest = 100 * (Change.Forest/(Total.Forest+0.01)),
                Perc.Change.Developed = 100 * (Change.Developed/(Total.Developed+0.01)),
                Perc.Change.Ag = 100 * (Change.Ag/(Total.Ag+0.01)),
                Perc.Change.Wetland = 100 * (Change.Wetland/(Total.Wetland+0.01)),
                Prop.Employ.NaturalRes =  100 * (Annual.Average.Employ.NaturalRes/as.numeric(From.Population)),
                subcounty_density = total_subcounty/Area.100sqM
  )



#model settings
correctionfactor = 10
pintercept = 0.001
DIC = TRUE
WAIC = TRUE
CPO = TRUE



### Land Cover Chqnge Models
form.approval.Developed.Perc = Perc.Change.Developed ~ 1 + From.Population.Density.100pSqM+ 
  Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  #ever.approved +
  #ever.approved:Full.Approval.Active +
  #Cond.Approval.Active +
  #Full.Approval.Active +
  Approval.Developed.Active +
  #f(StateAbbrev,model='iid') +
  f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.approval.Developed.Perc<- inla(form.approval.Developed.Perc,family='gaussian',data = net.change.df,
                                   control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                   control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                   verbose=T,
                                   control.inla = list(
                                     correct = TRUE,
                                     correct.factor = correctionfactor))


form.approval.Wetland.Perc = Perc.Change.Wetland ~ 1 + From.Population.Density.100pSqM+ Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  #ever.approved +
  #ever.approved:Full.Approval.Active +
  #Cond.Approval.Active +
  #Full.Approval.Active +
  Approval.Wetland.Active +
  #f(StateAbbrev,model='iid') +
  f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.approval.Wetland.Perc<- inla(form.approval.Wetland.Perc,family='gaussian',data = net.change.df,
                                 control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                 control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                 verbose=T,
                                 control.inla = list(
                                   correct = TRUE,
                                   correct.factor = correctionfactor))

form.approval.Forest.Perc = Perc.Change.Forest ~ 1 + From.Population.Density.100pSqM+ Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+ 
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  #ever.approved +
  #ever.approved:Full.Approval.Active +
  #Cond.Approval.Active + Full.Approval.Active +
  Approval.Forest.Active +
  #f(StateAbbrev,model='iid') +
  f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.approval.Forest.Perc <- inla(form.approval.Forest.Perc,family='gaussian',data = net.change.df,
                                 control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                 control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                 verbose=T,
                                 control.inla = list(
                                   correct = TRUE,
                                   correct.factor = correctionfactor))

form.approval.Ag.Perc = Perc.Change.Ag ~ 1 + From.Population.Density.100pSqM+ Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  #ever.approved +
  #ever.approved:Full.Approval.Active +
 # Cond.Approval.Active +
  #  Full.Approval.Active +
  Approval.Ag.Active +
  #f(StateAbbrev,model='iid') +
  f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.approval.Ag.Perc <- inla(form.approval.Ag.Perc,family='gaussian',data = net.change.df,
                             control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                             control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                             verbose=T,
                             control.inla = list(
                               correct = TRUE,
                               correct.factor = correctionfactor))


### Coordination Models
form.coordination.Developed.Perc = Perc.Change.Developed ~ 1 + From.Population.Density.100pSqM+ Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  Approval.Developed.Active +
  Coord.Formal.Agreements.Active + 
  Coord.Spec.Responsibilities.Active + 
  Coord.Institution.Active+
  f(StateAbbrev,model='iid') + f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.coordination.Developed.Perc<- inla(form.coordination.Developed.Perc,family='gaussian',data = net.change.df,
                                       control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                       control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                       verbose=T,
                                       control.inla = list(
                                         correct = TRUE,
                                         correct.factor = correctionfactor))

form.coordination.Wetland.Perc = Perc.Change.Wetland ~ 1 + From.Population.Density.100pSqM+ Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  Approval.Wetland.Active +
  Coord.Formal.Agreements.Active + 
  Coord.Spec.Responsibilities.Active + 
  Coord.Institution.Active+
  f(StateAbbrev,model='iid') + f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.coordination.Wetland.Perc<- inla(form.coordination.Wetland.Perc,family='gaussian',data = net.change.df,
                                     control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                     control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                     verbose=T,
                                     control.inla = list(
                                       correct = TRUE,
                                       correct.factor = correctionfactor))

form.coordination.Forest.Perc = Perc.Change.Forest ~ 1 + From.Population.Density.100pSqM+ Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  Approval.Forest.Active +
  Coord.Formal.Agreements.Active + 
  Coord.Spec.Responsibilities.Active + 
  Coord.Institution.Active+
  f(StateAbbrev,model='iid') + f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.coordination.Forest.Perc <- inla(form.coordination.Forest.Perc,family='gaussian',data = net.change.df,
                                     control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                     control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                     verbose=T,
                                     control.inla = list(
                                       correct = TRUE,
                                       correct.factor = correctionfactor))

form.coordination.Ag.Perc = Perc.Change.Ag ~ 1 + From.Population.Density.100pSqM+ Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  Approval.Ag.Active +
  Coord.Formal.Agreements.Active + 
  Coord.Spec.Responsibilities.Active + 
  Coord.Institution.Active+
  f(StateAbbrev,model='iid') + f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.coordination.Ag.Perc <- inla(form.coordination.Ag.Perc,family='gaussian',data = net.change.df,
                                 control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                 control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                 verbose=T,
                                 control.inla = list(
                                   correct = TRUE,
                                   correct.factor = correctionfactor))


### Participation Models
form.participation.Developed.Perc = Perc.Change.Developed ~ 1 + From.Population.Density.100pSqM+ Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  Approval.Developed.Active +
  Part.Outreach.Active+
  Part.Advisory.Active+
  Part.Input.Active +
  f(StateAbbrev,model='iid') + f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.participation.Developed.Perc<- inla(form.participation.Developed.Perc,family='gaussian',data = net.change.df,
                                        control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                        control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                        verbose=T,
                                        control.inla = list(
                                          correct = TRUE,
                                          correct.factor = correctionfactor))

form.participation.Wetland.Perc = Perc.Change.Wetland ~ 1 + From.Population.Density.100pSqM+ Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  Approval.Wetland.Active +
  Part.Outreach.Active+
  Part.Advisory.Active+
  Part.Input.Active +
  f(StateAbbrev,model='iid') + f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.participation.Wetland.Perc<- inla(form.participation.Wetland.Perc,family='gaussian',data = net.change.df,
                                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                      verbose=T,
                                      control.inla = list(
                                        correct = TRUE,
                                        correct.factor = correctionfactor))

form.participation.Forest.Perc = Perc.Change.Forest ~ 1 + From.Population.Density.100pSqM+ Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  Approval.Forest.Active +
  Part.Outreach.Active+
  Part.Advisory.Active+
  Part.Input.Active +
  f(StateAbbrev,model='iid') + f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.participation.Forest.Perc <- inla(form.participation.Forest.Perc,family='gaussian',data = net.change.df,
                                      control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                      control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                      verbose=T,
                                      control.inla = list(
                                        correct = TRUE,
                                        correct.factor = correctionfactor))

form.participation.Ag.Perc = Perc.Change.Ag ~ 1 + From.Population.Density.100pSqM+ Perc.Population.Change +
  From.Per.Capita.Income.1k + Perc.Change.Per.Capita.Income + 
  Prop.Employ.NaturalRes+
  #Annual.Average.Employ.Const.1k +
  Prop.Forested + Prop.Developed + Prop.Wetland + Prop.Cultivated + 
  subcounty_density + Area.100sqM+
  Approval.Ag.Active +
  Part.Outreach.Active+
  Part.Advisory.Active+
  Part.Input.Active +
  f(StateAbbrev,model='iid') + f(ToYear, model = 'iid') + f(rowID,model='bym',graph = county.ADJ)

mod.participation.Ag.Perc <- inla(form.participation.Ag.Perc,family='gaussian',data = net.change.df,
                                  control.compute=list(dic=DIC, cpo=CPO,waic=WAIC),
                                  control.fixed= list(prec.intercept = pintercept, correlation.matrix = TRUE),
                                  verbose=T,
                                  control.inla = list(
                                    correct = TRUE,
                                    correct.factor = correctionfactor))


# 
# tex.approval.Developed <- texreg::createTexreg(
#   coef.names = mod.approval.Developed$names.fixed,
#   coef = mod.approval.Developed$summary.lincomb.derived$mean,
#   ci.low = mod.approval.Developed$summary.lincomb.derived$`0.025quant`,
#   ci.up = mod.approval.Developed$summary.lincomb.derived$`0.975quant`,
#   gof.names = c('DIC','WAIC'),
#   gof =  c(mod.approval.Developed$dic$dic,mod.approval.Developed$waic$waic))
# 
# 
# tex.approval.Forest <- texreg::createTexreg(
#   coef.names = mod.approval.Forest$names.fixed,
#   coef = mod.approval.Forest$summary.lincomb.derived$mean,
#   ci.low = mod.approval.Forest$summary.lincomb.derived$`0.025quant`,
#   ci.up = mod.approval.Forest$summary.lincomb.derived$`0.975quant`,
#   gof.names = c('DIC','WAIC'),
#   gof =  c(mod.approval.Forest$dic$dic,mod.approval.Forest$waic$waic))
# 
# 
# tex.approval.Ag <- texreg::createTexreg(
#   coef.names = mod.approval.Ag$names.fixed,
#   coef = mod.approval.Ag$summary.lincomb.derived$mean,
#   ci.low = mod.approval.Ag$summary.lincomb.derived$`0.025quant`,
#   ci.up = mod.approval.Ag$summary.lincomb.derived$`0.975quant`,
#   gof.names = c('DIC','WAIC'),
#   gof =  c(mod.approval.Ag$dic$dic,mod.approval.Ag$waic$waic))
# 
# 
# tex.approval.Wetland <- texreg::createTexreg(
#   coef.names = mod.approval.Wetland$names.fixed,
#   coef = mod.approval.Wetland$summary.lincomb.derived$mean,
#   ci.low = mod.approval.Wetland$summary.lincomb.derived$`0.025quant`,
#   ci.up = mod.approval.Wetland$summary.lincomb.derived$`0.975quant`,
#   gof.names = c('DIC','WAIC'),
#   gof =  c(mod.approval.Wetland$dic$dic,mod.approval.Wetland$waic$waic))
# 


tex.approval.Developed.Perc <- texreg::createTexreg(
  coef.names = mod.approval.Developed.Perc$names.fixed,
  coef = mod.approval.Developed.Perc$summary.lincomb.derived$mean,
  ci.low = mod.approval.Developed.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.approval.Developed.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.approval.Developed.Perc$dic$dic,mod.approval.Developed.Perc$waic$waic))


tex.approval.Forest.Perc <- texreg::createTexreg(
  coef.names = mod.approval.Forest.Perc$names.fixed,
  coef = mod.approval.Forest.Perc$summary.lincomb.derived$mean,
  ci.low = mod.approval.Forest.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.approval.Forest.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.approval.Forest.Perc$dic$dic,mod.approval.Forest.Perc$waic$waic))


tex.approval.Ag.Perc <- texreg::createTexreg(
  coef.names = mod.approval.Ag.Perc$names.fixed,
  coef = mod.approval.Ag.Perc$summary.lincomb.derived$mean,
  ci.low = mod.approval.Ag.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.approval.Ag.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.approval.Ag.Perc$dic$dic,mod.approval.Ag.Perc$waic$waic))


tex.approval.Wetland.Perc <- texreg::createTexreg(
  coef.names = mod.approval.Wetland.Perc$names.fixed,
  coef = mod.approval.Wetland.Perc$summary.lincomb.derived$mean,
  ci.low = mod.approval.Wetland.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.approval.Wetland.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.approval.Wetland.Perc$dic$dic,mod.approval.Wetland.Perc$waic$waic))

tex.coordination.Developed.Perc <- texreg::createTexreg(
  coef.names = mod.coordination.Developed.Perc$names.fixed,
  coef = mod.coordination.Developed.Perc$summary.lincomb.derived$mean,
  ci.low = mod.coordination.Developed.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.coordination.Developed.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.coordination.Developed.Perc$dic$dic,mod.coordination.Developed.Perc$waic$waic))


tex.coordination.Forest.Perc <- texreg::createTexreg(
  coef.names = mod.coordination.Forest.Perc$names.fixed,
  coef = mod.coordination.Forest.Perc$summary.lincomb.derived$mean,
  ci.low = mod.coordination.Forest.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.coordination.Forest.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.coordination.Forest.Perc$dic$dic,mod.coordination.Forest.Perc$waic$waic))


tex.coordination.Ag.Perc <- texreg::createTexreg(
  coef.names = mod.coordination.Ag.Perc$names.fixed,
  coef = mod.coordination.Ag.Perc$summary.lincomb.derived$mean,
  ci.low = mod.coordination.Ag.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.coordination.Ag.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.coordination.Ag.Perc$dic$dic,mod.coordination.Ag.Perc$waic$waic))


tex.coordination.Wetland.Perc <- texreg::createTexreg(
  coef.names = mod.coordination.Wetland.Perc$names.fixed,
  coef = mod.coordination.Wetland.Perc$summary.lincomb.derived$mean,
  ci.low = mod.coordination.Wetland.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.coordination.Wetland.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.coordination.Wetland.Perc$dic$dic,mod.coordination.Wetland.Perc$waic$waic))



tex.participation.Developed.Perc <- texreg::createTexreg(
  coef.names = mod.participation.Developed.Perc$names.fixed,
  coef = mod.participation.Developed.Perc$summary.lincomb.derived$mean,
  ci.low = mod.participation.Developed.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.participation.Developed.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.participation.Developed.Perc$dic$dic,mod.participation.Developed.Perc$waic$waic))


tex.participation.Forest.Perc <- texreg::createTexreg(
  coef.names = mod.participation.Forest.Perc$names.fixed,
  coef = mod.participation.Forest.Perc$summary.lincomb.derived$mean,
  ci.low = mod.participation.Forest.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.participation.Forest.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.participation.Forest.Perc$dic$dic,mod.participation.Forest.Perc$waic$waic))


tex.participation.Ag.Perc <- texreg::createTexreg(
  coef.names = mod.participation.Ag.Perc$names.fixed,
  coef = mod.participation.Ag.Perc$summary.lincomb.derived$mean,
  ci.low = mod.participation.Ag.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.participation.Ag.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.participation.Ag.Perc$dic$dic,mod.participation.Ag.Perc$waic$waic))


tex.participation.Wetland.Perc <- texreg::createTexreg(
  coef.names = mod.participation.Wetland.Perc$names.fixed,
  coef = mod.participation.Wetland.Perc$summary.lincomb.derived$mean,
  ci.low = mod.participation.Wetland.Perc$summary.lincomb.derived$`0.025quant`,
  ci.up = mod.participation.Wetland.Perc$summary.lincomb.derived$`0.975quant`,
  gof.names = c('DIC','WAIC'),
  gof =  c(mod.participation.Wetland.Perc$dic$dic,mod.participation.Wetland.Perc$waic$waic))



library(texreg)

if(onscreen)
{
  htmlreg(l=list(tex.approval.Developed.Perc,tex.approval.Forest.Perc,tex.approval.Wetland.Perc,tex.approval.Ag.Perc),leading.zero=TRUE,
          omit.coef = c('(Intercept)'),ci.test = 0,digits = 2,caption = 'Percent Net Change',
          custom.model.names = c('Developed','Forested','Wetland','Ag'),single.row =TRUE,
          file = 'Output/Version1/approval.table.html')
  htmlreg(l=list(tex.participation.Developed.Perc,tex.participation.Forest.Perc,tex.participation.Wetland.Perc,tex.participation.Ag.Perc),leading.zero=TRUE,
          omit.coef = c('(Intercept)'),ci.test = 0,digits = 2,caption = 'Percent Net Change',
          custom.model.names = c('Developed','Forested','Wetland','Ag'),single.row =TRUE,
          file = 'Output/Version1/part.table.html')
  htmlreg(l=list(tex.coordination.Developed.Perc,tex.coordination.Forest.Perc,
                 tex.coordination.Wetland.Perc,tex.coordination.Ag.Perc),leading.zero=TRUE,
          omit.coef = c('(Intercept)'),ci.test = 0,digits = 2,caption = 'Percent Net Change',
          custom.model.names = c('Developed','Forested','Wetland','Ag'),single.row =TRUE,
          file = 'Output/Version1/coord.table.html')
}



if(!onscreen)
{
  htmlreg(l=list(tex.approval.Developed.Perc,tex.approval.Forest.Perc,tex.approval.Wetland.Perc,tex.approval.Ag.Perc),leading.zero=TRUE,
          omit.coef = c('(Intercept)'),ci.test = 0,digits = 2,caption = 'Percent Net Change',
          custom.model.names = c('Developed','Forested','Wetland','Ag'),
          file = '../Output/Version1/approval.table.html')
  htmlreg(l=list(tex.participation.Developed.Perc,tex.participation.Forest.Perc,tex.participation.Wetland.Perc,tex.participation.Ag.Perc),leading.zero=TRUE,
          omit.coef = c('(Intercept)'),ci.test = 0,digits = 2,caption = 'Percent Net Change',
          custom.model.names = c('Developed','Forested','Wetland','Ag'),
          file = '../Output/Version1/part.table.html')
  htmlreg(l=list(tex.coordination.Developed.Perc,tex.coordination.Forest.Perc,
                 tex.coordination.Wetland.Perc,tex.coordination.Ag.Perc),leading.zero=TRUE,
          omit.coef = c('(Intercept)'),ci.test = 0,digits = 2,caption = 'Percent Net Change',
          custom.model.names = c('Developed','Forested','Wetland','Ag'),
          file = '../Output/Version1/coord.table.html')}


sum.changes = net.change.df %>% dplyr::select(Perc.Change.Developed,Perc.Change.Forest,Perc.Change.Ag,Perc.Change.Wetland)
library(stargazer)
stargazer(sum.changes,title='Land Cover Change Variables',
          out = 'Output/Version1/cover.summary.html',
          data.frame=TRUE,style = 'jpam',type = 'html',digits = 2)

library(coefplot)
# 
# temp = mod.approval.Ag.Perc$summary.fixed %>% dplyr::mutate(Vars = rownames(.)) %>%dplyr::rename(upper = `0.975quant`,lower = `0.025quant`) %>% filter(Vars != '(Intercept)') %>%
#   dplyr::mutate(Where.Plot = 1:nrow(.),Mod = 'Ag')
# temp1 = mod.approval.Forest.Perc$summary.fixed %>% dplyr::mutate(Vars = rownames(.)) %>%dplyr::rename(upper = `0.975quant`,lower = `0.025quant`) %>% filter(Vars != '(Intercept)') %>%
#   dplyr::mutate(Where.Plot = 1:nrow(.),Mod = 'Forest')
# temp2 = mod.approval.Developed.Perc$summary.fixed %>% dplyr::mutate(Vars = rownames(.)) %>%dplyr::rename(upper = `0.975quant`,lower = `0.025quant`) %>% filter(Vars != '(Intercept)') %>%
#   dplyr::mutate(Where.Plot = 1:nrow(.),Mod = 'Developed')
# temp3 = mod.approval.Wetland.Perc$summary.fixed %>% dplyr::mutate(Vars = rownames(.)) %>%dplyr::rename(upper = `0.975quant`,lower = `0.025quant`) %>% filter(Vars != '(Intercept)') %>%
#   dplyr::mutate(Where.Plot = 1:nrow(.),Mod = 'Wetland')
# 
# temp.all = join_all(list(temp,temp1,temp2,temp3),type='full')
# library(ggthemes)


net.change.df %>% dplyr::select(FromYear,FIPS,contains('Approved')) %>% dplyr::select(-ever.approved) %>% gather(Cover.Type,Approval.Year,-FIPS,-FromYear) %>%
  group_by(FromYear,Cover.Type) %>% dplyr::summarise(Active = sum(Approval.Year<=FromYear),No = n() - Active)




