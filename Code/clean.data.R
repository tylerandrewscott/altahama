
rm(list=ls())

treatment.year.lag = 1
set.treatment.lag = treatment.year.lag * 365

benth.data = read.csv('Input/epa_coastal_benthic_data.csv')
trawl.data = read.csv('Input/epa_coastal_trawl_data.csv')
wq.data = read.csv('Input/epa_coastal_wq_data.csv',skip=1)
sed.data = read.csv('Input/epa_coastal_sedtoxicity_data.csv')

grant.data = read.csv('Input/epa_nps_management_program_grants.csv')


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
library(ggplot2);library(plyr);library(dplyr)


wq.data$Station.Abbrev = gsub('[[:digit:]]|[[:punct:]]','',wq.data$Station.Name)
wq.data = wq.data %>% filter(wq.data$Station.Abbrev %in% state.abb)
wq.data$State.Name = cbind(state.abb,state.name)[,2][match(wq.data$Station.Abbrev,cbind(state.abb,state.name)[,1])]

library(RCurl)
library(mosaic)
library(lubridate)


cnp.history = fetchGoogle("https://docs.google.com/spreadsheets/d/1dbSJRtuSah56zBwjk-YySYwJ09ryURrofbJ6Nne5rv0/pub?output=csv")



wq.data$sample.date = parse_date_time(wq.data$Sampling.Collection.Date,'dmy')

cnp.history = fetchGoogle("https://docs.google.com/spreadsheets/d/1dbSJRtuSah56zBwjk-YySYwJ09ryURrofbJ6Nne5rv0/pub?output=csv")

wq.data <- wq.data %>% filter(wq.data$State.Name %in% cnp.history$State)

wq.data$Status = 'Unapproved'
wq.data$Conditional = 0
wq.data$Full = 0
wq.data$Conditional[wq.data$sample.date - set.treatment.lag > mdy(cnp.history$Conditional[match(wq.data$State.Name,cnp.history$State)])] = 1
wq.data$Full[wq.data$sample.date - set.treatment.lag > mdy(cnp.history$Full[match(wq.data$State.Name,cnp.history$State)])] = 1

wq.data$Year = year(wq.data$sample.date)
wq.data$Month = month(wq.data$sample.date)
wq.data$Status <- relevel(as.factor(wq.data$Status),ref='Unapproved')
wq.data$Column <- NA
wq.data$Column[grep('Surface',wq.data$Water.Column.Sampled)] <- 'Surface'
wq.data$Column[grep('Bottom',wq.data$Water.Column.Sampled)] <- 'Bottom'
wq.data$Column[grep('Mid',wq.data$Water.Column.Sampled)] <- 'Mid'
wq.data$Column[grep('Varies',wq.data$Water.Column.Sampled)] <- 'Varies'

#Create phosphorus metric
wq.data = wq.data %>% group_by(Water.Measurement.Name) %>% mutate(Value = scale(Value)) %>% 
  mutate(Water.Meas.Master = as.character(Water.Measurement.Name)) %>% ungroup()

wq.data$Water.Meas.Master[grep('Phos|phos',wq.data$Water.Meas.Master)] = 'Phos'
wq.data$Water.Meas.Master[grep('Nitr|nitr',wq.data$Water.Meas.Master)] = 'Nitr'
wq.data$Water.Meas.Master[grep('Dissolved oxygen',wq.data$Water.Meas.Master)] = 'DO'
wq.data$Water.Meas.Master[grep('Ammonium NH4',wq.data$Water.Meas.Master)] = 'NH4'
wq.data$Water.Meas.Master[grep('Total suspended solids',wq.data$Water.Meas.Master)] = 'TSS'


wq.data <- wq.data %>% filter(!duplicated(paste(Sampling.Collection.Date,Station.Name,Water.Column.Sampled,Water.Meas.Master)))

metric.vector = c('DO','NH4','Phos','Nitr','TSS')

wq.data <- filter(wq.data,!is.na(Water.Column.Sampled))
wq.data$Latitude.Decimal.Degrees <- wq.data$Latitude.Decimal.Degrees - mean(wq.data$Latitude.Decimal.Degrees)
wq.data$Longitude.Decimal.Degrees <- wq.data$Longitude.Decimal.Degrees - mean(wq.data$Longitude.Decimal.Degrees)


month.absolute.reference <- data.frame(
  Year = rep(min(wq.data$Year):max(wq.data$Year),each=12),
  Month = rep(1:12,length(min(wq.data$Year):max(wq.data$Year))),
  absolute.month = 1:length(rep(1:12,length(min(wq.data$Year):max(wq.data$Year)))))

wq.data <- join(wq.data,month.absolute.reference)



mod.list = NULL

for (i in 1:length(metric.vector))
{
  sub.metric = 
    filter(wq.data,wq.data$Water.Meas.Master==metric.vector[i],!is.na(Value)) 
  #sub.metric$log.metric <- log(sub.metric$Value+0.01) 
  #sub.metric$log.metric <- sub.metric$log.metric - mean(sub.metric$log.metric)
  require(INLA)
  #Model 0: No spatial effect
  form0 <-  y ~ 0 + b0 +  Latitude.Decimal.Degrees + Longitude.Decimal.Degrees + 
   Conditional*Full + f(Column,model='iid') + f(State.Name,model='iid') + f(Month,model='iid')+
    f(Year,model='iid') + f(absolute.month,model='rw2')

  m <- inla(form0, family='gaussian', 
               data=data.frame(y=sub.metric$Value, sub.metric,b0=1), 
               control.predictor=list(compute=TRUE),
               control.fixed = list(expand.factor.strategy='model.matrix'),
               #     control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
               control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE))
  mod.list[[i]] <- m
}

table(wq.data$Month)
wq.data$absolute.month

summary(mod.list[[6]])
table(sub.metric$Conditional)
table(sub.metric$State.Name)


dic.values3 = NULL
for (i in 1:length(mod.list))
{
  dic.values3[i] = mod.list[[i]]$dic$dic
}


table(sub.metric$Full,sub.metric$State.Name)
summary(mod.list[[1]])


table(wq.data$State.Name,wq.data$Water.Meas.Master)


dic.values2 - dic.values3

dic.values2
dic.values3

mod.list[[1]]$dic$dic

library(texreg)


round(mod.list[[7]]$summary.fixed,2)

metric.vector[7]
summary(mod.list[[7]])$fixed[4:5,]



library(INLA)

inla.setOption(num.threads=16) 









temp = do.data[do.data$Station.Abbrev == 'FL',]

ggplot(temp,aes(x=Longitude.Decimal.Degrees,y=Latitude.Decimal.Degrees)) + geom_point() + 
  facet_wrap(~Year)


table(do.data$Station)
table(as.character(do.data$Station.Name))

summary(m$gam)
BIC(m[[1]])

head(do.data)

table(as.character(do.data$Data.Group))
do.data$Station.Abbrev
do.data$absolute.month

do.data$Station
BIC(m[[1]])
table(do.data$Column)
sum(is.na(do.data$Column))
do.data$Station.Abbrev
BIC(m[[1]])
> BIC(m[[1]])
[1] 19806.49


BIC(m0[[1]])


library(lme4)
test = lmer(log.metric~Latitude.Decimal.Degress + Longitude.Decimal.Degrees + 
              Status + Column + (1|State.Name) + (1|Year),data=do.data)


test = do.data %>% group_by(Station.Abbrev,absolute.month) %>% summarise(avg.met = mean(log.metric))

ggplot(test, aes(x=absolute.month,y=avg.met,group=Station.Abbrev,id=Station.Abbrev)) + geom_path() 

ggplot(test,aes(x=absolute.month,y=avg.met)) + geom_path(aes(color=Station.Abbrev))





geom_point()plot(x,y, yaxp=c(0,100,10))
?colmin(do.data$sample.date)
max(do.data$sample.date)


do.data$log.metric
ggplot(do.data[do.data$State.Name=='Washington',],aes(x=as.factor(year(sample.date)),y=Value)) + geom_boxplot()




%in% cnp.history$State

cnp.history$State
mdy(cnp.history$Conditional)


wq.data$Sampling.Collection.Date[wq.data$Sampling.Year==2000]
head(wq.data$Sampling.Collection.Date)

sum(is.na(year(parse_date_time(wq.data$Sampling.Collection.Date,"dmy"))))

