
rm(list=ls())
benth.data = read.csv('Input/epa_coastal_benthic_data.csv')
trawl.data = read.csv('Input/epa_coastal_trawl_data.csv')
wq.data = read.csv('Input/epa_coastal_wq_data.csv',skip=1)
sed.data = read.csv('Input/epa_coastal_sedtoxicity_data.csv')

grant.data = read.csv('../Input/epa_nps_management_program_grants.csv')


library(ggplot2);library(plyr);library(dplyr)
ggplot(wq.data,aes(y=Latitude.Decimal.Degrees,x=Longitude.Decimal.Degrees)) + geom_point() + facet_wrap(~Sampling.Year)
plot(sed.data$Longitude.Decimal.Degrees,sed.data$Latitude.Decimal.Degrees)

wq.data$Station.Abbrev = gsub('[[:digit:]]|[[:punct:]]','',wq.data$Station.Name)

wq.data = wq.data %>% filter(wq.data$Station.Abbrev %in% state.abb)
wq.data$State.Name = cbind(state.abb,state.name)[,2][match(wq.data$Station.Abbrev,cbind(state.abb,state.name)[,1])]

library(RCurl)
library(mosaic)
library(lubridate)
wq.data$sample.date = parse_date_time(wq.data$Sampling.Collection.Date,'dmy')

cnp.history = fetchGoogle("https://docs.google.com/spreadsheets/d/1dbSJRtuSah56zBwjk-YySYwJ09ryURrofbJ6Nne5rv0/pub?output=csv")

wq.data <- wq.data %>% filter(wq.data$State.Name %in% cnp.history$State)

wq.data$Status = 'Unapproved'
wq.data$Status[wq.data$sample.date > mdy(cnp.history$Conditional[match(wq.data$State.Name,cnp.history$State)])] = 'Conditional'
wq.data$Status[wq.data$sample.date > mdy(cnp.history$Full[match(wq.data$State.Name,cnp.history$State)])] = 'Full'

wq.data$Year = year(wq.data$sample.date)
wq.data$Month = month(wq.data$sample.date)
wq.data$Status <- relevel(as.factor(wq.data$Status),ref='Unapproved')
wq.data$Column <- NA
wq.data$Column[grep('Surface',wq.data$Water.Column.Sampled)] <- 'Surface'
wq.data$Column[grep('Bottom',wq.data$Water.Column.Sampled)] <- 'Bottom'
wq.data$Column[grep('Mid',wq.data$Water.Column.Sampled)] <- 'Mid'
wq.data$Column[grep('Varies',wq.data$Water.Column.Sampled)] <- 'Varies'

wq.data <- filter(wq.data,!is.na(Water.Column.Sampled))
wq.data$Latitude.Decimal.Degrees <- wq.data$Latitude.Decimal.Degrees - mean(wq.data$Latitude.Decimal.Degrees)
wq.data$Longitude.Decimal.Degrees <- wq.data$Longitude.Decimal.Degrees - mean(wq.data$Longitude.Decimal.Degrees)


month.absolute.reference <- data.frame(
  Year = rep(min(wq.data$Year):max(wq.data$Year),each=12),
  Month = rep(1:12,length(min(wq.data$Year):max(wq.data$Year))),
  absolute.month = 1:length(rep(1:12,length(min(wq.data$Year):max(wq.data$Year)))))

wq.data <- join(wq.data,month.absolute.reference)
do.data = filter(wq.data,wq.data$Water.Measurement.Name=='Dissolved oxygen',!is.na(Value))
do.data$log.metric <- log(do.data$Value+0.01) 
do.data$log.metric <- do.data$log.metric - mean(do.data$log.metric)


library(INLA)
#Model 0: No spatial effect
form0 <-  y ~ 0 + b0 + Column + Latitude.Decimal.Degrees + Longitude.Decimal.Degrees + 
  Status +  f(State.Name,model='iid') + 
  f(Year,model='iid') + f(absolute.month,model='seasonal',season.length=12)


mod0 <- inla(form0, family='gaussian', 
             data=data.frame(y=do.data$log.metric, do.data,b0=1), 
             control.predictor=list(compute=TRUE),
             #     control.inla=list(strategy='laplace'), #note that we are here using laplace, default in R-INLA is the simplified laplace approximation (run faster)
             control.compute=list(dic=TRUE, cpo=TRUE))


summary(mod0)
min(do.data$sample.date)
max(do.data$sample.date)


do.data$log.metric
ggplot(do.data[do.data$State.Name=='Washington',],aes(x=as.factor(year(sample.date)),y=Value)) + geom_boxplot()




%in% cnp.history$State

cnp.history$State
mdy(cnp.history$Conditional)


wq.data$Sampling.Collection.Date[wq.data$Sampling.Year==2000]
head(wq.data$Sampling.Collection.Date)

sum(is.na(year(parse_date_time(wq.data$Sampling.Collection.Date,"dmy"))))

