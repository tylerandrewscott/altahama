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
  mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1))) 


total.file.names = paste0('Input/',grep('LandCover[[:digit:]]',list.files('Input/'),value=TRUE))
data.list = lapply(as.list(total.file.names),read.csv)
total.df = join_all(data.list,type='full')

area.df = total.df %>% group_by(Year,FIPS,StateAbbrev,CountyName) %>% summarise(Area = sum(SquareMiles)) %>% dplyr::select(-Year)

focal.change.df <- join(focal.change.df,area.df)

county.pop.2011 = read.delim('Input/county.acs.2011.txt',skip=10)
head(county.pop.2011)

county.pop.2006 = read.csv('Input/county.acs.2006.csv',skip=1)
county.pop.2000 = read.csv('Input/county.census.2000.csv',skip=1)
county.pop.1990 = read.delim('Input/county.census.1990.txt',skip=1)

head(county.pop.1990)


county.pop.1990 = read.delim('Input/county.census.1990.2000.txt',sep=',',header=T)
names <- colnames(county.pop.1990)[-1]
county.pop.1990 <- county.pop.1990[,-ncol(county.pop.1990)]
colnames(county.pop.1990) <- names


county.pop.1990$Name

sum(!is.na(county.pop.1990$Name))


head(county.pop.1990)
county.pop.1990$Name
sum(!is.na(county.pop.1990$Name))
class(county.pop.1990)
head(county.pop.1990,20)
dim(county.pop.1990)
colnames(county.pop.1990)
head(county.pop.1990,10)

head(county.pop.2000)

head(test)




library(INLA)



head(focal.change.df)

area.df[area.df$CountyName=='Baldwin',]
head(area.df)
rm(data.list)


head(total.df)

test <- change.df %>%  filter(fromClass == toClass) %>% group_by(CountyName,ToYear,FromYear) %>% summarise(Total = sum(SquareMiles))
  
  
head(test)  
  
  
filter(toClass %in% grep('Developed|Cultivated|Pasture',uq.classes,value=TRUE)) %>% 
  filter(fromClass %in% grep('Grass|Forest|Estuar|Wetland|Water|Palustrine',uq.classes,value=TRUE)) %>% 
  filter(fromClass != toClass) %>% 
  filter(ToYear-FromYear<=5) %>%
  group_by(CountyName,FIPS,StateAbbrev,FromYear,ToYear) %>% summarise(TotalChange = sum(SquareMiles)) %>%
  mutate(TotalChange.ihs = log(TotalChange + sqrt(TotalChange^TotalChange + 1)))





start = list(mu = 0, sigma = 2, lambda = 0, k = 1)
X.var <- focal.change.df$TotalChange
X.f = X ~ X.var
test = ihs::ihs.mle(X.f,start=start)
warnings()
print(test)
summary(test)

coef(test)





focal.change.df$`sum(SquareMiles)`==0
hist(focal.change.df$`sum(SquareMiles)`)

max(focal.change.df$`sum(SquareMiles)`)
focal.change.df[focal.change.df$CountyName=='Pierce',]
hist(focal.change.df$`sum(SquareMiles)`)
head(focal.change.df)

head(focal.change.df)


head(change.df)



head(change.df)
