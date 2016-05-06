
library(plyr)
library(dplyr)
site.2010 = read.csv('Input/ncca/ncca2010/assessed_ncca2010_siteinfo.csv')
unsite.2010 = read.csv('Input/ncca/ncca2010/not_assessed_ncca2010_siteinfo.csv')
benthic.2010 = read.csv('Input/ncca/ncca2010/ncca2010_benthic_indicator_status.csv')
benthic.2010 = left_join(benthic.2010,site.2010)
benthic.2010 = benthic.2010 %>% mutate(DIV.INDEX = NCA_BI)
library(lubridate)
benthic.2010$Sampling.Collection.Date = dmy(benthic.2010$DATE_COL)
benthic.2010$Sampling.Year = year(benthic.2010$Sampling.Collection.Date)
benthic.2010$Month = month(benthic.2010$Sampling.Collection.Date)
colnames(benthic.2010) = tolower(colnames(benthic.2010))
benthic.2010$benthic_index = ifelse(!is.na(benthic.2010$oti),benthic.2010$oti,benthic.2010$nca_bi)


site.2000 = read.csv('Input/ncca/ncca2000_2005/nca_siteinformationdata_narschallenge.csv')
benthic.2000 = read.csv('Input/ncca/ncca2000_2005/epa_coastal_benthic_data.csv',skip=1)
site.2000 = site.2000 %>% filter(!is.na(LON_DD))
benthic.2000 = benthic.2000 %>% rename(SITE_ID = Station.Name) %>% mutate(Sampling.Collection.Date = dmy(Sampling.Collection.Date))
benthic.2000 = join(benthic.2000,site.2000,type='full')
benthic.2000 = benthic.2000 %>% mutate(EPA_REG = as.numeric(gsub('EPA_REG_','',EPA_REG)),Sampling.Year = year(Sampling.Collection.Date),Month = month(Sampling.Collection.Date))
benthic.2000 = benthic.2000 %>% rename(STATE = PSTL_CODE,DIV.INDEX = H..Diversity.Index)
colnames(benthic.2000) = tolower(colnames(benthic.2000))
benthic.all = join(benthic.2000,benthic.2010,type='full')


table(is.na(benthic.2010$benthic_index))

table(!is.na(benthic.2010$oti),!is.na(benthic.2010$nca_bi))


cbind(benthic.2010$oti,benthic.2010$nca_bi)
head(benthic.all)
table(benthic.all$sampling.year)

benthic.2010$nca_bi_status
benthic.2010$ncca_bi_status
benthic.2010$nca_bi
benthic.all$div.index
benthic.all$in

sort(names(benthic.2000))[sort(names(benthic.2000)) %in% sort(names(benthic.2010)) == F]
sort(names(benthic.2010))[sort(names(benthic.2010)) %in% sort(names(benthic.2000)) == F]
intersect(sort(names(benthic.2000)),sort(names(benthic.2010)))



benthic.2000$epa_reg

cond.df = benthic.all

library(lme4)

sort(names(benthic.2010))
sort(names(benthic.2000))


summary(cond.df$DIV.INDEX)



cond.df$Latitude.Decimal.Degrees
# 
# 
# cond.df$LAT = site.2010$ALAT_DD[match(benthic.2010$SITE_ID,site.2010$SITE_ID)]
# cond.df$LON = site.2010$ALON_DD[match(benthic.2010$SITE_ID,site.2010$SITE_ID)]
# cond.df$STATE = site.2010$STATE[match(benthic.2010$SITE_ID,site.2010$SITE_ID)]
# cond.df$DATE = site.2010$DATE_COL[match(benthic.2010$SITE_ID,site.2010$SITE_ID)]
# cond.df$DEPTH = site.2010$STATION_DEPTH[match(benthic.2010$SITE_ID,site.2010$SITE_ID)]
# cond.df$EPA_REG = site.2010$EPA_REG[match(benthic.2010$SITE_ID,site.2010$SITE_ID)]

require(plyr);require(dplyr)
covars = cond.df %>% filter(!is.na(NCA_BI))
require(INLA)
# STEP 1: MESH CONSTRUCTION
coords <- cbind(covars$LON, covars$LAT)
bound1 <- inla.nonconvex.hull(coords,convex=0.5, concave=-0.01,resolution=c(121,54))
mesh.a <- inla.mesh.2d(boundary=bound1, cutoff=.01, max.edge=c(1,20))
#plot(mesh.a)
#points(coords,col='red')

# STEP 2: CREATE SPDE OBJECT
spde.a <- inla.spde2.matern(mesh.a) 

# STEP 3: PREPARE DATA {STACK}
### m1: 1 spatial effect constant over time
A.1 <- inla.spde.make.A(mesh.a, loc=coords)
ind.1 <- inla.spde.make.index('s', mesh.a$n)
stk.1 <- inla.stack(data=list(y=covars$NCA_BI), A=list(A.1,1),
                    effects=list(ind.1, list(data.frame(b0=1,covars))))

# STEP 4: Formulae
form1 <-  y ~ 0 + b0 + I(LAT) + I(LON)+   + f(inla.group(DEPTH), model='rw1') + f(STATE,model='iid') + f(EPA_REG,model='iid')  +f(s, model=spde.a)

r <- inla(form1, family='gaussian', data=inla.stack.data(stk.1),
          control.predictor=list(A=inla.stack.A(stk.1), compute=TRUE),
          control.inla=list(strategy='laplace'), 
          control.compute=list(dic=TRUE, cpo=TRUE))


##################################
### MAP of random field mean and sd (Figure 6)
##################################
data = covars %>% rename(X = LON,Y=LAT)
proj<-inla.mesh.projector(mesh.a,xlim=range(data$X), ylim=range(data$Y))
dat = as.data.frame(proj$lattice$loc)
ndat<-data.frame(sapply(dat, rep.int, times=4))
# dim(ndat)#100 x 100 replicated 4 times for each year
ndat$year<-rep(1:4,each=10000)
table(ind$s.repl)#476 spde nodes every year
length(rl.nb2.gr$summary.random$s$mean)#1904/4

xmeans <- list()
xsd <- list()
for (j in 1:4) {
  xmeans[[j]] <- inla.mesh.project(proj,rl.nb2.gr$summary.random$s$mean[ind$s.repl==j])
  xsd[[j]] <- inla.mesh.project(proj,rl.nb2.gr$summary.random$s$sd[ind$s.repl==j])
}

ndat <- data.frame(ndat, unlist(xmeans),unlist(xsd)) 
head(ndat)
ndat$year <- factor(ndat$year+2007)
colnames(ndat)[4:5]<-c("mean","sd")



### range (km) and 95% credible intervals
c(mean=inla.emarginal(function(x) x, rf.gr$marginals.range[[1]]), 
  q=inla.hpdmarginal(0.95, rf.gr$marginals.range[[1]]))[c(1,2,3)]

#Summary table of the posterior distributions of all parameters
tables <- t(sapply(c(rl.nb2.gr$marginals.fix, 
                     rl.nb2.gr$marginals.hy[1:2], 
                     rf.gr$marginals.range[1]), function(m) 
                       c(mean=inla.emarginal(function(x) x, m), 
                         lim=inla.hpdmarginal(0.95, m)))) 







head(site.2000)

plot(site.2000$LAT_DD~site.2000$LON_DD)

plot(site.2010$ALON_DD,site.2010$ALAT_DD)
points(unsite.2010$ALON_DD,unsite.2010$ALAT_DD,col='red')
points(site.2000$LAT_DD~site.2000$LON_DD,col='blue')

table(site.2000$SITE_ID %in% site.2010$ID)
head(site.2000)
table(site.2000$ESTUARY)

as.character(site.2010$WTBDY_NM)
head(site.2000)
head(site.2010)  

head(site.2010,15)
