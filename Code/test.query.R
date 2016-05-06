library(RCurl)


query.stations = 'http://www.waterqualitydata.us/Station/search?siteType=Estuary&siteType=Ocean&project=EMAP-NCA&project=NCCA_NCA2000&project=NCCA_NCA199706&project=EMAP&project=NCCA_2015&project=NCCA&project=NCCA_WEMAP1999&project=NCCA_WEMAP2004&project=NCCA_NEP_SJH2002&project=NCCA_WEMAP199700&project=NCCA_WEMAP2002AK&project=NCCA_WEMAP2002HI&project=NCCA_WEMAP2003MB&project=NCCA_WEMAP2003SM&project=NCCA_WEMAP2004AK&project=NCCA_WEMAP2004GU&project=NCCA_WEMAP200506&project=NCCA_WEMAP2006HI&project=NCCA_NEP_SE200002&project=NCCA_NEP_GULF200002&mimeType=csv&zip=no&sorted=no'
query.results = 'http://www.waterqualitydata.us/Result/search?siteType=Estuary&siteType=Ocean&project=EMAP-NCA&project=NCCA_NCA2000&project=NCCA_NCA199706&project=EMAP&project=NCCA_2015&project=NCCA&project=NCCA_WEMAP1999&project=NCCA_WEMAP2004&project=NCCA_NEP_SJH2002&project=NCCA_WEMAP199700&project=NCCA_WEMAP2002AK&project=NCCA_WEMAP2002HI&project=NCCA_WEMAP2003MB&project=NCCA_WEMAP2003SM&project=NCCA_WEMAP2004AK&project=NCCA_WEMAP2004GU&project=NCCA_WEMAP200506&project=NCCA_WEMAP2006HI&project=NCCA_NEP_SE200002&project=NCCA_NEP_GULF200002&mimeType=csv&zip=no&sorted=no'


stations = read.csv(query.stations)
results = read.csv(query.results)

sort(unique(results$CharacteristicName))
head(results)
library(data.table)
library(ggplot2) 

ggplot(stations,aes(x=LongitudeMeasure,y=LatitudeMeasure)) + geom_point()
