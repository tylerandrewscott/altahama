#rm(list=ls())
library(plyr)
library(dplyr)

library(rgdal)
library(sp)
library(maptools)
library(rgeos)


counties = readOGR('Shapefiles','coastalcounties')
wholecounties = unionSpatialPolygons(counties,ifelse(as.numeric(as.character(counties$INTPTLON)) < (-100), 1,2),threshold = 100)

stations = read.delim('LargeInput/station.csv',sep=',')

stations = stations %>% filter(!is.na(LatitudeMeasure), !is.na(LongitudeMeasure),LongitudeMeasure!='NA',LatitudeMeasure!='NA')

stations$MonitoringLocationTypeName = as.character(stations$MonitoringLocationTypeName)
stations$MonitoringLocationTypeName[grep('Ocean',stations$MonitoringLocationTypeName)] = 'Ocean'
stations$MonitoringLocationTypeName[grep('Estuar',stations$MonitoringLocationTypeName)] = 'Estuary'
stations$MonitoringLocationTypeName[grep('Wetland',stations$MonitoringLocationTypeName)] = 'Wetland'
stations$MonitoringLocationTypeName[grep('Lake',stations$MonitoringLocationTypeName)] = 'Lake'
stations$MonitoringLocationTypeName[grep('River|Stream|stream|river',stations$MonitoringLocationTypeName)] = 'Stream/River'
stations$MonitoringLocationTypeName[grep('Pond',stations$MonitoringLocationTypeName)] = 'Pond'
stations$MonitoringLocationTypeName[grep('Canal',stations$MonitoringLocationTypeName)] = 'Canal'
stations = stations[grep('Floodwater|Site-Land',stations$MonitoringLocationTypeName,invert=T),]

stations = stations[stations$StateCode %in% 
                      c(2, 3, 7, 64, 14, 84, 86, 67, 89, 
                        68, 71, 70, 72, 74, 78, 79) == FALSE,]


stations = stations[grep('Guam|Puerto Rico|Samoa|Marshall Islands',stations$MonitoringLocationName,invert=T),]
stations = stations[grep('Guam|Samoa|Puerto Rico|Marshall Islands|Northern Mariana Islands',stations$OrganizationFormalName,invert=T),]
stations = stations[stations$HUCEightDigitCode %in% c(22010000,22020000,22030000,21020002,21020001) == FALSE,]
stations = stations[stations$HorizontalCoordinateReferenceSystemDatumName != 'GUAM',]
stations$LongitudeMeasure = -(abs(stations$LongitudeMeasure))
stations = stations[stations$LongitudeMeasure<(-5),]
switch.ll = stations[stations$LatitudeMeasure<0,]
stations = stations[stations$LatitudeMeasure>0,]
switch.ll = switch.ll %>% mutate(lat2 = abs(LongitudeMeasure),long2 = -abs(LatitudeMeasure)) %>%
  mutate(LongitudeMeasure = long2, LatitudeMeasure = lat2)
switch.ll$LongitudeMeasure[switch.ll$lat2 > 50] =  -abs(switch.ll$lat2[switch.ll$lat2 > 50])
switch.ll$LatitudeMeasure[switch.ll$lat2 > 50] =  abs(switch.ll$long2[switch.ll$lat2 > 50])
stations = switch.ll %>%dplyr::select(-long2,-lat2) %>% join(stations,.,type='full')

stations = stations[(abs(stations$LongitudeMeasure)<52 & abs(stations$LatitudeMeasure)<52) == FALSE ,]

switch.ll = stations[stations$LongitudeMeasure> -40,]
stations = stations[stations$LongitudeMeasure< -40,]
switch.ll = switch.ll %>% mutate(lat2 = abs(LongitudeMeasure),long2 = -abs(LatitudeMeasure)) %>%
  mutate(LongitudeMeasure = long2, LatitudeMeasure = lat2) %>%
 dplyr::select(-long2,-lat2) %>% join(stations,.,type='full')

stations = stations[stations$LongitudeMeasure< -60,]

stations = stations[(stations$LatitudeMeasure> 60 & stations$LongitudeMeasure>-120) == FALSE,]
stations = stations[(stations$LatitudeMeasure< 30 & stations$LongitudeMeasure<(-119) & stations$LongitudeMeasure>(-122))==FALSE ,]

stations = stations[stations$StateCode %in% c(32,4, 5, 8, 16, 19, 20, 21, 29, 30, 31, 32, 35, 38, 40, 
                                   46, 47, 49, 54, 56) == FALSE,]

station.spdf = SpatialPointsDataFrame(coords= cbind(stations$LongitudeMeasure,stations$LatitudeMeasure),
                                      data = stations, proj4string = CRS(proj4string(counties)))

is.over = over(station.spdf,wholecounties)

is.over[station.spdf@data$MonitoringLocationTypeName %in% c('Ocean','Estuary') & 
          station.spdf@data$LongitudeMeasure<(-10)] = 1 

is.over[station.spdf@data$MonitoringLocationTypeName %in% c('Ocean','Estuary') & 
          station.spdf@data$LongitudeMeasure>(-100)] = 2

station.spdf = station.spdf[!is.na(is.over),] 


# wq.atl = read.delim('LargeInput/wq_atl.tsv',sep = '\t')
# wq.atl = wq.atl %>%dplyr::select(OrganizationIdentifier,OrganizationFormalName,
#                            ActivityIdentifier,ActivityMediaName,
#                            ActivityStartDate,ActivityDepthHeightMeasure.MeasureValue,
#                            ActivityStartTime.Time,
#                            ActivityDepthHeightMeasure.MeasureUnitCode,
#                            MonitoringLocationIdentifier,CharacteristicName,ResultMeasureValue,
#                            ResultMeasure.MeasureUnitCode,USGSPCode)
# wq.atl = wq.atl[wq.atl$MonitoringLocationIdentifier %in% station.spdf@data$MonitoringLocationIdentifier,]
# write.csv(wq.atl,'LargeInput/wq.atl.scrubbed.csv')
# 
# 
# wq.ne = read.delim('LargeInput/wq_ne.tsv',sep = '\t')
# wq.ne = wq.ne %>%dplyr::select(OrganizationIdentifier,OrganizationFormalName,
#                            ActivityIdentifier,ActivityMediaName,
#                            ActivityStartDate,ActivityDepthHeightMeasure.MeasureValue,
#                            ActivityStartTime.Time,
#                            ActivityDepthHeightMeasure.MeasureUnitCode,
#                            MonitoringLocationIdentifier,CharacteristicName,ResultMeasureValue,
#                            ResultMeasure.MeasureUnitCode,USGSPCode)
# wq.ne = wq.ne[wq.ne$MonitoringLocationIdentifier %in% station.spdf@data$MonitoringLocationIdentifier,]
# write.csv(wq.ne,'LargeInput/wq.ne.scrubbed.csv')
# 
# wq.gulf = read.delim('LargeInput/wq_gulf.tsv',sep = '\t')
# wq.gulf = wq.gulf %>%dplyr::select(OrganizationIdentifier,OrganizationFormalName,
#                            ActivityIdentifier,ActivityMediaName,
#                            ActivityStartDate,ActivityDepthHeightMeasure.MeasureValue,
#                            ActivityStartTime.Time,
#                            ActivityDepthHeightMeasure.MeasureUnitCode,
#                            MonitoringLocationIdentifier,CharacteristicName,ResultMeasureValue,
#                            ResultMeasure.MeasureUnitCode,USGSPCode)
# wq.gulf = wq.gulf[wq.gulf$MonitoringLocationIdentifier %in% station.spdf@data$MonitoringLocationIdentifier,]
# write.csv(wq.gulf,'LargeInput/wq.gulf.scrubbed.csv')
# 
# 
# wq.mw = read.delim('LargeInput/wq_mw.tsv',sep = '\t')
# wq.mw = wq.mw %>%dplyr::select(OrganizationIdentifier,OrganizationFormalName,
#                            ActivityIdentifier,ActivityMediaName,
#                            ActivityStartDate,ActivityDepthHeightMeasure.MeasureValue,
#                            ActivityStartTime.Time,
#                            ActivityDepthHeightMeasure.MeasureUnitCode,
#                            MonitoringLocationIdentifier,CharacteristicName,ResultMeasureValue,
#                            ResultMeasure.MeasureUnitCode,USGSPCode)
# wq.mw = wq.mw[wq.mw$MonitoringLocationIdentifier %in% station.spdf@data$MonitoringLocationIdentifier,]
# write.csv(wq.mw,'LargeInput/wq.mw.scrubbed.csv')
# 
# wq.west = read.delim('LargeInput/wq_west.tsv',sep = '\t')
# wq.west = wq.west %>%dplyr::select(OrganizationIdentifier,OrganizationFormalName,
#                            ActivityIdentifier,ActivityMediaName,
#                            ActivityStartDate,ActivityDepthHeightMeasure.MeasureValue,
#                            ActivityStartTime.Time,
#                            ActivityDepthHeightMeasure.MeasureUnitCode,
#                            MonitoringLocationIdentifier,CharacteristicName,ResultMeasureValue,
#                            ResultMeasure.MeasureUnitCode,USGSPCode)
# wq.west = wq.west[wq.west$MonitoringLocationIdentifier %in% station.spdf@data$MonitoringLocationIdentifier,]
# write.csv(wq.west,'LargeInput/wq.west.scrubbed.csv')
# 


wq.west = read.csv('LargeInput/wq.west.scrubbed.csv')
wq.atl = read.csv('LargeInput/wq.atl.scrubbed.csv')
wq.ne = read.csv('LargeInput/wq.ne.scrubbed.csv')
wq.gulf = read.csv('LargeInput/wq.gulf.scrubbed.csv')
wq.mw = read.csv('LargeInput/wq.mw.scrubbed.csv')










