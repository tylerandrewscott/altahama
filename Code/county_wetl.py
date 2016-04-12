# Name: MakeZonalStatistics_wetlland.py
# Description: Summarizes values of wetlland raster (wetl = 1, other = 0) within the zones of county, CFIPS, Counties
#               reports the results to a table.
# Requirements: Spatial Analyst Extension
# Author: Tyler Scott

# Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")

# Set environment settings
env.workspace = "H:/altahama"

# Set local variables
inZoneData = "Shapefiles/coastalcounties.shp"
zoneField = "GEOID"
inValueRaster = "Shapefiles/tf_wetl_1992"
outTable = "Input/wetl_county_1992.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")

# Set local variables
inZoneData = "Shapefiles/coastalcounties.shp"
zoneField = "GEOID"
inValueRaster = "Shapefiles/tf_wetl_2001"
outTable = "Input/wetl_county_2001.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
      # Set local variables
inZoneData = "Shapefiles/coastalcounties.shp"
zoneField = "GEOID"
inValueRaster = "Shapefiles/tf_wetl_2006"
outTable = "Input/wetl_county_2006.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
inZoneData = "Shapefiles/coastalcounties.shp"
zoneField = "GEOID"
inValueRaster = "Shapefiles/tf_wetl_2011"
outTable = "Input/wetl_county_2011.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 

                                                                  
arcpy.CheckInExtension("Spatial")