#make conditional (0/1) wetlands rasters

# Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *

# Set environment settings
env.workspace = "H:/duckabush"
# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

###2001 Wetlands######
# Set local variables
inRaster = Raster("nlcd_2001_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
whereClause = "Value >= 89"
# Execute Con
outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
# Save the outputs 
outCon.save("H:/duckabush/tf_wetl_2001")

####2006 Wetlands######
# Set local variables
inRaster = Raster("nlcd_2006_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
whereClause = "Value >= 89"
# Execute Con
outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
# Save the outputs 
outCon.save("H:/duckabush/tf_wetl_2006")


#####2011 Wetlands######
# Set local variables
inRaster = Raster("nlcd_2011_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
inFalseConstant = 0
whereClause = "Value >= 89"
# Execute Con
outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
# Save the outputs 
outCon.save("H:/duckabush/tf_wetl_2011")

# Check in the ArcGIS Spatial Analyst extension license
arcpy.CheckInExtension("Spatial")

