#-----------------------------------------------------------------------------------------------------
print("Working directory, data and settings")
#-----------------------------------------------------------------------------------------------------
W.DIR <- "d:/Dropbox/_git/CAWaVal/"
FUNC.DIR <- "_functions/"
#-----------------------------------------------------------------------------------------------------
print("Import functions")
#-----------------------------------------------------------------------------------------------------
source(file.path(W.DIR,FUNC.DIR,"fPackages.R"))
source(file.path(W.DIR,FUNC.DIR,"fClassCompareR.R"))
source(file.path(W.DIR,FUNC.DIR,"fClassAcc.R"))
source(file.path(W.DIR,FUNC.DIR,"fClassCompareP.R"))
#-----------------------------------------------------
print("Comparison of raster- and polygon-based classifications")
#-------------------------------------------------------------------------------
fClassCompareR(W.DIR,
               IN.DIR = "_data/",
               OUT.DIR = "_result/",
               CLASS.SHP = "MODIS_fergana2015_rf_CLASS",
               CLASS.RASTER = "2015_cropClass",
               RASTER.FORMAT = ".tif")
#-----------------------------------------------------
print("Comparison of point- and polygon-based classifications")
#-------------------------------------------------------------------------------
fClassCompareP(W.DIR,
               IN.DIR  = "_data/",
               OUT.DIR = "_result/",
               R.SHP   = "Fergana_2015_points",
               X.SHP   = "MODIS_fergana2015_rf_CLASS")

