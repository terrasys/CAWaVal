print("1 | Zonal statistics of reference units for raster data and accuracy assessment")
#-------------------------------------------------------------------------------
fClassCompareR <- function(
  W.DIR,
  IN.DIR,
  OUT.DIR,
  CLASS.SHP,
  CLASS.RASTER,
  RASTER.FORMAT){
#-----------------------------------------------------------------------------------------------------
print("2 | Import reference units")
#-----------------------------------------------------------------------------------------------------
c <- shapefile(file.path(paste(W.DIR,IN.DIR,CLASS.SHP,".shp",sep="")))
#-----------------------------------------------------------------------------------------------------
print("Import raster")
#-----------------------------------------------------------------------------------------------------
r <- raster(paste(W.DIR,IN.DIR,CLASS.RASTER,RASTER.FORMAT,sep=""))
#-----------------------------------------------------------------------------------------------------
print("Reproject classification according to raster's projection")
#-----------------------------------------------------------------------------------------------------
#reproject ru accordung to m
c <- spTransform(c, r@crs)
#export shape file
shapefile(c, paste(W.DIR,OUT.DIR,CLASS.SHP,"_COMPARE",sep=""), overwrite=TRUE)
#-----------------------------------------------------------------------------------------------------
print("3 | Transform imagery in SAGA format")
#-----------------------------------------------------------------------------------------------------
writeRaster(r,file.path(W.DIR,IN.DIR,CLASS.RASTER),format="SAGA",overwrite=TRUE)
#-----------------------------------------------------------------------------------------------------
print("4 | Zonal statistic")
#-----------------------------------------------------------------------------------------------------
  rsaga.geoprocessor(
    lib="shapes_grid",
    module=2,
    param=list(GRIDS=paste(W.DIR,IN.DIR,CLASS.RASTER,".sgrd",sep=""),
               POLYGONS=paste(W.DIR,OUT.DIR,CLASS.SHP,"_COMPARE",c(".shp"),sep=""),
               COUNT=0,
               MEAN=0,
               MIN=0,
               MAX=1,
               RANGE=0,
               SUM=0,
               VAR=0,
               STDDEV=0,
               QUANTILE=0,
               NAMING=0),
    env=myenv)


rsaga.geoprocessor(
  lib="shapes_grid",
  module=2,
  param=list(GRIDS=paste(W.DIR,IN.DIR,CLASS.RASTER,".sgrd",sep=""),
             POLYGONS=paste(W.DIR,OUT.DIR,CLASS.SHP,"_COMPARE",c(".shp"),sep=""),
             COUNT=0,
             MEAN=0,
             MIN=1,
             MAX=0,
             RANGE=0,
             SUM=0,
             VAR=0,
             STDDEV=0,
             QUANTILE=0,
             NAMING=0),
  env=myenv)


rsaga.geoprocessor(
  lib="shapes_grid",
  module=2,
  param=list(GRIDS=paste(W.DIR,IN.DIR,CLASS.RASTER,".sgrd",sep=""),
             POLYGONS=paste(W.DIR,OUT.DIR,CLASS.SHP,"_COMPARE",c(".shp"),sep=""),
             COUNT=0,
             MEAN=0,
             MIN=0,
             MAX=1,
             RANGE=0,
             SUM=0,
             VAR=0,
             STDDEV=0,
             QUANTILE=0,
             NAMING=0),
  env=myenv)

rsaga.geoprocessor(
  lib="shapes_grid",
  module=2,
  param=list(GRIDS=paste(W.DIR,IN.DIR,CLASS.RASTER,".sgrd",sep=""),
             POLYGONS=paste(W.DIR,OUT.DIR,CLASS.SHP,"_COMPARE",c(".shp"),sep=""),
             COUNT=0,
             MEAN=0,
             MIN=0,
             MAX=0,
             RANGE=1,
             SUM=0,
             VAR=0,
             STDDEV=0,
             QUANTILE=0,
             NAMING=0),
  env=myenv)
#-----------------------------------------------------------------------------------------------------
print("5 | Accuracy assessment")
#-----------------------------------------------------------------------------------------------------
o <- st_read(paste(W.DIR,OUT.DIR,CLASS.SHP,"_COMPARE",c(".shp"),sep=""))
#k <- read.csv2(KEY)
#o <- merge(o,k,by = "CLASS")
o <- o[which(o$G01_RANGE==0),]
#all samples
set.seed(777)
acc.m <- fClassAcc(actual=o$G01_MAX, predicted=o$CLASS)
setwd(file.path(W.DIR,OUT.DIR))
write.csv2(acc.m$ConfusionMatrix, 
           file = paste(CLASS.SHP,"_CM.csv"))
write.csv2(acc.m$AccMetrics, 
           file = paste(CLASS.SHP,"_AM.csv"))

}
