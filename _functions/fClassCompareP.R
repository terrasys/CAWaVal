fClassCompareP <- function(W.DIR,
                          IN.DIR,
                          OUT.DIR,
                          POINT.SHP,
                          CLASS.SHP){
  print("Geometric overlay of two polgone shape files and test for geometric correspondence")
  #-------------------------------------------------------------------------------
  print("1 | Import shape files")
  #-------------------------------------------------------------------------------
  ##import data
  x <- st_read(paste(W.DIR,IN.DIR,CLASS.SHP,".shp",sep=""))
  r <- st_read(paste(W.DIR,IN.DIR,POINT.SHP,".shp",sep=""))
  x$HECTARES <- st_area(x)/10000
  #reproject x according to r
  x <- st_transform(x, st_crs(r))
  #proportion of classes: reference data
  setwd(file.path(W.DIR,OUT.DIR))
  pdf(paste(POINT.SHP,"_BARPLOT",c(".pdf"),sep=""),
      width=5.5,height=5.5)#Plotting
  n.tab <-   table(r$CLASS_R)
  xx <- barplot(n.tab/sum(n.tab)*100,
                ylab="Proportion [%]",
                sub=paste('Total number =',sum(n.tab)),
                xlab="CLASS",
                las=1,
                ylim=c(0,100),
                main=paste(POINT.SHP))
  text(x = xx, y = 0, label = round(n.tab/sum(n.tab)*100,1), pos = 3, cex = 0.8, col = "red")
  dev.off()
  
  #proportion of classes: classifiation 
  setwd(file.path(W.DIR,OUT.DIR))
  pdf(paste(CLASS.SHP,"_BARPLOT",c(".pdf"),sep=""),
      width=5.5,height=5.5)#Plotting
  n.tab <-   table(x$CLASS)
  xx <- barplot(n.tab/sum(n.tab)*100,
                ylab="Proportion [%]",
                sub=paste('Total number =',sum(n.tab)),
                xlab="CLASS",
                las=1,
                ylim=c(0,100),
                main=paste(CLASS.SHP))
  text(x = xx, y = 0, label = round(n.tab/sum(n.tab)*100,1), pos = 3, cex = 0.8, col = "red")
  dev.off()
  
  plot(x$CLASS,x$CLASS_PB)
  
  head(x)
  #-------------------------------------------------------------------------------
  print("2 | Overlay")
  #-------------------------------------------------------------------------------
  t <- st_intersection(r,x)
  #-------------------------------------------------------------------------------
  print("Aggregation")
  #-------------------------------------------------------------------------------
  #reference
  t.agg <- data.frame(CLASS=names(split(t,t$CLASS_R)))
  t.temp <- data.frame(CLASS=t$CLASS_R,t[grepl(paste("MD",sep=""), names(t))])
  for(i in names(t.temp[grepl(paste("MD",sep=""), names(t.temp))])){
    x <- aggregate(t.temp[[paste(i)]], 
                   by=list(t.temp$CLASS),
                   FUN=mean, 
                   na.rm=TRUE)
    colnames(x) <- c("CLASS",paste(i))
    t.agg <- merge(t.agg,x,by="CLASS")
  }
  #Export aggregated samples") 
  setwd(file.path(W.DIR,OUT.DIR))
  write.table(t.agg,
              file=paste(POINT.SHP,"_NDVI-agg",c(".csv"),sep=""),
              dec=",",
              sep=";",
              row.names = FALSE)
  #classification
  t.agg <- data.frame(CLASS=names(split(t,t$CLASS)))
  t.temp <- data.frame(CLASS=t$CLASS,t[grepl(paste("MD",sep=""), names(t))])
  for(i in names(t.temp[grepl(paste("MD",sep=""), names(t.temp))])){
    x <- aggregate(t.temp[[paste(i)]], 
                   by=list(t.temp$CLASS),
                   FUN=mean, 
                   na.rm=TRUE)
    colnames(x) <- c("CLASS",paste(i))
    t.agg <- merge(t.agg,x,by="CLASS")
  }
  #Export aggregated samples") 
  setwd(file.path(W.DIR,OUT.DIR))
  write.table(t.agg,
              file=paste(CLASS.SHP,"_NDVI-agg",c(".csv"),sep=""),
              dec=",",
              sep=";",
              row.names = FALSE)
  #-------------------------------------------------------------------------------
  print("3 | Accuracy assessment")
  #-------------------------------------------------------------------------------
  acc.m <- fClassAcc(actual=t$CLASS_R, predicted=t$CLASS)
  setwd(file.path(W.DIR,OUT.DIR))
  write.csv2(acc.m$ConfusionMatrix, 
             file = paste(CLASS.SHP,"__",POINT.SHP,"_CM.csv"))
  write.csv2(acc.m$AccMetrics, 
             file = paste(CLASS.SHP,"__",POINT.SHP,"_AM.csv"))
}
