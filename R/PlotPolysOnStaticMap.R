`PlotPolysOnStaticMap` <-
function(MyMap, polys, col, verbose = 1,...){
  Rcoords <- LatLon2XY.centered(MyMap,lat= polys[,"Y"],lon= polys[,"X"]);
  polys.XY <- polys;
  polys.XY[,"X"] <- Rcoords$newX;
  polys.XY[,"Y"] <- Rcoords$newY;
  if (!missing(col)) polys.XY[,"col"] <- col;
  if ( !( "col" %in% colnames(polys.XY)) )
     polys.XY[,"col"] <- rgb(.1,.1,.1,.05);
  #polys.XY[polys.XY[,"PID"] == 292,"col"] <- rgb(1,0,0,.75)
  tmp <- PlotOnStaticMap(MyMap, verbose=0, ...)
  tmp <- by(polys.XY[,c("X","Y","col")], polys.XY[,"PID"], mypolygon, lwd = .25);
}

