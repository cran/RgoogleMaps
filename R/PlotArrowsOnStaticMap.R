`PlotArrowsOnStaticMap` <-
function(MyMap, lat0, lon0, lat1 = lat0, lon1 = lon0, TrueProj = TRUE, FUN = arrows, add = FALSE, verbose = 1,...){
      
  if (TrueProj){
    Rcoords0 <- LatLon2XY.centered(MyMap,lat= lat0,lon= lon0);   
    Rcoords1 <- LatLon2XY.centered(MyMap,lat= lat1,lon= lon1);
  }  else { #no transformtion
    Rcoords0 <- list(newY= lat0,newX= lon0);   
    Rcoords1 <- list(newY= lat1,newX= lon1);
  }
  
  if (!add) tmp <- PlotOnStaticMap(MyMap, TrueProj = TrueProj, verbose=verbose);
  FUN(x0=Rcoords0$newX, y0=Rcoords0$newY, x1=Rcoords1$newX, y1=Rcoords1$newY, ...);
}


