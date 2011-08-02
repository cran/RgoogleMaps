`TextOnStaticMap` <-
function(MyMap, lat, lon, labels = seq_along(lat), TrueProj = TRUE, FUN = text, add = FALSE, verbose = 1,...){
      
  if (TrueProj){
    Rcoords <- LatLon2XY.centered(MyMap,lat= lat,lon= lon);   
  }  else { #no transformtion
    Rcoords <- list(newY= lat,newX= lon);   
  }
  
  if (!add) tmp <- PlotOnStaticMap(MyMap, TrueProj = TrueProj, verbose=verbose);
  FUN(x=Rcoords$newX, y=Rcoords$newY, labels = labels, ...);
}
