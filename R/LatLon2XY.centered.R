`LatLon2XY.centered` <-
function(MyMap,lat,lon,zoom){
   lat.center <- MyMap[[1]];
   lon.center <- MyMap[[2]];
   if (missing(zoom)) zoom <- MyMap[[3]];
       
   mypoints <- LatLon2XY(lat,lon,zoom);
   mycenter <- LatLon2XY(lat.center,lon.center,zoom);
   
   Rcoords <- Tile2R(mypoints, mycenter);
   
   return(list(newX=Rcoords$X,newY=Rcoords$Y));
}

