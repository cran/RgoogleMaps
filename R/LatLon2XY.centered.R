`LatLon2XY.centered` <-structure(function#computes the centered coordinate transformation from lat/lon to map tile coordinates 
### The function LatLon2XY.centered(MyMap, lat,lon,zoom) computes the coordinate transformation from lat/lon to map tile coordinates given a map object.
(
  MyMap, ##<< map object 
  lat, ##<< latitude values to transform
  lon, ##<< longitude values to transform
  zoom ##<< optional zoom level. If missing, taken from \code{MyMap}
){
   lat.center <- MyMap[[1]];
   lon.center <- MyMap[[2]];
   if (missing(zoom)) zoom <- MyMap[[3]];
       
   mypoints <- LatLon2XY(lat,lon,zoom);
   mycenter <- LatLon2XY(lat.center,lon.center,zoom);
   
   Rcoords <- Tile2R(mypoints, mycenter);
   
  ##seealso<< \link{LatLon2XY} \link{Tile2R} 
   return(list(newX=Rcoords$X,newY=Rcoords$Y));
### properly scaled and centered (with respect to the center of \code{MyMap} ) coordinates  
###  \item{newX }{ transformed longitude}
###  \item{newY }{transformed latitude}
})


