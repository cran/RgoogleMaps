`XY2LatLon` <-
function(MyMap,X,Y,zoom){
#X and Y are the centered integer pixel values, i.e. they should be zero in the center of the matrix/image !

   lat.center <- MyMap[[1]];
   lon.center <- MyMap[[2]];
   if (missing(zoom)) zoom <- MyMap[[3]];
      
   mycenter <- LatLon2XY(lat.center,lon.center,zoom);
   
   #first transform to original x,y coordinates
   x <- mycenter$Tile[,"X"] + (X+mycenter$Coords[,"x"])/256;
   y <- mycenter$Tile[,"Y"] - (Y-mycenter$Coords[,"y"])/256;
   
   ytilde <- 1 - y/2^(zoom-1);
   yy = (exp(2*pi* ytilde) - 1)/(exp(2*pi* ytilde) + 1);
   ShiftLat <- function(yy){
   	 n = c(-1,0,1);
     #print(180*asin( yy )/pi)
     #lat = 2*pi*(n+1) - asin( yy ) ; 
     lat = 2*pi*(n) + asin( yy ) ; 
     lat <- lat[which(lat <= pi/2 & lat > -pi/2 )];
     #convert back to degrees: 
     lat <- 180 * lat/ pi;
     return(lat);
   }
   lat <- sapply(yy, ShiftLat);
   #longitude is easy:
   lon =  180*( x/2^(zoom-1) - 1 );
   
   return(cbind(lat=lat, lon=lon));
}

