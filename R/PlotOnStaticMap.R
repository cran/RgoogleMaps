`PlotOnStaticMap` <-
function(MyMap, lat, lon, destfile, zoom=NULL, size = c(640,640), GRAYSCALE  = FALSE, add=FALSE, FUN = points, verbose = 1,...){
   
   if (missing(MyMap)) MyMap <- MapBackground(lat=lat,lon=lon, destfile =destfile, zoom=zoom, size=size, GRAYSCALE =GRAYSCALE, verbose=verbose);
   if (verbose) print(str(MyMap));
   
   lat.center <- MyMap[[1]];
   lon.center <- MyMap[[2]];
   zoom <- MyMap[[3]];
   
   if (!add) {
   	 #require(rimage)
   	 par(mar=c(0,0,0,0));#par(pin=c(9,9))
   	 #if (class(MyMap[[4]]) == 'SpatialGridDataFrame'){
   	 if (class(MyMap[[4]]) == 'matrix'){
		image(z=MyMap[[4]], col = attr(MyMap[[4]], "COL"))
   	 	#image(MyMap[[4]], "ind", col = attr(MyMap[[4]], "COL"))
   	 } else {plot(MyMap[[4]]);}
     tmp2 <- par('usr');
     updateusr(tmp2[1:2], x2=c(-size[1], size[1])/2, tmp2[3:4], y2=c(-size[2], size[2])/2 );
   }
   
   if (!missing(lat) & !missing(lon)){
   	 Rcoords <- LatLon2XY.centered(MyMap,lat,lon);
     newX <- Rcoords$newX;
     newY <- Rcoords$newY;
     if (verbose) {
		print(range(newX, na.rm=TRUE));
		print(range(newY, na.rm=TRUE));
		#print(list(newX,newY));
     }
	 FUN(newX, newY, ...)
   }
   
   #invisible(list(newX,newY)); 
   invisible(MyMap)
 }

