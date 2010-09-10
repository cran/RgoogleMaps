`PlotOnStaticMap` <-
function(MyMap, lat, lon, destfile, zoom=NULL, size = c(640,640), GRAYSCALE  = FALSE, add=FALSE, FUN = points, mar=c(0,0,0,0), NEWMAP = TRUE, TrueProj = TRUE, axes= FALSE, verbose = 1,...){
   
   if (!NEWMAP & missing(MyMap) & missing(lat) & missing(lon) ){
   	  MyMap <- ReadMapTile(destfile, TRUE);
   } else if (missing(MyMap)) MyMap <- MapBackground(lat=lat,lon=lon, destfile =destfile, zoom=zoom, size=size, GRAYSCALE =GRAYSCALE, mar=mar, NEWMAP = NEWMAP, verbose=verbose);
   
   if (missing(size)) size <- dim(MyMap[[4]]);
    browser();
    
   lat.center <- MyMap[[1]];
   lon.center <- MyMap[[2]];
   zoom <- MyMap[[3]];
   if (TrueProj & MyMap$url == "OSM") {
   	  print("map type is OpenStreetMap. Until we find the correct projection algorithm, we treat lat/lon like planar coordinates and set TrueProj = FALSE.")
   	  TrueProj = FALSE;
   	}
   
   if ( !('BBOX' %in% names(MyMap)) ) MyMap$BBOX <- list(ll = XY2LatLon(MyMap, -size[1]/2 + 0.5, -size[2]/2 + 0.5), ur = XY2LatLon(MyMap, size[1]/2 - 0.5, size[2]/2 - 0.5) );
   
   if (verbose) print(str(MyMap));
   if (!add) {
   	 #require(rimage)
   	 par(mar=mar);#par(pin=c(9,9))
   	 #if (class(MyMap[[4]]) == 'SpatialGridDataFrame'){
   	 x= seq(MyMap$BBOX$ll[2], MyMap$BBOX$ur[2], length= size[1]);
   	 y= seq(MyMap$BBOX$ll[1], MyMap$BBOX$ur[1], length= size[2]);
   	 if (class(MyMap[[4]])[1] == 'matrix'){
		image(x=x,y=y, z=MyMap[[4]], col = attr(MyMap[[4]], "COL"), xlab = "", ylab = "", axes = FALSE)
   	 	#image(MyMap[[4]], "ind", col = attr(MyMap[[4]], "COL"))
   	 } else if (class(MyMap[[4]])[1] == 'SpatialGridDataFrame'){
   	 	image(MyMap[[4]], red=1, green=2, blue=3, axes = FALSE);
   	 } else {myplot.imagematrix(x=x, y=y, z=MyMap[[4]], axes = FALSE);}
     tmp2 <- par('usr');
     offset = 1;
     if (TrueProj){
       updateusr(tmp2[1:2], x2=c(-size[1]+offset, size[1]-offset)/2, tmp2[3:4], y2=c(-size[2]+offset, size[2]-offset)/2 );
     } 
     if (axes){
     	if (require(sp)) {degAxis(1); degAxis(2);} else {axis(1);axis(2);}
     }
     #browser();
   }
   
   if (!missing(lat) & !missing(lon)){
   	if (TrueProj){
   	 Rcoords <- LatLon2XY.centered(MyMap,lat,lon);
     newX <- Rcoords$newX;
     newY <- Rcoords$newY;
     if (verbose) {
		print(range(newX, na.rm=TRUE));
		print(range(newY, na.rm=TRUE));
		#print(list(newX,newY));
     }
    } else {
       newX <- lon;
       newY <- lat;
    }    
	FUN(newX, newY, ...)
   }
   
   #invisible(list(newX,newY)); 
   invisible(MyMap)
 }

