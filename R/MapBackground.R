`MapBackground` <-
function(lat, lon, destfile, NEWMAP=TRUE, myTile, zoom=NULL, size = c(640,640), GRAYSCALE  = FALSE, mar=c(0,0,0,0), PLOT = FALSE, verbose = 1,...){
   #library(TeachingDemos);#only needed for the function updateusr       
    bb <- qbbox(lat,lon, TYPE = "all", margin = list(m=rep(5,4), TYPE = c("perc", "abs")[1]));
   	lat.center <- mean(bb$latR);
    lon.center <- mean(bb$lonR);
    if (is.null(zoom)) 
      if (diff(bb$latR) <= 0.000001 | diff(bb$lonR) <=  0.000001) zoom <- 12 else zoom <- min(MaxZoom(bb$latR, bb$lonR, size));
   
    
   	if (NEWMAP | missing(destfile)){
   	  if (missing(destfile)) destfile = paste(round(lat.center,3),round(lon.center,3),"png",sep=".")
   	  #GetMap(center = c(lat.center, lon.center), zoom = zoom, destfile = destfile);
   	  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = destfile, verbose = verbose, size = size,...);
   	  size <- dim(MyMap[[4]])[1:2];
   	  if (is.null(zoom)) zoom <- min(MaxZoom(bb$latR, bb$lonR, size));
   	  if (verbose) cat("center, zoom: ", lat.center, lon.center, zoom, "\n");
     } else {
      cat("no new tile query, instead load prestored ", destfile, "\n");
    
       MyMap <- ReadMapTile(destfile, TRUE);
       if ( !('lat.center' %in% names(MyMap)) | !('lon.center' %in% names(MyMap)) | !('zoom' %in% names(MyMap)) ) 
         MyMap <- list(lat.center=lat.center, lon.center=lon.center, zoom=zoom, myTile= MyMap);
    }
    
     MyMap$BBOX <- list(ll = XY2LatLon(MyMap, -size[1]/2 + 0.5, -size[2]/2 + 0.5), ur = XY2LatLon(MyMap, size[1]/2 - 0.5, size[2]/2 - 0.5) );
     if (GRAYSCALE ) MyMap[[4]] <- RGB2GRAY(MyMap[[4]]);
     if (PLOT){
       par(mar=mar);#par(pin=c(9,9))
       
   	   if (class(MyMap[[4]])[1] == 'matrix'){
		 image(y= seq(MyMap$BBOX$ll[1], MyMap$BBOX$ur[1], length= size[1]), x= seq(MyMap$BBOX$ll[2], MyMap$BBOX$ur[2], length= size[2]), z=MyMap[[4]], col = attr(MyMap[[4]], "COL"), xlab = "", ylab = "")
   	   } else if (class(MyMap[[4]])[1] == 'SpatialGridDataFrame'){
   	 	 image(MyMap[[4]], red=1, green=2, blue=3)
   	   } else	{
   	   	plot(MyMap[[4]]);
   	   }
     
      tmp2 <- par('usr');
      #updateusr(tmp2[1:2], x2=c(-size[1]+1, size[1]-1)/2, tmp2[3:4], y2=c(-size[2]+1, size[2]-1)/2 );
      updateusr(tmp2[1:2], x2=c(MyMap$BBOX$ll[1], MyMap$BBOX$ur[1]), tmp2[3:4], y2=c(MyMap$BBOX$ll[2], MyMap$BBOX$ur[2]));
    
     if (verbose) {
   	   cat("tmp2:");print(tmp2); 
       cat("par('usr')");print(par('usr'))
      }
   }   
   #browser();
   invisible(MyMap); 

}
