`GetMap.bbox` <-
function(lonR, latR, center, size = c(640,640), destfile = "MyTile.png", MINIMUMSIZE = FALSE, RETURNIMAGE = TRUE, GRAYSCALE =FALSE, NEWMAP = TRUE, zoom, verbose=1,...){
 	#Example use:
 	#bb <- qbbox(x[j,"lat"],x[j,"lon"])
 	#tmp <- GetMap.bbox(bb$lonR, bb$latR,destfile = "MyTile.sm.jpg", MINIMUMSIZE =T, RETURNIMAGE=T)
 	#or:
 	#bb <- qbbox(c(40.702147,40.711614,40.718217),c(-74.015794,-74.012318,-73.998284), TYPE = "all", margin = list(m=rep(5,4), TYPE = c("perc", "abs")[1]))
 	#MyMap <- GetMap.bbox(bb$lonR, bb$latR,destfile = "TestMarkers.sm.jpg", MINIMUMSIZE =T, RETURNIMAGE=T, markers = '40.702147,-74.015794,blues%7C40.711614,-74.012318,greeng%7C40.718217,-73.998284,redc')
 	
 	if (missing(zoom)) zoom <- min(MaxZoom(latR, lonR, size));
 	if (missing(center)){
 	  lat.center <- mean(latR);#latR[1] + diff(latR)/2;
      lon.center <- mean(lonR);#lonR[1] + diff(lonR)/2;
    } else {
    	lat.center <- center[1];
    	lon.center <- center[2];
    }
     
    if (MINIMUMSIZE){
    	ll <- LatLon2XY(latR[1], lonR[1], zoom);#lower left corner
    	ur <- LatLon2XY(latR[2], lonR[2], zoom );#upper right corner
    	cr <- LatLon2XY(lat.center, lon.center, zoom );#center
    	ll.Rcoords <- Tile2R(ll, cr);
    	ur.Rcoords <- Tile2R(ur, cr);
    	if (verbose>1){
    		cat("ll:"); print(ll);print(ll.Rcoords)
    		cat("ur:"); print(ur);print(ur.Rcoords);
    		cat("cr:"); print(cr);
    	}
    	#return(list(ll,ur));
    	size[1] <- 2*max(c(ceiling(abs(ll.Rcoords$X)), ceiling(abs(ur.Rcoords$X)) ))+1;
    	size[2] <- 2*max(c(ceiling(abs(ll.Rcoords$Y)), ceiling(abs(ur.Rcoords$Y)) ))+1;
    	#size[1] <- ceiling(abs(256* (ur$Tile[,"X"] - ll$Tile[,"X"]) + (ur$Coords[,"x"] - ll$Coords[,"x"])));
    	#size[2] <- ceiling(abs(256* (ur$Tile[,"Y"] - ll$Tile[,"Y"]) + (ur$Coords[,"y"] - ll$Coords[,"y"])));
    	if (verbose) cat("new size: ", size, "\n")
    }
 	#if (NEWMAP) 
 	invisible(GetMap(center = c(lat.center, lon.center), zoom = zoom, size=size, destfile = destfile, RETURNIMAGE = RETURNIMAGE, verbose = verbose, ...));
 	
 	
 }

