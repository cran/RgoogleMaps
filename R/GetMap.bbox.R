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
 	if (NEWMAP) GetMap(center = c(lat.center, lon.center), zoom = zoom, size=size, destfile = destfile, verbose = verbose, ...);
 	
 	if (RETURNIMAGE){
 	  #if (require(rimage) & substring(destfile,nchar(destfile)-2,nchar(destfile)) == "jpg"){
 	  if ( substring(destfile,nchar(destfile)-2,nchar(destfile)) == "jpg"){
 	  #if (@RIMAGE@){
 	    require(ReadImages);
 	    myTile <- read.jpeg(destfile);
 	  #} else if (@RIMAGE@){ 
 	   } else if (require(rgdal)  & substring(destfile,nchar(destfile)-2,nchar(destfile)) == "png"){
 	    #require(rgdal)
 	    myTile <- readGDAL(destfile, silent = TRUE);
 	    if (0){
 	      myTile@data <- myTile@data[,1:3]
 	      ## create index for RGB colours
		  col <- SGDF2PCT(myTile) ## myTile is a spatialGridDataFrame with 3 bands
		  myTile$ind <- col$idx ## add the colour index to the data frame
		  myTile <- as.image.SpatialGridDataFrame(myTile["ind"],1,2)$z;
		  attr(myTile, "COL") <- col$ct;
	    } else {
		   	 myTile <- SPGDF2matrix(myTile);
		}	
		attr(myTile, "type") <- "rgb";
		#image(myTile, "ind", col = col$ct) 
 	  } else stop("either rgdal (ONLY png files) or rimage (ONLY jpg files) library are required");
 	  #if (GRAYSCALE & attr(myTile, "type") == "rgb")
 	  if (GRAYSCALE) 
     	myTile <- RGB2GRAY(myTile);
 	  invisible(list(lat.center, lon.center, zoom, myTile));
 	}
 }

