`GetMap.OSM` <-
function(lonR=c(-74.02132,-73.98622), latR= c(40.69983,40.72595), scale= 20000, destfile = "MyTile.png", format = 'png', RETURNIMAGE = TRUE, GRAYSCALE =FALSE, NEWMAP = TRUE, verbose=1,...){
 	 options(scipen = 12);#to suppress scientific notation on the scale parameter
 	 
 	OSMbbox <- paste(lonR[1],latR[1],lonR[2],latR[2], sep=",")
 	#http://tile.openstreetmap.org/cgi-bin/export?bbox=-4.54,47.49,4.37,52.45&scale=14000000&format=png
    OSMurl <- 'http://tile.openstreetmap.org/cgi-bin/export?';
 	url <- paste(OSMurl, "bbox=", OSMbbox, "&scale=", scale, "&format=", format, sep="")
 	#OR, use zoom level (e.g. z=12 ):
 	# http://tah.openstreetmap.org/MapOf/index.php?long=-74.02132&lat=40.69983&z=12&w=256&h=256&format=png

 	if (verbose) print(url)
 	    
 	if (NEWMAP) ret <- download.file(url, destfile, mode="wb", quiet = FALSE);
    BBOX <- list(ll = c(lonR[1], latR[1]), ur = c(lonR[2], latR[2]) );
	MetaInfo <- list(lat.center = mean(latR), lon.center  = mean(lonR), zoom = NULL, url = "OSM", BBOX = BBOX, scale=scale);
	save(MetaInfo, file = paste(destfile,"rda",sep="."));
 	
 	if (RETURNIMAGE){
 	  myMap <- ReadMapTile(destfile);
 	  if (GRAYSCALE) 
     	myMap$myTile <- RGB2GRAY(myMap$myTile);
     invisible(myMap);	    
 	}
 }

