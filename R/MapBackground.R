`MapBackground` <-
function(lat, lon, destfile, NEWMAP=TRUE, myTile, zoom=NULL, size = c(640,640), GRAYSCALE  = FALSE, verbose = 1,...){
   #library(TeachingDemos);#only needed for the function updateusr       
      
   if (verbose < 2) {
   	if (NEWMAP | missing(destfile)){
   	   bb <- qbbox(lat,lon, TYPE = "all", margin = list(m=rep(5,4), TYPE = c("perc", "abs")[1]));
   	   lat.center <- mean(bb$latR);
       lon.center <- mean(bb$lonR);

   	  if (missing(destfile)) destfile = paste(round(lat.center,3),round(lon.center,3),"png",sep=".")
   	  #GetMap(center = c(lat.center, lon.center), zoom = zoom, destfile = destfile);
   	  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = destfile, verbose = verbose, ...);
   	  myTile <- MyMap[[4]];size <- dim(myTile)[1:2];
   	  if (is.null(zoom)) zoom <- min(MaxZoom(bb$latR, bb$lonR, size));
   	  if (verbose) cat("center, zoom: ", lat.center, lon.center, zoom, "\n");
   	  
      if (verbose) print("map Tile received")
    } else {
      cat("no new tile query, instead load prestored ", destfile, "\n");
    
     if (missing(myTile) ) {
     	 #if (require(rimage) & substring(destfile,nchar(destfile)-2,nchar(destfile)) == "jpg"){ 
     	 if (substring(destfile,nchar(destfile)-2,nchar(destfile)) == "jpg"){ 
     	   require(ReadImages);   
 	       myTile <- read.jpeg(destfile);
 	     } else if (require(rgdal)  & substring(destfile,nchar(destfile)-2,nchar(destfile)) == "png"){
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
 	     } else {stop("either rgdal (ONLY png files) or rimage (ONLY jpg files) library are required");}
 	     size <- dim(myTile)[1:2]
        }
       MyMap <- list(lat.center, lon.center, zoom, myTile);
    }
     
     #Gray scale intensity = 0.30R + 0.59G + 0.11B
     #if (GRAYSCALE & attr(myTile, "type") != "gray")
     if (GRAYSCALE ) 
     	myTile <- RGB2GRAY(myTile);
     par(mar=c(0,0,0,0));#par(pin=c(9,9))
     #if (class(MyMap[[4]]) == 'SpatialGridDataFrame'){
   	 if (class(MyMap[[4]])[1] == 'matrix'){
		image(z=MyMap[[4]], col = attr(MyMap[[4]], "COL"))
   	 } else if (class(MyMap[[4]])[1] == 'SpatialGridDataFrame'){
   	 	image(MyMap[[4]], red=1, green=2, blue=3)
   	 } else	{plot(myTile);}
     
   }
   tmp2 <- par('usr');
   updateusr(tmp2[1:2], x2=c(-size[1], size[1])/2, tmp2[3:4], y2=c(-size[2], size[2])/2 );
   
   if (verbose) {
   	cat("tmp2:");print(tmp2); 
    cat("par('usr')");print(par('usr'))
   }
   invisible(list(lat.center, lon.center, zoom, myTile)); 

}

