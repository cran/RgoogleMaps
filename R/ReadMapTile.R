ReadMapTile <- function(destfile, METADATA = TRUE){
  fileBase <- substring(destfile,1, nchar(destfile)-4);
  fileExt <-  substring(destfile,nchar(destfile)-2,nchar(destfile));
  
  
  if (fileExt == "jpg"){ 
     	   require(ReadImages);   
 	       myTile <- read.jpeg(destfile);
 	     } else if (require(rgdal)  & fileExt == "png"){
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
 	     } else {stop("either rgdal (ONLY png files) or rimage (ONLY jpg files) library are required");
 	    }
 size <- dim(myTile)[1:2];
 if (METADATA) {
    try({
 	#load(paste(fileBase,"rda",sep="."));
 	load(paste(destfile,"rda",sep="."));
 	MyMap <- list(lat.center= MetaInfo$lat.center, lon.center=MetaInfo$lon.center, zoom=MetaInfo$zoom, myTile=myTile, BBOX = MetaInfo$BBOX, url = MetaInfo$url);
 	return(MyMap);
   });
 } 
 return(myTile);
 
}	
	
