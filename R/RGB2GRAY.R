`RGB2GRAY` <-
function(myTile){
#Gray scale intensity = 0.30R + 0.59G + 0.11B
     stopifnot(attr(myTile, "type") != "gray");
     
     if (class(myTile)[1] == 'matrix'){
     	if ('COL' %in% names(attributes(myTile))){
     		tmp <- col2rgb(attr(myTile, 'COL'));
     		attr(myTile, 'COL') <- gray( (.3*tmp[1,] + .59*tmp[2,] + .11*tmp[3,])/256);
     	}  
     } else if (class(myTile)[1] == 'SpatialGridDataFrame'){
        myTile@data <- myTile@data[,1:3]
 	    ## create index for RGB colours
	    col <- SGDF2PCT(myTile) ## myTile is a spatialGridDataFrame with 3 bands
	    myTile$ind <- col$idx ## add the colour index to the data frame
		myTile <- as.image.SpatialGridDataFrame(myTile["ind"],1,2)$z;
		attr(myTile, "COL") <- col$ct;
		tmp <- myTile;
        myTile <- myTile[,,1];
        myTile <- .3*tmp[,,1] + .59*tmp[,,2] + .11*tmp[,,3];
        attr(myTile, "class") <- c("imagematrix", "array");
     } else {
       tmp <- myTile;
       myTile <- myTile[,,1];
       myTile <- .3*tmp[,,1] + .59*tmp[,,2] + .11*tmp[,,3];
       attr(myTile, "class") <- c("imagematrix", "array");
     }
     attr(myTile, "type") <- "grey";
     
     return(myTile);	
}

