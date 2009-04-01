`RGB2GRAY` <-
function(myTile){
#Gray scale intensity = 0.30R + 0.59G + 0.11B
     stopifnot(attr(myTile, "type") != "gray");
     
     if (class(myTile) == 'matrix'){
     	if ('COL' %in% names(attributes(myTile))){
     		tmp <- col2rgb(attr(myTile, 'COL'));
     		attr(myTile, 'COL') <- gray( (.3*tmp[1,] + .59*tmp[2,] + .11*tmp[3,])/256);
     	}  
     } else {
       tmp <- myTile;
       myTile <- myTile[,,1];
       myTile <- .3*tmp[,,1] + .59*tmp[,,2] + .11*tmp[,,3];
       attr(myTile, "class") <- c("imagematrix", "array");
     }
     attr(myTile, "type") <- "grey";
     
     return(myTile);	
}

