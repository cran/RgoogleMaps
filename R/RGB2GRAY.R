`RGB2GRAY` <-structure(function#translates an RGB image matrix to gray scale
###  This function translates the rgb values of the array myTile into a scalar matrix with just one gray value per pixel.
(
  myTile ##<< rgb image matrix, usually array with 3 dimensions
){
#Gray scale intensity = 0.30R + 0.59G + 0.11B
     stopifnot(attr(myTile, "type") != "gray");
##details<< Gray scale intensity defined as  0.30R + 0.59G + 0.11B
     
    if (class(myTile)[1] == 'array'){
      if (0) {
        tmp = matrix(0.7, ncol=dim(myTile)[2], nrow=dim(myTile)[1] )
        for (i in 1:nrow(tmp))
		for (j in 1:ncol(tmp))
			tmp[i,j] = .3*myTile[i,j,1] + .59*myTile[i,j,2] + .11*myTile[i,j,3]
         myTile = tmp
       }
       #or in vectorized form:
	 ncol=dim(myTile)[2]; nrow=dim(myTile)[1]
	 dim(myTile) <- c(prod(dim(myTile)[1:2]),dim(myTile)[3])
	 tmp = .3*myTile[,1] + .59*myTile[,2] + .11*myTile[,3]
       myTile = matrix(tmp, ncol=ncol, nrow=nrow )

     } 
     
     return(myTile);	
### image tile
})

