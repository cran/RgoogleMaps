`Tile2R` <-
function(points, center){
  X <- 256* (points$Tile[,"X"] - center$Tile[,"X"]) + (points$Coords[,"x"] - center$Coords[,"x"]) ;
  Y <- -256*(points$Tile[,"Y"] - center$Tile[,"Y"]) - (points$Coords[,"y"] - center$Coords[,"y"]) ;
  return(list(X=X,Y=Y))
}

