`PlotPolysOnStaticMap` <-
function(MyMap, polys, col, border = NULL, lwd = .25, verbose = 0, add=TRUE,...){
  Rcoords <- LatLon2XY.centered(MyMap,lat= polys[,"Y"],lon= polys[,"X"]);
  polys.XY <- as.data.frame(polys);
  polys.XY[,"X"] <- Rcoords$newX;
  polys.XY[,"Y"] <- Rcoords$newY;
  
  if ( !( "PID" %in% colnames(polys.XY)) ) 
     polys.XY[,"PID"] <- 1;
  if ( !( "SID" %in% colnames(polys.XY)) ) 
     polys.XY[,"SID"] <- 1;
  polys.XY[,"PIDSID"] <- apply(polys.XY[,c("PID","SID")],1,paste,collapse=":")
  if (!add) tmp <- PlotOnStaticMap(MyMap, verbose=0, ...)
  if (verbose>1) browser()

if (require(PBSmapping) & all(c("PID","X","Y","POS") %in% colnames(polys.XY)) ) {
	attr(polys.XY, "projection") <- NA;
	addPolys(polys.XY,col=col, border = border, lwd = lwd, ...)
} else {
  if (!missing(col)) {
  	polys.XY[,"col"] <- col;
  	PIDtable <- as.numeric(table(polys.XY[,"PID"]));
  	SIDtable <- as.numeric(table(polys.XY[,"PIDSID"]))
  	if (length(SIDtable)==length(col))  polys.XY[,"col"] <- rep(col, SIDtable);
  	if (length(PIDtable)==length(col))  polys.XY[,"col"] <- rep(col, PIDtable);
  }
  
  if ( !( "col" %in% colnames(polys.XY)) )
     polys.XY[,"col"] <- rgb(.1,.1,.1,.05);
    #polys.XY[polys.XY[,"PID"] == 292,"col"] <- rgb(1,0,0,.75)
  #tmp <- by(polys.XY[,c("X","Y","col")], polys.XY[,"PID"], mypolygon, lwd = lwd, border = border);
  #if (0){
  	pids = unique(polys.XY[,"PIDSID"])
  	for (i in pids){
  	  jj = 	polys.XY[,"PIDSID"] == i;
  	  xx= polys.XY[jj,];
  	  if ( ( "POS" %in% colnames(xx)) ) xx <- xx[order(xx[,"POS"]),]
  	  polygon( xx[, c("X","Y")], col=xx[,"col"]);
  	  #readLines(n=1)
  	}
  #}
  }
}

