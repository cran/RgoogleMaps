`qbbox` <-
function(lat, lon, TYPE = c("all", "quantile")[1], margin = list(m=c(1,1,1,1), TYPE = c("perc", "abs")[1]),  q.lat = c(0.1,0.9), q.lon = c(0.1,0.9), verbose=0){
 	if (TYPE == "all"){
 	  latR <- range(lat,na.rm=TRUE);
   	  lonR <- range(lon,na.rm=TRUE)
 	} else if (TYPE == "quantile"){
 	  latR <- quantile(lat, q.lat);
      lonR <- quantile(lon, q.lon);
 	}
 	if (!is.null(margin)){
 	  m <- margin$m;
 	  lat.center <- latR[1] + diff(latR)/2;
      lon.center <- lonR[1] + diff(lonR)/2;
      if (margin$TYPE == "perc"){
      	dlon <- c(-1,1)*(1+m[c(2,4)]/100)*diff(lonR)/2;
      	dlat <- c(-1,1)*(1+m[c(1,3)]/100)*diff(latR)/2;
      } else if (margin$TYPE == "abs"){
      	dlon <- c(-1,1)*(m[c(2,4)] + diff(lonR)/2);
      	dlat <- c(-1,1)*(m[c(1,3)] + diff(latR)/2);
      }
      lonR.margin <- lon.center + dlon;
      latR.margin <- lat.center + dlat;
      if (verbose>1) {
      	cat("old/new lon range:");print(lonR);print(lonR.margin);
      	cat("old/new lat range:");print(latR);print(latR.margin);
      }
      return(list(latR=latR.margin, lonR=lonR.margin))
 	}
 		
 	return(list(latR=latR, lonR=lonR))
 }

