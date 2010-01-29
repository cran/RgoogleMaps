`GetMap` <-
function(key, center, zoom=12, markers, path, span, frame, hl, sensor = 'true', maptype = c("roadmap","mobile","satellite","terrain","hybrid","mapmaker-roadmap","mapmaker-hybrid")[4], format = c("gif","jpg","jpg-baseline","png8","png32")[5], size = c(640,640), destfile = "MyTile.png", RETURNIMAGE = TRUE, GRAYSCALE =FALSE, verbose=1){
  
  #Note that size is in order (lon, lat) !
  stopifnot(all(size <=640));
   if (missing(key)) {
  	KeyFile <- paste(Sys.getenv("HOME"), "/API.key.txt",sep="");
  	if (!file.exists(KeyFile)) stop(paste("You need to pass a Google Maps API key or keep it in the file ", KeyFile, ". If you do not already have a Google Maps API key, sign up for a free API key at http://code.google.com/apis/maps/signup.html"));
  	key <- scan(KeyFile, what = "");
  }
  fileBase <- substring(destfile,1, nchar(destfile)-4);
  fileExt <-  substring(destfile,nchar(destfile)-2,nchar(destfile));
  #save meta information about the image:    
  if (!missing(center) & !missing(zoom)) {  
      MyMap <- list(lat.center = center[1], lon.center  = center[2], zoom = zoom);
      BBOX <- list(ll = XY2LatLon(MyMap, -size[1]/2 + 0.5, -size[2]/2 - 0.5), ur = XY2LatLon(MyMap, size[1]/2 + 0.5, size[2]/2 - 0.5) );
	  MetaInfo <- list(lat.center = center[1], lon.center  = center[2], zoom = zoom, url = "google", BBOX = BBOX);
	  save(MetaInfo, file = paste(destfile,"rda",sep="."));
	} else {
	  print("Note that when center and zoom are not specified, no meta information on the map tile can be stored. This basically means that R cannot compute proper coordinates. You can still download the map tile and view it in R but overlays are not possible. Do you want to proceed ? (y/n)");
	  ans <- readLines(n=1);
	  if (ans != "y") return(); 
	}

  if (length(size) < 2) {s <- paste(size,size,sep='x')} else {s <- paste(size,collapse="x");}
  if (!missing(center)) center <- paste(center,collapse=",")
  if (missing(format)){
    if ( fileExt == "jpg") format <- "jpg";	
    if ( fileExt == "png") format <- "png32"
  }
 
  googleurl <- 'http://maps.google.com/staticmap?';
	
	if (!missing(span)){#Images may specify a viewport (defined by latitude and longitude values expressed as degrees) to display around a provided center point by passing a span parameter. Defining a minimum viewport in this manner obviates the need to specify an exact zoom level. The static map service uses the span parameter in conjunction with the size parameter to construct a map of the proper zoom level which includes at least the given viewport constraints.
		span <- paste(span,collapse=",")
		url <- paste(googleurl, "center=", center, "&span=", span,  "&size=",  s, "&maptype=", maptype, "&format=", format, "&key=", key, "&sensor=", sensor, sep="")

	} else 	if (missing(center) & missing(zoom)) {#let the Static Maps API determine the correct center and zoom level implicitly, based on evaluation of the position of the markers:
		stopifnot(!missing(markers));
		url <- paste(googleurl,  "size=",  s, "&maptype=", maptype, "&format=", format, "&key=", key, "&sensor=", sensor, sep="")
	} else {
		stopifnot(!missing(center), !missing(zoom));
		url <- paste(googleurl, "center=", center, "&zoom=", zoom,  "&size=",  s, "&maptype=", maptype, "&format=", format, "&key=", key, "&sensor=", sensor, sep="")
	}
	
	if (!missing(markers)) {
		#assumes markers is a list with names lat, lon, size (optional), color (optional), char (optional)
		if (is.character(markers)) {#already in the correct string format:
		  markers.string <- markers;
		} else if (is.data.frame(markers)) {
		  for (i in 1:nrow(markers)){
		  	m1 <- '';
		  	m <- paste(markers[i,c("lat","lon")], collapse=",")
		  	if (all(c("size","col","char") %in% colnames(markers) ) ) {
		  	  m1 <-  paste(markers[i,c("size","col","char")],collapse="");
		  	  m <- paste(m,m1,sep=",")
		  	}
		  	#print(m)
		  	#note that the above depends on the correct column order!
		  	if (i==1){ markers.string <- m;
			} else { markers.string <- paste(markers.string,m, sep=''); }
			if (i < nrow(markers))#only put a | if there is more to come:
			  markers.string <- paste(markers.string,'|', sep='');
			#print(markers.string)
		  }
		}
		if (verbose) print(markers.string);
		url <- paste(url, "&markers=", markers.string, sep="")
	}
	if (verbose) print(url);
	if (verbose < 2) download.file(url, destfile, mode="wb", quiet = TRUE);
	
	if (RETURNIMAGE){
 	  myMap <- ReadMapTile(destfile);
 	  if (GRAYSCALE) 
     	myMap$myTile <- RGB2GRAY(myMap$myTile);
 	  invisible(myMap);
 	}
 	
	invisible(url)
}

