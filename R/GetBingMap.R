`GetBingMap` <- structure(function# download a static map from the Microsoft map tile server
### Query the Google server for a static map tile, defined primarily by its 
### center and zoom. Many additional arguments allow the user to customize 
### the map tile.
(
  center=c(lat=42, lon=-76), ##<< optional center (lat first,lon second  )
  mapArea=c(45.219,-122.325,47.610,-122.107), ##<< A rectangular area specified as a bounding box (ll,ur). Required when a center point or set of route points are not specified
  size = c(640,640), ##<< desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels 
  destfile, ##<<  File to load the map image from or save to, depending on \code{NEWMAP}.
  zoom =12, ##<< Google maps zoom level.
  markers,  ##<< (optional) defines one or more markers to attach to the image at specified locations. This parameter takes a string of marker definitions separated by the pipe character (|) 
  path="",  ##<< (optional) defines a single path of two or more connected points to overlay on the image at specified locations. This parameter takes a string of point definitions separated by the pipe character (|)
 
  # span, ##<< (optional) defines a minimum viewport for the map image expressed as a latitude and longitude pair. The static map service takes this value and produces a map of the proper zoom level to include the entire provided span value from the map`s center point. Note that the resulting map may include larger bounds for either latitude or longitude depending on the rectangular dimensions of the map. If zoom is specified, span is ignored 
#  frame, ##<< (optional) specifies that the resulting image should be framed with a colored blue border. The frame consists of a 5 pixel, 55 % opacity blue border. 
 # hl, ##<< (optional) defines the language to use for display of labels on map tiles. Note that this paramater is only supported for some country tiles; if the specific language requested is not supported for the tile set, then the default language for that tile set will be used.
#  sensor = "true",  ##<< specifies whether the application requesting the static map is using a sensor to determine the user`s location. This parameter is now required for all static map requests.
  maptype = c("Road","Aerial","AerialWithLabels")[1], ##<< defines the type of map to construct. See https://msdn.microsoft.com/en-us/library/ff701724.aspx
  format = c("png","gif","jpg","jpg-baseline","png8","png32")[1],  ##<< (optional) defines the format of the resulting image. By default, the Static Maps API creates GIF images. There are several possible formats including GIF, JPEG and PNG types. Which format you use depends on how you intend to present the image. JPEG typically provides greater compression, while GIF and PNG provide greater detail. This version supports only PNG.
  extraURL="", ##<< custom URL suffix
  RETURNIMAGE = TRUE, ##<< return image yes/no default: TRUE
  GRAYSCALE =FALSE, ##<< Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY}
  NEWMAP = TRUE, ##<< if TRUE, query the Google server and save to \code{destfile}, if FALSE load from destfile. 
  SCALE = 1, ##<< use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map
  apiKey = NULL, ##<< optional API key (allows for higher rate of downloads)
  verbose=0 ##<< level of verbosity
){
  ##note<<Note that size is in order (lon, lat)
  if (!(maptype %in% c("Road","Aerial","AerialWithLabels"))) maptype="Road"
  
  if (missing(destfile)) destfile=file.path(tempdir(),"mapTile.png")
  if (is.character(center)) {
    if (verbose) cat("geocoding ", center, "\n")
    center = getGeoCode(center,verbose)
  }
  if (all(c("lat","lon") %in% names(center))) center = center[c("lat","lon")]
  ##seealso<< \link{GetMap.bbox}
  
#   if (is.null(names(center))) {
#     names(center) = c("lat", "lon");
#   } else stopifnot( all(names(center) %in% c("lat", "lon")) )
#   
  stopifnot(all(size <=640));
  
  fileBase <- substring(destfile,1, nchar(destfile)-4);
  fileExt <-  substring(destfile,nchar(destfile)-2,nchar(destfile));
  #save meta information about the image:    
  if (is.null(center)) {
    if (verbose) print("Note that when center and zoom are not specified, no meta information on the map tile can be stored. This basically means that R cannot compute proper coordinates. You can still download the map tile and view it in R but overlays are not possible.");
	  #ans <- readLines(n=1);
	  #if (ans != "y") return(); 
	  MetaInfo <- list(lat.center = NULL, lon.center  = NULL, zoom = zoom, 
	                   url = "bing", BBOX = NULL, size=size, SCALE = SCALE);
	  save(MetaInfo, file = paste(destfile,"rda",sep="."));
  } else if ( is.numeric(center) & !missing(zoom)) {  
      MyMap <- list(lat.center = center[1], lon.center  = center[2], zoom = zoom, SCALE = SCALE);
      BBOX <- list(ll = XY2LatLon(MyMap, -size[1]/2 + 0.5, -size[2]/2 - 0.5), ur = XY2LatLon(MyMap, size[1]/2 + 0.5, size[2]/2 - 0.5) );
	  MetaInfo <- list(lat.center = center[1], lon.center  = center[2], zoom = zoom, 
        url = "bing", BBOX = BBOX, size=size, SCALE = SCALE);
	  save(MetaInfo, file = paste(destfile,"rda",sep="."));
  } 
  
  if (length(size) < 2) {s <- paste(size,size,sep=",")} else {s <- paste(size,collapse=",");}
  if (!is.null(center)) center <- paste(center,collapse=",")
  # if (missing(format)){	
  #   if ( fileExt == "png") format <- "png32"
  # }
 
  bingURL = paste0("http://dev.virtualearth.net/REST/v1/Imagery/Map/",maptype, "/")
  #paste0(bingURL,"Road/47.619048,-122.35384/15?mapSize=640,640&key=",apiKey)
 
  #browser()
  # if (!missing(extraURL)) {
  #   url <- paste0(bingURL,extraURL,"&format=", format)
  # } else 
  if (is.null(center) & missing(zoom)) {#let the Static Maps API determine the correct center and zoom level implicitly, based on evaluation of the position of the markers:
		stopifnot(!missing(markers) | path != "");
		url <- paste0(bingURL,  "size=",  s, "&maptype=", maptype, "&format=", format)
	} else if (missing(mapArea)) {
		stopifnot(!is.null(center), !missing(zoom));
		url <- paste0(bingURL, center, "/", zoom,  "?mapSize=",  s, "&format=", format)
	} else if (!missing(mapArea)) {
	  latR=range(mapArea[c(1,3)])
	  lonR=range(mapArea[c(2,4)])
	  zoom <- min(MaxZoom(latR, lonR, size));
	  #if (missing(center)){
	    lat.center <- mean(latR);#latR[1] + diff(latR)/2;
	    lon.center <- mean(lonR);#lonR[1] + diff(lonR)/2;
	  #}
	  center = c(lat.center, lon.center)
	  BBOX = list(ll = mapArea[1:2], ur =mapArea[3:4])
	  names(BBOX$ll) = c("lat", "lon");names(BBOX$ur) = c("lat", "lon")
	  MetaInfo <- list(lat.center = center[1], lon.center  = center[2], zoom = zoom, 
	                   url = "bing", BBOX = BBOX, size=size, SCALE = SCALE);
	  save(MetaInfo, file = paste(destfile,"rda",sep="."));
	  
	  bingURL = paste0("http://dev.virtualearth.net/REST/v1/Imagery/Map/",maptype, "?mapArea=")
	  url <- paste0(bingURL, paste0(mapArea,collapse=","), "&mapSize=",  s, "&format=", format)
	}
	
	url <- paste(url, path, sep="");
	url <- paste(url, extraURL, sep="");
	
  #if (!missing(hl)) url <- paste0(url, "&language=",hl);
  #if (SCALE == 2) url <- paste(url, "&scale=", SCALE, sep="");
	
	if (!missing(markers)) {
		#assumes markers is a list with names lat, lon, size (optional), color (optional), char (optional)
		#if(is.data.frame(markers)) markers<-as.matrix(markers)
		if ( is.matrix(markers) | is.data.frame(markers)) {
		  stopifnot(all(c("lat","lon") %in% colnames(markers)))
		  latlon = which(colnames(markers) %in% c("lat","lon"))
		  for (i in 1:nrow(markers)){
		  	m1 <- paste(markers[i,c("lat","lon")], collapse=",");
		  	if (any(c("size","color","label") %in% colnames(markers) ) ) {
		  	  m2 <- paste(colnames(markers)[-latlon], markers[i,-latlon], collapse="|",sep=":");
		  	  m <- paste(m2,m1,sep="|")
		  	} else {
		  	  m <- m1
		  	}
		  	#m <- paste("&markers=",m,sep="")
		  	#print(m)
		  	if (i==1){ 
		  		markers.string <- paste0("&markers=",m);
			  } else { 
				  markers.string <- paste(markers.string,paste0("&markers=",m), sep=""); 
		    }
		    #if (verbose) print(markers.string);
		  }
		  #browser()
		  #markers.string <- paste("&markers=", markers.string,sep="")
		} else if (is.character(markers)) {#already in the correct string format:
		  markers.string <- markers;
		} 
		
		url <- paste0(url, markers.string);
    
	}
  #if (!is.null(API_console_key))  url <- paste0(url,"&key=", API_console_key);
	url <- paste0(url, "&key=", apiKey)
	
	if (verbose) print(url);
	if (verbose == -1) browser();
	if (verbose < 2 & NEWMAP) 
	  suppressWarnings(download.file(url, destfile, mode="wb", quiet = TRUE));
	
	if (GRAYSCALE) {
		myTile <- readPNG(destfile, native=FALSE);
		#browser()
		myTile <- RGB2GRAY(myTile);
      	writePNG(myTile, destfile)
	  }

	if (RETURNIMAGE){
 	  myMap <- ReadMapTile(destfile); 
  	  return(myMap);
 	}
 	
	invisible(url)
### map structure or URL used to download the tile.
}, ex = function(){
  
  if (0){
    #for bing maps you will need your own API key, 
    #sign up at https://msdn.microsoft.com/en-us/library/ff428642.aspx
    apiKey = scan("bingAPIkey.txt",what="")
    map1=GetBingMap(center=c(47.619048,-122.35384),zoom=15,apiKey=apiKey, 
                    verbose=1, destfile="Seattle.png") 
    PlotOnStaticMap(map1)
    m="&pp=47.620495,-122.34931;21;AA&pp=47.619385,-122.351485;;AB&pp=47.616295,-122.3556;22"
    map2=GetBingMap(center=c(47.619048,-122.35384),zoom=15,markers=m,apiKey=apiKey, 
                    verbose=1, destfile="Seattle2.png")
    
    PlotOnStaticMap(map2,lat=c(47.620495,47.619385,47.616295),
                    lon=c(-122.34931,-122.351485,-122.3556))
    	
    m="&pp=49.28273,-123.12074;22&pp=44.05207,-123.08675;22"
    
    map3= GetBingMap(center=c(47.677006,-122.125526),zoom=6,markers=m,apiKey=apiKey,
                     verbose=1, destfile="Seattle2.png")
    #plotmap(map=map3)
    m=cbind.data.frame(lat=c(49.28273,44.05207),lon=c(-123.12074,-123.08675),col=c(3:4))
    PlotOnStaticMap(map3, lat =m$lat,lon=m$lon,col=m$col,pch=19)
    
    #overlay traffic:
    #Get a map with Road imagery and traffic flow based on a query.
    #This example gets a map with road imagery based on a query result Bellevue, Washington. 
    #Traffic flow is also included on the map.
    
    #http://dev.virtualearth.net/REST/V1/Imagery/Map/Road/Bellevue%20Washington
    #?mapLayer=TrafficFlow&key=BingMapsKey
    #note that we are using the extraURL argument to pass any extra parameters:
    map4 = GetBingMap(center="Bellevue%20Washington", zoom=12, extraURL="&mapLayer=TrafficFlow", 
                      apiKey=apiKey,verbose=1, destfile="BellevueTraffic.png")
    PlotOnStaticMap(map4)
    
    #Get a map with Road imagery that displays a route.
    #This example gets a map with road imagery that displays a driving 
    #route between the cities of Seattle and Redmond in Washington State. 
    
    #note that we are using the extraURL argument to pass any extra parameters:
    #http://dev.virtualearth.net/REST/v1/Imagery/Map/Road/Routes
    #?wp.0=Seattle,WA;64;1&wp.1=Redmond,WA;66;2&key=BingMapsKey 
    map5 = GetBingMap(center="Bellevue%20Washington", zoom=8, 
                      extraURL="&Routes?wp.0=Seattle,WA;64;1&wp.1=Redmond,WA;66;2", 
                    apiKey=apiKey,verbose=1, destfile="Seattle2Redmond.png")
    PlotOnStaticMap(map5)
  }
})

