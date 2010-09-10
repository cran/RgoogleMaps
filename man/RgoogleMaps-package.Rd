\name{RgoogleMaps-package}
\alias{RgoogleMaps-package}
\alias{RgoogleMaps}
\docType{package}
\title{
Utilities to enable overlays on Google Maps
}
\description{
This package serves two purposes: (i) Provide a comfortable R interface to query the Google server for static maps, and (ii) Use the map as a background image to overlay plots within R. This requires proper coordinate scaling.
}
\details{
\tabular{ll}{
Package: \tab RgoogleMaps\cr
Type: \tab Package\cr
Version: \tab 1.1.9\cr
Date: \tab 2010-09-10\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
The two main functions in the package are \code{GetMap}, which queries the Google server for a map, and \code{PlotOnStaticMap}, which enables the actual overlay plots in R.

The examples below give a bit of support code for using the functions in
this package. Details on using the functions themselves can be found in
the documentation for those functions.

NOTE: To do anything but downloading static map tiles, RgoogleMaps needs EITHER rgdal OR ReadImages installed ! Such an OR dependency is difficult to express in the 'Depends' field, so I moved both packages to suggested. rgdal is your package if you prefer png file format and ReadImages/rimage if you prefer jpg format. In the latter cases, you will also need the libjpeg library installed.

}
\author{
Markus Loecher, Sense Networks <markus@sensenetworks.com> 
}
\keyword{ package }

\examples{
#This section contains examples that will execute once you obtain an API key:
	
#The first step naturally will be to download a static map from the Google server. A simple example:

  \dontrun{MyMap <- GetMap(markers = '40.702147,-74.015794,blues\%7C40.711614,-74.012318,greeng\%7C40.718217,-73.998284,redc', destfile = "MyTile1.png");}
                 
   \dontrun{tmp <- PlotOnStaticMap(MyMap,lat = c(40.702147,40.711614,40.718217), lon = c(-74.015794,-74.012318,-73.998284), cex=1.5,pch=20,col=c('red', 'blue', 'green'), add=F)}
#Of course, one can do without the markers and/or just pass a bounding box:

  \dontrun{MyMap <- GetMap(center=c(40.714728,-73.99867), zoom =14, destfile = "MyTile2.png",maptype = "mobile");}
  
#The function qbbox() basically computes a bounding box for the given lat,lon points with a few additional options such as quantile boxes, additional buffers, etc.  

  \dontrun{bb <- qbbox(c(40.702147,40.711614,40.718217),c(-74.015794,-74.012318,-73.998284), 
            TYPE = "all", margin = list(m=rep(5,4), TYPE = c("perc", "abs")[1]));}
            
      \dontrun{MyMap <- GetMap.bbox(bb$lonR, bb$latR,destfile = "MyTile3.png", maptype = "satellite") }
  
#The main function that overlays points (can easily be extended to lines or any other object) is PlotOnStaticMap().
#The following simple sequence of calls serves to test the coincidence of the markers placed by Google and the corresponding points overlaid by PlotOnStaticMap(). Note that we convert the map to grayscale first to make the colored points more discernible.

#Define the markers:

  \dontrun{mymarkers <- cbind.data.frame(lat = c(38.898648,38.889112, 38.880940), 
          lon = c(-77.037692, -77.050273, -77.03660), size =  c('tiny','tiny','tiny'), 
          col = c('blue', 'green', 'red'), char = c('','',''));}

#get the bounding box:

  \dontrun{bb <- qbbox(lat = mymarkers[,"lat"], lon = mymarkers[,"lon"])}
  
#download the map:

  \dontrun{MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "DC.png", GRAYSCALE =T,
                markers = mymarkers);}
                
#determine the max zoon, so that all points fit on the plot (not necessary in this case):  

   \dontrun{zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR)));}
   
#plot:
 
 \dontrun{png("OverlayTest.png",640,640);}
  
  \dontrun{tmp <- PlotOnStaticMap(MyMap,lat = mymarkers[,"lat"], lon = mymarkers[,"lon"], 
         cex=1.5,pch=20,col=c('blue', 'green', 'red'), add=F);}
         
  \dontrun{tmp <- PlotOnStaticMap(MyMap,lat = mymarkers[,"lat"], lon = mymarkers[,"lon"], 
       col=c('purple'), add=T, FUN = lines, lwd = 2)}
       
  \dontrun{dev.off()}
                   
}
