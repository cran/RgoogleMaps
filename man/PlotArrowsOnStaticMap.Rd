\name{PlotArrowsOnStaticMap}
\alias{PlotArrowsOnStaticMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ plots arrows or segments on map}
\description{
  This function plots/overlays arrows or segments on a map.
}
\usage{
PlotArrowsOnStaticMap(MyMap, lat0, lon0, lat1 = lat0, lon1 = lon0, TrueProj = TRUE, FUN = arrows, add = FALSE, verbose = 1,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MyMap}{ map image returned from e.g. \code{GetMap()} }
  \item{lat0, lon0}{coordinates of points from which to draw.}
  \item{lat1, lon1}{coordinates of points to which to draw.}
  \item{TrueProj}{set to FALSE if you are willing to accept some degree of inaccuracy in the mapping. In that case, the coordinates of the image are in lat/lon and the user can simply overly points/lines/axis without worrying about projections}
  \item{FUN}{ plotting function to use for overlay; typical choices would be \link{arrows} and \link{segments} }
  \item{add}{ add to existing map ? }
  \item{verbose}{ level of verbosity }
  \item{\dots}{further arguments passed to \link{arrows} or \link{segments}}
}
\details{
  }
\value{
  }
\references{ }
\author{ Markus Loecher, Sense Networks <markus@sensenetworks.com> }
\note{ }
\seealso{ \link{PlotOnStaticMap} \link{mypolygon} }
\examples{

 \dontrun{
  	
  require(PBSmapping);
  shpFile <- paste(system.file(package = "RgoogleMaps"), "/shapes/bg11_d00.shp", sep = "")
  #shpFile <- system.file('bg11_d00.shp', package = "RgoogleMaps");
  
  shp=importShapefile(shpFile,projection="LL");
  bb <- qbbox(lat = shp[,"Y"], lon = shp[,"X"]);
  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "DC.jpg");
  PlotPolysOnStaticMap(MyMap, shp, lwd=.5, col = rgb(0.25,0.25,0.25,0.025), add = F);

  #North Carolina SIDS data set:
  shpFile <- system.file("shapes/sids.shp", package="maptools");
  shp=importShapefile(shpFile,projection="LL");
  bb <- qbbox(lat = shp[,"Y"], lon = shp[,"X"]);
  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "SIDS.jpg");
  #compute regularized SID rate
  sid <- 100*attr(shp, "PolyData")$SID74/(attr(shp, "PolyData")$BIR74+500)
  b <- as.integer(cut(sid, quantile(sid, seq(0,1,length=8)) ));
  b[is.na(b)] <- 1;
  opal <- col2rgb(grey.colors(7), alpha=TRUE)/255; opal["alpha",] <- 0.2;
  shp[,"col"] <- rgb(0.1,0.1,0.1,0.2);
  for (i in 1:length(b)) 
    shp[shp[,"PID"] == i,"col"] <- rgb(opal[1,b[i]],opal[2,b[i]],opal[3,b[i]],opal[4,b[i]]);
  PlotPolysOnStaticMap(MyMap, shp, lwd=.5, col = shp[,"col"], add = F);
  
  #compare the accuracy of this plot to a Google Map overlay:
  library(maptools);
  qk <- SpatialPointsDataFrame(as.data.frame(shp[, c("X","Y")]), as.data.frame(shp[, c("X","Y")]))
  proj4string(qk) <- CRS("+proj=longlat");
  tf <- "NC.counties";
  SGqk <- GE_SpatialGrid(qk)
  png(file=paste(tf, ".png", sep=""), width=SGqk$width, height=SGqk$height,
  bg="transparent")
  par(mar=c(0,0,0,0), xaxs="i", yaxs="i");par(mai = rep(0,4))
  plotPolys(shp, plt=NULL)
  dev.off()
  kmlOverlay(SGqk, paste(tf, ".kml", sep=""), paste(tf, ".png", sep=""));
  #This kml file can now be inspected in Google Earth or Google Maps
  
  #or choose an aspect ratio that corresponds better to North Carolina's elongated shape:
  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "SIDS.jpg", size = c(640, 320), zoom = 7);
  PlotPolysOnStaticMap(MyMap, shp, lwd=.5, col = shp[,"col"], add = F);
  
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

