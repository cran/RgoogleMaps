\name{LatLon2XY}
\alias{LatLon2XY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes the coordinate transformation from lat/lon to map tile coordinates }
\description{
  The function LatLon2XY(lat,lon,zoom) computes the coordinate transformation from lat/lon to map tile coordinates given a zoom level. It returns the tile coordinates as well as the pixel coordinates within the Tile itself. Thanks to Neil Young (see \url{http://groups.google.com/group/Google-Maps-API/browse_thread/thread/d2103ac29e95696f?hl=en}) for providing the formulae used.
}
\usage{
LatLon2XY(lat, lon, zoom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{ vector of latitude values}
  \item{lon}{ vector of longitude values }
  \item{zoom}{integer zoom level for Google Maps}
}
\details{
   
}
\value{
  A list with values
  \item{Tile}{integer numbers specifying the tile}
  \item{Coords}{pixel coordinate within the Tile}
  
}
\references{  }
\author{ Markus Loecher, Sense Networks <markus@sensenetworks.com> }
\note{ 
}
\seealso{ \link{LatLon2XY.centered} }
\examples{
	LatLon2XY(38.45, -122.375, 11)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
