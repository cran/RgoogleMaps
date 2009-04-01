\name{LatLon2XY.centered}
\alias{LatLon2XY.centered}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes the centered coordinate transformation from lat/lon to map tile coordinates }
\description{
  The function LatLon2XY.centered(MyMap, lat,lon,zoom) computes the coordinate transformation from lat/lon to map tile coordinates given a map object.
}
\usage{
LatLon2XY.centered(MyMap, lat, lon, zoom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MyMap}{ map object }
  \item{lat}{latitude values to transform }
  \item{lon}{ longitude values to transform }
  \item{zoom}{ optional zoom level. If missing, taken from \code{MyMap} }
}
\details{
 }
\value{
  properly scaled and centered (with respect to the center of \code{MyMap} ) coordinates  
  \item{newX }{ transformed longitude}
  \item{newY }{transformed latitude}

}
\references{ }
\author{ Markus Loecher, Sense Networks <markus@sensenetworks.com> }
\note{
}
\seealso{ \link{LatLon2XY} \link{Tile2R} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

