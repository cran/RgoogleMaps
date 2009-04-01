\name{GetMap.bbox}
\alias{GetMap.bbox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ download a static map from the Google server }
\description{
  Wrapper function for \link{GetMap}. Query the Google server for a static map tile, defined primarily by its lat/lon range and/or center and/or zoom. Multiple additional arguments allow the user to customize the map tile.
}
\usage{
GetMap.bbox(lonR, latR, center, size = c(640, 640), destfile = "MyTile.png", 
          MINIMUMSIZE = FALSE, RETURNIMAGE = TRUE, GRAYSCALE = FALSE, 
          NEWMAP = TRUE, zoom, verbose = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lonR}{ longitude range }
  \item{latR}{ latitude range  }
  \item{center}{ optional center }
  \item{size}{ desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels }
  \item{destfile}{ File to load the map image from or save to, depending on NEWMAP. }
  \item{MINIMUMSIZE}{ reduce the size of the map to its minimum size that still fits the lat/lon ranges ? default: FALSE }
  \item{RETURNIMAGE}{ return image yes/no default: TRUE }
  \item{GRAYSCALE}{ Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY} }
  \item{NEWMAP}{ if TRUE, query the Google server and save to \code{destfile}, if FALSE load from destfile. }
  \item{zoom}{Google maps zoom level. optional }
  \item{verbose}{level of verbosity  }
  \item{\dots}{ extra arguments to \link{GetMap} }
}
\details{
  
}
\value{
 map tile
 }
\references{  }
\author{ Markus Loecher, Sense Networks <markus@sensenetworks.com> }
\note{ 
}
\seealso{ \link{GetMap} }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
