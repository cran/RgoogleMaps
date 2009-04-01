\name{MapBackground}
\alias{MapBackground}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ get static Map from the Google server }
\description{
  get static Map from the Google server
}
\usage{
   MapBackground(lat, lon, destfile, NEWMAP = TRUE, myTile,
    zoom = NULL, size = c(640, 640), GRAYSCALE = FALSE, verbose = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{lat}{ vector latitude values for the map to contain  }
  \item{lon}{ vector of longitude values for the map to contain }
  \item{destfile}{ File to load the map image from or save to, depending on the value of NEWMAP. }
  \item{NEWMAP}{ get new map from teh Google server or load it from existing file }
  \item{myTile}{ map tile from previous downloads }
  \item{zoom}{ Google maps zoom level. optional if MyMap is passed, required if not.}
  \item{size}{ desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels.}
  \item{GRAYSCALE}{ Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY} }
  \item{verbose}{ level of verbosity }
  \item{\dots}{ further arguments to be passed to \link{GetMap.bbox} }
}
\details{
  
}
\value{
 list containing the map tile
 }
\references{ }
\author{Markus Loecher, Sense Networks <markus@sensenetworks.com> }
\note{ }
\seealso{ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

