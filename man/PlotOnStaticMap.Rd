\name{PlotOnStaticMap}
\alias{PlotOnStaticMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ overlays plot on background image of map tile }
\description{
  This function is the workhorse of the package RgoogleMaps. It }
\usage{
PlotOnStaticMap(MyMap, lat, lon, destfile, zoom = NULL, size = c(640, 640), 
                GRAYSCALE = FALSE, add = FALSE, FUN = points, verbose = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MyMap}{ optional map object to be passed }
  \item{lat}{ vector latitude values to be overlaid  }
  \item{lon}{ vector of longitude values to be overlaid }
  \item{destfile}{ File to load the map image from or save to, depending on whether \code{MyMap} was passed. }
  \item{zoom}{ Google maps zoom level. optional if \code{MyMap} is passed, required if not.}
  \item{size}{ desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is $640x640$ pixels.}
  \item{GRAYSCALE}{ Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY} }
  \item{add}{ start a new plot or add to an existing }
  \item{FUN}{ plotting function to use for overlay; typical choices would be \link{points} and \link{lines} }
  \item{verbose}{ level of verbosity }
  \item{\dots}{ further arguments to be passed to \code{FUN} }
}
\details{
  
}
\value{
  the map object is returned via \code{invisible(MyMap)}
 }
\references{ }
\author{ Markus Loecher, Sense Networks <markus@sensenetworks.com> }
\note{ Credit for the implementation of the png file reading goes to both Gabor Grothendieck (\url{http://www.mail-archive.com/r-sig-geo@stat.math.ethz.ch/msg04640.html}) and Michael Sumner (\url{http://finzi.psych.upenn.edu/R/Rhelp02a/archive/94605.html}).
	Credit for the implementation of the jpg file reading goes to Greg Snow: \url{https://stat.ethz.ch/pipermail/r-sig-geo/2009-March/005229.html}
}
\seealso{ \link{GetMap} \link{GetMap.bbox} }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
