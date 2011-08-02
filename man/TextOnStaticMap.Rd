\name{TextOnStaticMap}
\alias{TextOnStaticMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ plots text on map}
\description{
  TextOnStaticMap draws the strings given in the vector labels at the coordinates given by x and y on a map. y may be missing since xy.coords(x,y) is used for construction of the coordinates.
}
\usage{
TextOnStaticMap(MyMap, lat, lon, labels = seq_along(lat), TrueProj = TRUE, FUN = text, add = FALSE, verbose = 1,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MyMap}{ map image returned from e.g. \code{GetMap()} }
  \item{lat}{latitude where to put text.}
  \item{lon}{longitude where to put text.}
  \item{labels}{a character vector or \link{expression} specifying the text to be written. An attempt is made to coerce other language objects (names and calls) to expressions, and vectors and other classed objects to character vectors by \link{as.character}. If labels is longer than x and y, the coordinates are recycled to the length of labels.
}
  \item{TrueProj}{set to FALSE if you are willing to accept some degree of inaccuracy in the mapping. In that case, the coordinates of the image are in lat/lon and the user can simply overly points/lines/axis without worrying about projections}
  \item{FUN}{overlay function, typical choice would be \link{text} }
  \item{add}{ add to existing map ? }
  \item{verbose}{ level of verbosity }
  \item{\dots}{further arguments passed to \link{text} }
}
\details{
  }
\value{
  }
\references{ }
\author{ Markus Loecher <markus.loecher@gmail.com> }
\note{ }
\seealso{ \link{PlotOnStaticMap} \link{text} }
\examples{

  lat = c(40.702147,40.718217,40.711614);
  lon = c(-74.012318,-74.015794,-73.998284);
  center = c(mean(lat), mean(lon));
  zoom <- min(MaxZoom(range(lat), range(lon)));
  
  MyMap <- GetMap(center=center, zoom=zoom,markers = '&markers=color:blue|label:S|40.702147,-74.015794&markers=color:green|label:G|40.711614,-74.012318&markers=color:red|color:red|label:C|40.718217,-73.998284', destfile = "MyTile1.png");
 
  TextOnStaticMap(MyMap, lat=40.711614,lon=-74.012318, "Some Text", cex=2, col = 'red')
  
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

