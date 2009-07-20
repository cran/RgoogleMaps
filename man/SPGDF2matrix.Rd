\name{SPGDF2matrix}
\alias{SPGDF2matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ image gridded spatial data, or convert to format for image }
\description{ This is a modification/ripoff from as.image.SpatialGridDataFrame() in package sp.
    It merely serves to convert an object of type SpatialGridDataFrame to a matrix}
\usage{
SPGDF2matrix(x, xcol = 1, ycol = 2, col = heat.colors(12), red = 1, green = 2, blue = 3, axes = FALSE, xlim = NULL, ylim = NULL, add = FALSE, PLOT = FALSE, ..., asp = NA, setParUsrBB = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class \link[sp]{SpatialGridDataFrame} }

\item{xcol}{ column number of x-coordinate, in the coordinate matrix }
\item{ycol}{ column number of y-coordinate, in the coordinate matrix }
\item{col}{a vector of colors }
\item{red,green,blue}{columns names or numbers given instead of the \code{attr} argument when the data represent an image encoded in three colour bands on the 0-255 integer scale; all three columns must be given in this case, and the attribute values will be constructed using function \code{rgb} }
%\item{asp}{ aspect ratio of unit x and unit y axis }
\item{axes}{ logical; should coordinate axes be drawn? }
\item{xlim}{ x-axis limits }
\item{ylim}{ y-axis limits }
\item{add}{ logical; if FALSE, the image is added to the plot layout setup
by \code{plot(as(x, "Spatial"),axes=axes,xlim=xlim,ylim=ylim,asp=asp)}
which sets up axes and plotting region; if TRUE, the image is added
to the existing plot. }
\item{PLOT}{ plot, yes/no?}
\item{...}{ arguments passed to \link{image}, see examples }
\item{asp}{ aspect ratio to be used for plot }
\item{setParUsrBB}{default FALSE, see \code{\link[sp]{Spatial-class}} for further details}

}
\details{ }
\value{  }
\references{   }
\author{  Markus Loecher, Sense Networks <markus@sensenetworks.com>  }
\note{ }
\seealso{ }
\examples{ }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
