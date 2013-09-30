\name{bubbleMap}
\alias{bubbleMap}
\title{Create a bubble plot of spatial data on Google Maps}
\description{This function creates a bubble plot of spatial
data, with options for bicolour residual plots.}
\usage{bubbleMap(SP, map, filename = "", zcol = 1, max.radius = 100, 
    key.entries = quantile(SP@data[, zcol], (1:5)/5), do.sqrt = TRUE, 
    colPalette = NULL, strokeColor = "#FFAA00", alpha = 0.7, 
    strokeWeight = 1, LEGEND = TRUE, verbose = 0)}
\arguments{
  \item{SP}{object of \link[sp]{SpatialPointsDataFrame-class} with associated coordinate reference systems}
  \item{map}{map object}
  \item{filename}{filename to save the map under, IF map object not given}
  \item{zcol}{variable column name, or column number after removing spatial coordinates from x@data: 1 refers to the first non-coordinate column}
  \item{max.radius}{value for largest circle (the plotting symbols) in metre, circumcircle of triange or quadrangle (square)}
  \item{key.entries}{value for largest circle (the plotting symbols) in metre, circumcircle of triange or quadrangle (square)}
  \item{do.sqrt}{logical; if TRUE the plotting symbol area (sqrt(diameter)) is proportional to the value of the z-variable; if FALSE, the symbol size (diameter) is proportional to the z-variable}
  \item{colPalette}{colours to be used to fill plotting symbols; numeric vector of same size like key.entries

colours to be used to fill features depending on attribute}
  \item{strokeColor}{the color to draw the border of circle (the plotting symbols)}
  \item{alpha}{the fill opacity between 0.0 and 1.0}
  \item{strokeWeight}{the stroke width in pixels}
  \item{LEGEND}{logical; if TRUE add bubbleLegend}
  \item{verbose}{level of verbosity}
}

\value{##################################################################
map structure or URL used to download the tile.}

\author{"Markus Loecher, Berlin School of Economics and Law (BSEL)"
<markus.loecher@gmail.com>}




\examples{
library(sp)
data("meuse", package = "sp", envir = environment())
m<-bubbleMap(meuse,zcol='zinc');
}
