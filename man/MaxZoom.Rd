\name{MaxZoom}
\alias{MaxZoom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes the maximum zoom level which will contain the given lat/lon range}
\description{
	computes the maximum zoom level which will contain the given lat/lon range
  }
\usage{
MaxZoom(latrange, lonrange, size = c(640, 640))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{latrange}{ range of latitude values }
  \item{lonrange}{ range of longitude values }
  \item{size}{ size of the map tile, defaults to 640x640 }
}

\value{
  zoom level
}

\author{ Markus Loecher, Sense Networks <markus@sensenetworks.com> }

