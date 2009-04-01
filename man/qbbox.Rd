\name{qbbox}
\alias{qbbox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes bounding box  }
\description{
  The function qbbox computes a bounding box for the given lat,lon 
points with a few additional options such as quantile boxes, additional margins, etc.
}
\usage{
qbbox(lat, lon, TYPE = c("all", "quantile")[1], 
      margin = list( m = c(1, 1, 1, 1), TYPE = c("perc", "abs")[1] ), 
      q.lat = c(0.1, 0.9), q.lon = c(0.1, 0.9), verbose = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{ latitude values }
  \item{lon}{ longitude values}
  \item{TYPE}{ if 'ALL' return just the ranges, if 'quantile' return the quantiles specified by \code{q.lat} and \code{q.lon} }
  \item{margin}{ list of parameters describing any desired extra margin: \emph{m}: margins for the four sides; \emph{TYPE}: percentage or absolute margin ? }
  \item{q.lat}{ lower and upper quantile for the longitude range }
  \item{q.lon}{ lower and upper quantile for the latitude range }
  \item{verbose}{ level of verbosity }
}
\details{
 
}
\value{
  
  \item{latR }{latitude range}
  \item{lonR }{longitude range}
 
}
\references{  }
\author{Markus Loecher, Sense Networks <markus@sensenetworks.com> }
\note{ 
}
\seealso{ }
\examples{
  lat = 37.85 + rnorm(100, sd=0.001);
  lon = -120.47 + rnorm(100, sd=0.001);
  #add a few outliers:
  lat[1:5] <- lat[1:5] + rnorm(5, sd =.01);
  lon[1:5] <- lon[1:5] + rnorm(5, sd =.01);
  
  #range, discarding the upper and lower 10% of the data
  qbbox(lat, lon, TYPE = "quantile");
  #full range:
  qbbox(lat, lon, TYPE = "all");
  #add a 10% extra margin on all four sides:
  qbbox(lat, lon, margin = list(m = c(10, 10, 10, 10), TYPE = c("perc", "abs")[1]));
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

