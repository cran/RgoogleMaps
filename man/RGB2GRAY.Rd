\name{RGB2GRAY}
\alias{RGB2GRAY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 
	translates an RGB image matrix to gray scale }
\description{
  This function translates the rgb values of the array myTile into a scalar matrix with just one gray value per pixel.
}
\usage{
RGB2GRAY(myTile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{myTile}{ rgb image matrix, usually array with 3 dimensions }
}
\details{
	Gray scale intensity defined as  0.30R + 0.59G + 0.11B
 }
\value{
  gray scale image matrix
}
\references{  }
\author{ Markus Loecher }
\note{ }
\seealso{ }
\examples{

     }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
