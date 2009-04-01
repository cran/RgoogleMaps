\name{GetMap}
\alias{GetMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ download a static map from the Google server}
\description{
  Query the Google server for a static map tile, defined primarily by its center and zoom. Many additional arguments allow the user to customize the map tile.
 }
\usage{
GetMap(key, center, zoom = 12, markers, path, span, frame, hl, sensor = "true", 
  maptype = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")[4], 
  format = c("gif", "jpg", "jpg-baseline", "png8", "png32")[5], 
  size = c(640, 640), destfile = "MyTile.png", verbose = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{key}{ Google maps API key; NOTE: if missing the function trys to read the key from the file 'API.key.txt' in the user`s home directory. }
  \item{center}{ lat/lon center of map }
  \item{zoom}{Google maps (integer) zoom level. Zoom levels between $0$ (the lowest zoom level, in which the entire world can be seen on one map) to $19$ (the highest zoom level, down to individual buildings) are possible within the normal maps view. }
  \item{markers}{ (optional) defines one or more markers to attach to the image at specified locations. This parameter takes a string of marker definitions separated by the pipe character (|) }
  \item{path}{ (optional) defines a single path of two or more connected points to overlay on the image at specified locations. This parameter takes a string of point definitions separated by the pipe character (|)}
  \item{span}{ (optional) defines a minimum viewport for the map image expressed as a latitude and longitude pair. The static map service takes this value and produces a map of the proper zoom level to include the entire provided span value from the map`s center point. Note that the resulting map may include larger bounds for either latitude or longitude depending on the rectangular dimensions of the map. If zoom is specified, span is ignored }
  \item{frame}{ (optional) specifies that the resulting image should be framed with a colored blue border. The frame consists of a 5 pixel, 55\% opacity blue border. }
  \item{hl}{ (optional) defines the language to use for display of labels on map tiles. Note that this paramater is only supported for some country tiles; if the specific language requested is not supported for the tile set, then the default language for that tile set will be used. }
  \item{sensor}{ specifies whether the application requesting the static map is using a sensor to determine the user`s location. This parameter is now required for all static map requests. }
  \item{maptype}{ defines the type of map to construct. There are several possible maptype values, including satellite, terrain, hybrid, and mobile. }
  \item{format}{ (optional) defines the format of the resulting image. By default, the Static Maps API creates GIF images. There are several possible formats including GIF, JPEG and PNG types. Which format you use depends on how you intend to present the image. JPEG typically provides greater compression, while GIF and PNG provide greater detail. This version supports only JPEG. }
  \item{size}{ desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels }
  \item{destfile}{ File to save the map image to. }
  \item{verbose}{ level of verbosity }
}
\details{	The optional markers can be passed either as a string, such as 
	\samp{GetMap(markers = '40.702147,-74.015794,blues\%7C40.711614,-74.012318,greeng\%7C40.718217,-73.998284,redc', destfile = "MyTile1.png")} 
	or as a dataframe:   
	\samp{GetMap(markers = cbind.data.frame(lat = c(40.70214, 40.71161), lon = c(-74.01579, -73.9982), size = c('tiny','mid'), col = c('blue', 'red'), char = c('','n')), destfile = "MyTile2.png");} }
\value{
  URL used to download the tile.
 }
\references{ \url{http://code.google.com/apis/maps/documentation/staticmaps/} }
\author{ Markus Loecher, Sense Networks <markus@sensenetworks.com> }
\note{ Note: The Google Static Maps API requires a Maps API key. If you haven`t already done so, sign up for a free API key at \url{http://code.google.com/apis/maps/signup.html}. Future versions will include the capability to read png tiles using the rgdal package. In fact,the code is already built in but commented out. }
\seealso{ \link{GetMap.bbox} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

