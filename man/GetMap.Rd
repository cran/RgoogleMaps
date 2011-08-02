\name{GetMap}
\alias{GetMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ download a static map from the Google server}
\description{
  Query the Google server for a static map tile, defined primarily by its center and zoom. Many additional arguments allow the user to customize the map tile.
 }
\usage{
GetMap(center, zoom = 12, markers, path = "", span, frame, hl, sensor = "true", 
  maptype = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")[4], 
  format = c("gif", "jpg", "jpg-baseline", "png8", "png32")[5], 
  size = c(640, 640), destfile = "MyTile.png", RETURNIMAGE = TRUE, GRAYSCALE =FALSE, verbose = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{center}{ lat/lon center of map }
  \item{zoom}{Google maps (integer) zoom level. Zoom levels between $0$ (the lowest zoom level, in which the entire world can be seen on one map) to $19$ (the highest zoom level, down to individual buildings) are possible within the normal maps view. }
  \item{markers}{ (optional) defines one or more markers to attach to the image at specified locations. This parameter takes a string of marker definitions separated by the pipe character (|) }
  \item{path}{ (optional) defines a single path of two or more connected points to overlay on the image at specified locations. This parameter takes a string of point definitions separated by the pipe character (|)}
  \item{span}{ (optional) defines a minimum viewport for the map image expressed as a latitude and longitude pair. The static map service takes this value and produces a map of the proper zoom level to include the entire provided span value from the map`s center point. Note that the resulting map may include larger bounds for either latitude or longitude depending on the rectangular dimensions of the map. If zoom is specified, span is ignored }
  \item{frame}{ (optional) specifies that the resulting image should be framed with a colored blue border. The frame consists of a 5 pixel, 55\% opacity blue border. }
  \item{hl}{ (optional) defines the language to use for display of labels on map tiles. Note that this paramater is only supported for some country tiles; if the specific language requested is not supported for the tile set, then the default language for that tile set will be used. }
  \item{sensor}{ specifies whether the application requesting the static map is using a sensor to determine the user`s location. This parameter is now required for all static map requests. }
  \item{maptype}{ defines the type of map to construct. There are several possible maptype values, including satellite, terrain, hybrid, and mobile. }
  \item{format}{ (optional) defines the format of the resulting image. By default, the Static Maps API creates GIF images. There are several possible formats including GIF, JPEG and PNG types. Which format you use depends on how you intend to present the image. JPEG typically provides greater compression, while GIF and PNG provide greater detail. This version supports only JPEG and PNG. }
  \item{size}{ desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels }
  \item{destfile}{ File to save the map image to. }
  \item{RETURNIMAGE}{ return image yes/no default: TRUE }
   \item{GRAYSCALE}{ Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY} }
  \item{verbose}{ level of verbosity }
}
\details{	The optional markers can be passed either as a string, such as 
	#\samp{GetMap(markers = '40.702147,-74.015794,blues\%7C40.711614,-74.012318,greeng\%7C40.718217,-73.998284,redc', destfile = "MyTile1.png")}
	\samp{MyMap <- GetMap(markers = '&markers=color:blue|label:S|40.702147,-74.015794&markers=color:green|label:G|40.711614,-74.012318&markers=color:red|color:red|label:C|40.718217,-73.998284', destfile = "MyTile1.png")} 
	#http://maps.google.com/maps/api/staticmap?size=320x320&maptype=roadmap&markers=color:blue|label:S|40.702147,-74.015794&markers=color:green|label:G|40.711614,-74.012318&markers=color:red|color:red|label:C|40.718217,-73.998284&sensor=false
	or as a dataframe:   
	\samp{GetMap(markers = cbind(lat = c(40.70214, 40.71161), lon = c(-74.01579, -73.9982), size = c('tiny','mid'), color = c('blue', 'red'), label = c('S','N')), destfile = "MyTile2.png");} }
\value{
  URL used to download the tile.
 }
\references{ \url{http://code.google.com/apis/maps/documentation/staticmaps/} }
\author{ Markus Loecher <markus.loecher@gmail.com> }
\note{ Note: The Google Static Maps API does NOT requires a Maps API key any longer !! You can but do not have to sign up for a free API key at \url{http://code.google.com/apis/maps/signup.html}.}
\seealso{ \link{GetMap.bbox} }
\examples{
	
  lat = c(40.702147,40.718217,40.711614);
  lon = c(-74.012318,-74.015794,-73.998284);
  center = c(mean(lat), mean(lon));
  zoom <- min(MaxZoom(range(lat), range(lon)));
  #this overhead is taken care of implicitly by GetMap.bbox();              
  \dontrun{MyMap <- GetMap(center=center, zoom=zoom,markers = '&markers=color:blue|label:S|40.702147,-74.015794&markers=color:green|label:G|40.711614,-74.012318&markers=color:red|color:red|label:C|40.718217,-73.998284', destfile = "MyTile1.png");}
  #Note that in the presence of markers one often needs to add some extra padding to the latitude range to accomodate the extent of the top most marker
  
  #add a path, i.e. polyline:
   \dontrun{MyMap <- GetMap(path = "&path=color:0x0000ff|weight:5|40.737102,-73.990318|40.749825,-73.987963|40.752946,-73.987384|40.755823,-73.986397", destfile = "MyTile3.png");}
   
   #The example below defines a polygonal area within Manhattan, passed a series of intersections as locations:
    # \dontrun{MyMap <- GetMap(path = "&path=color:0x00000000|weight:5|fillcolor:0xFFFF0033|8th+Avenue+%26+34th+St,New+York,NY|8th+Avenue+%26+42nd+St,New+York,NY|Park+Ave+%26+42nd+St,New+York,NY,NY|Park+Ave+%26+34th+St,New+York,NY,NY", destfile = "MyTile3a.png");}

  #note that since the path string is just appended to the URL you can "abuse" the path argument to pass anything to the query, e.g. the style parameter:
  #The following example displays a map of Brooklyn where local roads have been changed to bright green and the residential areas have been changed to black:
   \dontrun{ GetMap(center='Brooklyn', zoom=12, maptype = "roadmap", path = "&style=feature:road.local|element:geometry|hue:0x00ff00|saturation:100&style=feature:landscape|element:geometry|lightness:-100", sensor='false', destfile = "MyTile4.png",  RETURNIMAGE = FALSE);}
   
   #In the last example we set RETURNIMAGE to FALSE which is a useful feature in general if neither png nor ReadImages are installed. In those cases, the images can still be fetched and saved but not read into R.

  #In the following example we let the Static Maps API determine the correct center and zoom level implicitly, based on evaluation of the position of the markers. However, to be of use within R we do need to know the values for zoom and center explicitly, so it is better practice to compute them ourselves and pass them as arguments, in which case meta information on the map tile can be saved as well.
  
  \dontrun{MyMap <- GetMap(markers = '&markers=color:blue|label:S|40.702147,-74.015794&markers=color:green|label:G|40.711614,-74.012318&markers=color:red|color:red|label:C|40.718217,-73.998284', destfile = "MyTile1.png",  RETURNIMAGE = FALSE);}
  
  
}


