bubbleMap <- structure(function#Create a bubble plot of spatial data on Google Maps
### This function creates a bubble plot of spatial
### data, with options for bicolour residual plots.
(
  SP, ##<< object of \link[sp]{SpatialPointsDataFrame-class} with associated coordinate reference systems
  map, ##<< map object
  filename = "", ##<< filename to save the map under, IF map object not given
  zcol = 1, ##<< variable column name, or column number after removing spatial coordinates from x@data: 1 refers to the first non-coordinate column
  max.radius = 100, ##<< value for largest circle (the plotting symbols) in metre, circumcircle of triange or quadrangle (square)
  key.entries = quantile(SP@data[,zcol], (1:5)/5), ##<< value for largest circle (the plotting symbols) in metre, circumcircle of triange or quadrangle (square)
  do.sqrt = TRUE,##<< logical; if TRUE the plotting symbol area (sqrt(diameter)) is proportional to the value of the z-variable; if FALSE, the symbol size (diameter) is proportional to the z-variable
#  add = FALSE, ##<< logical; if TRUE the result of the function will be a list stored as variable  in  R. It is possible to combine more layers in the one plot, previously saved output from plotGoogleMaps should be given in the previousMap attribute.
#  previousMap = NULL,##<<  
  colPalette = NULL, ##<< colours to be used to fill plotting symbols; numeric vector of same size like key.entries
  strokeColor = "#FFAA00", ##<< the color to draw the border of circle (the plotting symbols)
  alpha = 0.7, ##<< the fill opacity between 0.0 and 1.0
  strokeWeight = 1, ##<< the stroke width in pixels
  LEGEND = TRUE, ##<< logical; if TRUE add bubbleLegend
  verbose =0 ##<< level of verbosity
){
  ####################################################################
  PolyCol <-   function#Create list of colors depending on attribute data. (for bubbleMap)
  (attribute, ##<< vector of attribute data
   colPalette=NULL, ##<< colours to be used to fill features depending on attribute
   at ##<< values at which colours will change
  ) {
    # attribute=soil.ll@data$ID
    pal<-colorRampPalette(c( "green", "orange","brown"), space = "Lab")
    
    if(!is.numeric(attribute)){ attribute<-as.factor( attribute)}
    
    if(length(colPalette)==1) {
      x<- rep(colPalette,length(attribute))
      col.data<-list(cols=as.character(substr(x,1,7)),col.uniq=colPalette, att=ifelse(!is.factor(attribute),paste("[",min(attribute)," , ",max(attribute),"]",sep=""), " "))
      return(col.data) }
    
    if(is.null(colPalette) ){
      colPalette<-pal(min(10,length(attribute) ) ) }else{ xx<-colPalette<-as.character(substr(colPalette,1,7)) }
    
    if(is.factor(attribute)){
      
      if(length(colPalette)!=nlevels(attribute)) {
        xx<-colPalette<- as.character(substr(pal(nlevels(attribute)),1,7))    }
      
      x<-factor(attribute,labels=colPalette)
      col.data<-list(cols=as.character(substr(x,1,7)),col.uniq=colPalette, att=levels(attribute) )
      return(col.data)
      
    }else{
      bre<-quantile(attribute, seq(1,length(colPalette))/length(colPalette))
      breakss<-factor(c(min(attribute),bre))
      break_unique<-as.numeric(levels(breakss))
      
      if(length(colPalette)>=length(break_unique)){
        colPalette<-colPalette[1:length(break_unique)] } else{
          colPalette<- as.character(substr(colPalette[1:length(break_unique)-1],1,7))}
      
      atr<-cut(attribute, break_unique ,include.lowest = TRUE, dig.lab=6)                                 
      x<-factor(atr,labels=colPalette[1:(length(break_unique)-1)] )
      col.data<-list(cols=as.character(substr(x,1,7)),col.uniq=colPalette, att=levels(atr) )
      return(col.data)                               
      
    }
    
    ###The function provide list of colors (cols), unique colors (col.uniq), levels of attribute (att),attribute breaks (brks).   
  }
    require(rgdal)
    if (class(SP) == "data.frame") {
      SP = DF2SpatialPointsDataFrame(SP, coords=c("x", "y"))
    } 
    stopifnot(class(SP) == "SpatialPointsDataFrame")
    obj = as(SP, "SpatialPointsDataFrame")
    data = obj@data
    if (NCOL(data) == 1) {
        z = data
    }
    else {
        z = data[, zcol]
    }
    if (min(key.entries) < 0) {
        ke <- abs(min(key.entries)) + key.entries + mean(key.entries)
    }
    else {
        ke <- key.entries + mean(key.entries)
    }
    if (do.sqrt) {
        scale.level <- sqrt(ke/(max(ke)))
    }
    else {
        scale.level <- ke/(max(ke))
    }
    radius.level <- max.radius * scale.level
    breakss <- factor(c(min(z), key.entries))
    break_unique <- as.numeric(levels(breakss))
    if (length(unique(z)) == length(key.entries)) {
        zz = factor(z, labels = radius.level)
        radius.vector <- floor(as.numeric(as.vector(zz)))
    }
    else {
        zz = factor(cut(z, break_unique, include.lowest = TRUE), 
            labels = radius.level)
        radius.vector <- floor(as.numeric(as.vector((zz))))
    }
    SP.ll <- spTransform(SP, CRS("+proj=longlat +datum=WGS84"))
    Centar = c(mean(SP.ll@bbox[1, ]), mean(SP.ll@bbox[2, ]))
    sw <- c(SP.ll@bbox[2, 1], SP.ll@bbox[1, 1])
    ne <- c(SP.ll@bbox[2, 2], SP.ll@bbox[1, 2])
    nameOfSP <- sapply(as.list(substitute({
        SP
    })[-1]), deparse)
    nameOfSP <- gsub("[!,\",#,$,%,&,(,),*,+,-,.,/,:,;,<,=,>,?,@,^,`,|,~]", 
        "_", nameOfSP)
    nameOfSP <- gsub("[[]", "_", nameOfSP)
    nameOfSP <- gsub("[]]", "_", nameOfSP)
    if (filename == "") {
        filename <- paste(nameOfSP, ".png", sep = "")
    }
    attribute = SP@data[, zcol]
#     polyName <- paste("poly", nameOfSP, sep = "")
#     boxname <- paste(nameOfSP, "box", sep = "")
#     textname <- paste(nameOfSP, "text", sep = "")
#     divLegendImage <- tempfile("Legend")
#     divLegendImage <- substr(divLegendImage, start = regexpr("Legend", 
#         divLegendImage), stop = nchar(divLegendImage))
#     legendboxname <- paste("box", divLegendImage, sep = "")
#     textnameW <- paste(textname, "W", sep = "")
    
    if (strokeColor != "") {
        rgbc <- col2rgb(strokeColor)
        strokeColor <- rgb(rgbc[1], rgbc[2], rgbc[3], maxColorValue = 255)
    }
    if (!is.null(colPalette)) {
        rgbc <- col2rgb(colPalette)
        colPalette <- apply(rgbc, 2, function(x) rgb(x[1], x[2], 
            x[3], maxColorValue = 255))
    }
    for (i in 1:length(SP.ll@data)) {
        if (identical(attribute, SP.ll@data[, i])) {
            attributeName <- names(SP.ll@data)[i]
        }
    }
    att <- rep(NA, length(SP.ll@coords[, 1]))
    att1 = ""
    

    cxx <- PolyCol(factor(zz, labels = key.entries), colPalette)
    plotclr <- cxx$cols
    plotclr = AddAlpha(plotclr,alpha)
    
    bb <- qbbox(lat = SP.ll@coords[, 2], lon = SP.ll@coords[, 1]);
    if (verbose>1) browser()
    ##download the map:
    if (missing(map))
      map <- GetMap.bbox(bb$lonR, bb$latR, destfile = filename, maptype="mobile", SCALE = 2);
    PlotOnStaticMap(map, lat = SP.ll@coords[, 2], lon = SP.ll@coords[, 1], 
                    col = plotclr, cex = 3*radius.vector/max(radius.vector, na.rm=TRUE), pch = 20)
    if (LEGEND) {
      CEX = sqrt(as.numeric(key.entries))
      CEX = 4*CEX/max(CEX)
      cxx2 <- PolyCol(factor(CEX, labels = key.entries), colPalette)
      legend("topright", pt.cex=CEX, col=cxx2$cols, pch=20, legend=as.character(key.entries))
    }
invisible(map)
#####################################################################
### map structure or URL used to download the tile.
}, ex = function(){
  library(sp)
  data("meuse", package = "sp", envir = environment())
  m<-bubbleMap(meuse,zcol='zinc');
})


