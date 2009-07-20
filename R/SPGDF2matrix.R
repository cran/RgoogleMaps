SPGDF2matrix <- function (x, xcol = 1, ycol = 2, col = heat.colors(12), 
    red = 1, green = 2, blue = 3, axes = FALSE, xlim = NULL, 
    ylim = NULL, add = FALSE, PLOT = FALSE, ..., asp = NA, setParUsrBB = FALSE) 
{
    #This is a modification/ripoff from as.image.SpatialGridDataFrame() in package sp.
    #It merely serves to convert an object of type SpatialGridDataFrame to a matrix
        if (is.null(green) || is.null(blue)) 
            stop("all colour bands must be given")
        xd <- x@data[, c(red, green, blue)]
        NAs <- is.na(xd[, 1]) | is.na(xd[, 2]) | is.na(xd[, 3])
        if (any(NAs)) 
            xd <- xd[!NAs, ]
        RGBs <- rgb(xd, max = 255)
        if (any(NAs)) {
            z <- rep(NA, length(NAs))
            z[!NAs] <- RGBs
            RGBs <- z
        }
        fcols <- factor(RGBs)
        #cv <- coordinatevalues(getGridTopology(x))
        m <- matrix(as.integer(fcols), x@grid@cells.dim[1], x@grid@cells.dim[2], 
            byrow = FALSE)
        res <- m[, ncol(m):1];
        attr(res, "COL") <- levels(fcols);
        if (PLOT) {
        	par(mar=rep(0,4));
        	image(res, col = attr(res, "COL"), add = add, axes=axes, ...)
        }
    return(res);
}