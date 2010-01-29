myplot.imagematrix <- function (x,y,z, ...) 
{
    colvec <- switch(attr(z, "type"), grey = grey(z), rgb = rgb(z[, 
        , 1], z[, , 2], z[, , 3]))
    if (is.null(colvec)) 
        stop("image matrix is broken.")
    colors <- unique(colvec)
    colmat <- array(match(colvec, colors), dim = dim(z)[1:2]);
    
    if (missing(x)) x = 0:(dim(colmat)[2]);
	if (missing(y)) y = 0:(dim(colmat)[1])
	
    image(x,y, z = t(colmat[nrow(colmat):1, 
        ]), col = colors, xlab = "", ylab = "", ...)
}