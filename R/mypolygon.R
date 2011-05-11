`mypolygon` <-
function(x,...){
  polygon(x[,c("X","Y")],col=x[1,"col"],...)  
}

