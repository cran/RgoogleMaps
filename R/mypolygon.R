`mypolygon` <-
function(x,...){
  browser();
  polygon(x[,c("X","Y")],col=x[1,"col"],...)  
}

