`MaxZoom` <-
function(latrange,lonrange, size = c(640,640)){
   SinPhi = sin(latrange * pi /180);
   normX = lonrange / 180;
   normY = (0.5 * log(abs((1 + SinPhi) / (1 -SinPhi) )) ) / pi;
   
   MaxZoom.lon <- floor(1 + log2(abs(size[1]/256/diff(normX))));
   MaxZoom.lat <- floor(1 + log2(abs(size[2]/256/diff(normY))));
   
   return(c(MaxZoom.lat=MaxZoom.lat,MaxZoom.lon=MaxZoom.lon))
 }

