
setwd("C:/Users/mcolvin/Desktop/tmp")
library(RgoogleMaps)


map <- GetMap(center=c(lat=33.451063,lon=-88.900379), 
	zoom=4,
	size=c(640,640),
	destfile = file.path(getwd(),"meuse.png"),
	maptype="terrain", SCALE = 3);
PlotOnStaticMap(map)

