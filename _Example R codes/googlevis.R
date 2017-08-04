library("googleVis")

set.seed(2020)
x <- seq(0,100,by=0.5)
y <- (50-x)^2+rnorm(length(x),sd=100)
 
curvy <- data.frame(x,y)
 
 
gvScat <- gvisScatterChart(curvy,
                   options=list(
                     explorer="{actions: [‘dragToZoom’,
                     ‘rightClickToReset’],
                     maxZoomIn:0.05}",
                     chartArea="{width:’85%’,height:’80%’}",
                     hAxis="{title: ‘Explanatory x’,
                     titleTextStyle: {color: ‘#000000’}}",
                     vAxis="{title: ‘Response y’,
                     titleTextStyle: {color: ‘#000000’}}",
                     title="Curvilinear Relationship",
                     width=550, height=500,
                     legend="none"),
                   chartid="ZoomZoom")
 
print(gvScat,'chart')