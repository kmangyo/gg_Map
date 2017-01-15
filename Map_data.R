# This code based on below posting[1]
# [1] https://shiring.github.io/maps/2016/12/30/Standortverlauf_post

# install.packages('jsonlite')
# install.packages('ggplot2')
# install.packages('ggmap')

library(jsonlite)
library(ggplot2)
library(ggmap)

data<-fromJSON(file.choose())

loc = data$locations
loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7

head(loc)
nrow(loc)
as.POSIXct(as.numeric(min(loc$time))/1000,origin = "1970-01-01",tz = "GMT")
as.POSIXct(as.numeric(max(loc$time))/1000,origin = "1970-01-01",tz = "GMT")

korea <- get_map(location = c(lon = 127, lat = 37.56), zoom = 11, maptype='roadmap')
ggmap(korea)
ggmap(korea) + geom_point(data = loc, aes(x = lon, y = lat), alpha = 0.5, color = "black")
