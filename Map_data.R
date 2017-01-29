# This code based on below posting[1]
# [1] https://shiring.github.io/maps/2016/12/30/Standortverlauf_post

# install.packages('jsonlite')
# install.packages('ggplot2')
# install.packages('ggmap')
# devtools::install_github("dkahle/ggmap")

library(jsonlite)
library(ggplot2)
library(ggmap)
library(dplyr)
library(reshape2)

data<-fromJSON(file.choose())

loc = data$locations
loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7

head(loc)
nrow(loc)
as.POSIXct(as.numeric(min(loc$time))/1000,origin = "1970-01-01",tz = "GMT")
as.POSIXct(as.numeric(max(loc$time))/1000,origin = "1970-01-01",tz = "GMT")

loc$time<-as.POSIXct(as.numeric(loc$timestampMs)/1000,origin = "1970-01-01",tz = "GMT")
loc_accu<-subset(loc, accuracy<=20)

activities <- loc_accu$activitys

# example
# activities[[127]][2][1,1][[1]][1,1]
# activities[[127]][2][1,1][[1]][1,2]

activities_list <- list()

for(i in 1:length(activities)) {
  if (is.null(activities[i][[1]])) {
    activities_list[i] <- 'NA'
  } else {
    activities_list[i] <- activities[[i]][2][1,1][[1]][1,1]
  }
}

activities_list <-melt(activities_list)

activities_list_accu <- list()

for(i in 1:length(activities)) {
  if (is.null(activities[i][[1]])) {
    activities_list_accu[i] <- 'NA'
  } else {
    activities_list_accu[i] <- activities[[i]][2][1,1][[1]][1,2]
  }
}

activities_list_accu <-melt(activities_list_accu)

activities_list<-cbind(activities_list, activities_list_accu)
activities_list<-activities_list[c(1:3)]
names(activities_list)<-c('act','seq','ratio')


loc_accu_act<-cbind(loc_accu,activities_list)

loc_accu_act_name<-subset(loc_accu_act, 
                          act==c('still')|act==c('onFoot')|act==c('inVehicle')|act==c('exitingVehicle')|act==c('onBicycle')|act==c('tilting')&ratio>=80)

korea <- get_map(location = c(lon = 127, lat = 37.56), zoom = 11, maptype='roadmap')
ggmap(korea)
ggmap(korea) + geom_point(data = loc, aes(x = lon, y = lat), alpha = 0.5, color = "black")
ggmap(korea) + geom_point(data = loc_accu_act, aes(x = lon, y = lat), alpha = 0.5, color = "black")
ggmap(korea) + geom_point(data = loc_accu_act_name, aes(x = lon, y = lat, color=as.factor(act)), alpha = 0.5)
ggmap(korea) + geom_point(data = subset(loc_accu_act_name,act==c('onFoot')), aes(x = lon, y = lat, color=as.factor(act)), alpha = 0.5)
