# This code based on below posting[1]. Google activitiy references are below[2].
# [1] https://shiring.github.io/maps/2016/12/30/Standortverlauf_post
# [2] https://developers.google.com/android/reference/com/google/android/gms/location/DetectedActivity.html

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
# korea <- get_map(location = c(lon = 126.98, lat = 37.56), zoom = 11, maptype='toner')

ggmap(korea)
ggmap(korea) + geom_point(data = loc_accu_act, aes(x = lon, y = lat), alpha = 0.5, color = "black")
ggmap(korea) + geom_point(data = loc_accu_act_name, aes(x = lon, y = lat, color=as.factor(act)), size = I(5), alpha = 0.5)
ggmap(korea) + geom_point(data = subset(loc_accu_act_name,act==c('onFoot')), aes(x = lon, y = lat, color=as.factor(act)), size = I(5), alpha = 0.5)

loc_accu_act_name$date<-as.POSIXlt(loc_accu_act_name$time)
loc_accu_act_name$wday<-loc_accu_act_name$date$wday
loc_accu_act_name$mon<-loc_accu_act_name$date$mon+1
loc_accu_act_name$year<-loc_accu_act_name$date$year+1900

ggmap(korea) + geom_point(data =loc_accu_act_name, aes(x = lon, y = lat, color=as.factor(act)), alpha = 0.5) + facet_wrap(~ year)
ggmap(korea) + geom_point(data =loc_accu_act_name, aes(x = lon, y = lat, color=as.factor(act)), alpha = 0.5) + facet_wrap(~ mon)
ggmap(korea) + geom_point(data =loc_accu_act_name, aes(x = lon, y = lat, color=as.factor(act)), alpha = 0.5) + facet_wrap(~ wday)

# world <- get_map(location = c(lon = 135.5, lat = 34.8), zoom = 9, maptype='toner')
world <- get_map(location = c(lon = 135.5, lat = 34.8), zoom = 9, maptype='roadmap')
ggmap(world)
ggmap(world) + geom_point(data = subset(loc_accu_act_name,act==c('onFoot')|act==c('still')|act==c('tilting')), aes(x = lon, y = lat, color=as.factor(act)), size = I(5), alpha = 0.8)

# clustering
loc_japan<- subset(loc_accu_act_name, act==c('onFoot')|act==c('still')|act==c('tilting'))
loc_japan<- subset(loc_japan, lon>=134.5 & lon<=136.5 & lat>=34 & lat<=35.5)
ggmap(world) + geom_point(data = loc_japan, aes(x = lon, y = lat, color=as.factor(act)), size = I(3), alpha = 0.8)

# The number of clusters. Reference is below
# http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
japan_pst <- data.frame(loc_japan$lon, loc_japan$lat)
wss <- (nrow(japan_pst)-1)*sum(apply(japan_pst,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(japan_pst,centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

japan_pst_cluster <- kmeans(japan_pst, centers=4, iter.max=10, nstart=100)
japan_pst$cluster <- factor(japan_pst_cluster$cluster)

plot(japan_pst, col=japan_pst$cluster)
ggmap(world) + geom_point(data = japan_pst, aes(x = loc_japan.lon, y = loc_japan.lat, color=as.factor(cluster)), size = I(4), alpha = 0.8)
ggmap(world) + geom_point(data = japan_pst, aes(x = loc_japan.lon, y = loc_japan.lat), size = I(4), alpha = 0.8)
