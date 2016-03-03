dvd.2013.stations<-read.csv("data/2013/stations.csv")
dvd.2013.trips<-read.csv("data/2013/trips.csv")
dvd.2014.stations<-read.csv("data/2014/stations.csv")
dvd.2014.trips<-read.csv("data/2014/trips.csv")
colors= c("#114477", "#117744", "#117777", "#4477AA", "#44AA77", "#44AAAA", "#771122", "#771155", "#774411", "#777711","#77AADD", "#77CCCC","#88CCAA", "#AA4455", "#AA7744", "#AAAA44","#AA4488","#DD7788", "#DDAA77", "#DDDD77", "#AA4455","#CC99BB")
twocolors= c( "#114477", "#DD7788")
threecolors= c( "#114477", "#DD7788","#44AAAA")
sevencolors= c( "#114477", "#DD7788","#44AAAA","#44AA77","#771155","#777711","#77CCCC")

#1.	Customers V Subscribers 2013
dvd.2013.users.customers<-length(which(dvd.2013.trips$usertype=="Customer"))
dvd.2013.users.subscribers<-length(which(dvd.2013.trips$usertype=="Subscriber"))
pie(c(dvd.2013.users.customers,dvd.2013.users.subscribers),main="Customers V Subscribers",labels=c("Customers","Subscribers"),col=c("#CC0000","#FFFF00"))



#2. Rides Per month
dvd.2013.rides<-strptime(dvd.2013.trips$starttime,"%Y-%m-%d %H:%M")
dvd.2013.ridespermonth<-format(dvd.2013.rides,"%Y-%m")
plot(table(dvd.2013.ridespermonth),type = "b",col=c("#00CC00"),xlab="Month",ylab="Number of Rides",main="Number of Rides Per Month")



#3. Rides Per hour in a day
dvd.2013.ridesperhour<-format(dvd.2013.rides,format = "%H")
barplot(table(dvd.2013.ridesperhour),col = colors, beside=T, xlab = "Hours of Day", ylab = "Number of trips",main = "Number of trips per hour in a day")

#4. Rides per day of a week
dvd.2013.weekdays<-weekdays(dvd.2013.rides)
barplot(table(factor(dvd.2013.weekdays,levels= c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))),col=colors,beside=T,xlab="Day in a week",ylab="Number of trips",main="Number of trips per day in a Week")

#5. Rides by Month

dvd.2013.months<-months(dvd.2013.rides)
barplot(table(factor(dvd.2013.months,levels= c("January", "February","March", "April", "May", 
"June", "July","August","September","October","November","December"))),
col=colors,beside=T,xlab="Months",ylab="Number of trips",main="Number of trips per month")

plot(table(factor(dvd.2013.months,levels= c("January", "February","March", "April", "May", 
"June", "July","August","September","October","November","December"))),
col=colors,type="b",xlab="Months",ylab="Number of trips",main="Number of trips per month")

#5. Rides by Season
dvd.2013.trips$stformatted<-strptime(dvd.2013.trips$starttime,"%Y-%m-%d %H:%M")
dvd.2013.trips$month <- months(dvd.2013.trips$stformatted)
dvd.2013.trips$season[dvd.2013.trips$month=="January"] <- "Winter"
dvd.2013.trips$season[dvd.2013.trips$month=="February"] <- "Winter"
dvd.2013.trips$season[dvd.2013.trips$month=="March"] <- "Spring"
dvd.2013.trips$season[dvd.2013.trips$month=="April"] <- "Spring"
dvd.2013.trips$season[dvd.2013.trips$month=="May"] <- "Spring"
dvd.2013.trips$season[dvd.2013.trips$month=="June"] <- "Summer"
dvd.2013.trips$season[dvd.2013.trips$month=="July"] <- "Summer"
dvd.2013.trips$season[dvd.2013.trips$month=="August"] <- "Summer"
dvd.2013.trips$season[dvd.2013.trips$month=="September"] <- "Fall"
dvd.2013.trips$season[dvd.2013.trips$month=="October"] <- "Fall"
dvd.2013.trips$season[dvd.2013.trips$month=="November"] <- "Fall"
dvd.2013.trips$season[dvd.2013.trips$month=="December"] <- "Winter"

barplot(table(dvd.2013.trips$season),  beside=T,col = colors, xlab="Seasons", ylab="Number of Trips",main = "Number of Trips per Season in 2013", legend=T)


#6. Male V Female
dvd.2013.users.male<-length(which(dvd.2013.trips$gender=="Male"))
dvd.2013.users.female<-length(which(dvd.2013.trips$gender=="Female"))
dvd.2013.users.unknown<-length(which(dvd.2013.trips$gender==""))

pie(c(dvd.2013.users.male,dvd.2013.users.female,dvd.2013.users.unknown),main="Gender Distribution",
labels=c("Male","Female","Unknown"),col=c("#CC0000","#FFFF00","#00CCFF"))

#7. Rides by Season and user type
barplot(table(dvd.2013.trips$usertype,dvd.2013.trips$season),  beside=T,col = colors, xlab="Seasons", ylab="Number of Trips",main = "Number of Trips per Season in 2013", legend=T)

#8. Number of rides by usertype and month
barplot(table(dvd.2013.trips$usertype,dvd.2013.ridespermonth),col=twocolors,beside=T, xlab = "Month in a Year", ylab = "Number of trips",main = "Number of trips per Month in a Year 2013",legend=T)

#9. Number of trips by gender and month
barplot(table(dvd.2013.trips$gender,dvd.2013.ridespermonth),col=threecolors,beside=T, xlab = "Month in a Year", ylab = "Number of trips",main = "Number of trips per Month by each gender in a Year 2013",legend=T)

#10. Hour and day of week
barplot(table(dvd.2013.ridesperhour,dvd.2013.weekdays),col = colors, beside=T, xlab = "Hours of Day X weekdays", ylab = "Number of trips",main = "Distribution of trips by hour for weekdays",legend=T)

#11. Round Trip and Oneway Trips
library(plotrix)
dvd.2013.routes <- cbind(dvd.2013.trips$from_station_id,dvd.2013.trips$to_station_id)
dvd.2013.roundtrip <-0
dvd.2013.onewaytrip <-0
for (i in dvd.2013.routes) {
  if(dvd.2013.routes[i,1]==dvd.2013.routes[i,2]){
   dvd.2013.roundtrip<- dvd.2013.roundtrip+1
  }else{
    dvd.2013.onewaytrip<-dvd.2013.onewaytrip+1
  }
  }
  
values<-c(dvd.2013.roundtrip, dvd.2013.onewaytrip)
labels <- c("Round trip","One Way trip")
pcts <- round(values/sum(values)*100)
labels <- paste(labels, pcts) # add percents to labels 
labels <- paste(labels,"%",sep="") # ad % to labels 
pie(values, labels = labels, main="Trip Routes Type (Round and One Way Trip)",explode=0.1)
pie3D(values, labels = labels, main="Trip Routes Type (Round and One Way Trip)",explode=0.1)


#13. Map Plot of Stations
library(ggmap)
dvd.2013.latlong<-as.data.frame(cbind(dvd.2013.stations$latitude,dvd.2013.stations$longitude))
stationmap<-get_map(location=c(lon=mean(dvd.2013.latlong$V2),lat=mean(dvd.2013.latlong$V1)),zoom = 12,maptype="roadmap",scale=2)

 ggmap(stationmap)+
 geom_point(data = dvd.2013.latlong, aes(x = V2, y = V1, fill = "red", alpha = 0.8),size = 2, shape = 17,show.legend = TRUE,na.rm=TRUE) +
 guides(fill=FALSE, alpha=FALSE, size=FALSE)


#14. Age Distribution for 2014
library(ggplot)
dvd.2014.demographics<-as.integer(format(Sys.Date(),"%Y"))-dvd.2014.trips$birthyear
ggplot(dvd.2014.trips,aes(dvd.2014.demographics,fill=gender),col=colors)+geom_bar(position="stack",binwidth=1,col="black")+scale_x_continuous("Age")+scale_y_continuous("Frequency")+ggtitle("Age Distribution")



#11. Station pair
dvd.2013.stationpairs<-as.data.frame(cbind(dvd.2013.trips$from_station_id,dvd.2013.trips$to_station_id))


#12. Trip duration distribution
dvd.2013.startdate<-format(dvd.2013.rides,"%Y-%m-%d")

dvd.2013.stationpairs<-as.data.frame(cbind(dvd.2013.trips$from_station_id,dvd.2013.trips$to_station_id))
