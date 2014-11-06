a = read.csv(file="E:/Divvy_Trips_2014_Q1Q2.csv",head=TRUE,sep=",")
attach(a)
summary(a)

start = split(a, a$from_station_id) 
#For each station, get the average trip duration where it is the start
startMean = aggregate( a$tripduration~a$from_station_id, a, mean )
plot(startMean)

end = split(a, a$from_station_id)
#For each station, get the average trip duration where it is the end
endMean = aggregate( a$tripduration~a$to_station_id, a, mean )
prop.table(table(a$usertype)) *100 #percentage of custumers and subscribers

###For the members get the age and gender distribution
member = split(a, a$usertype)
hist(member$Subscriber$birthyear, axes = FALSE, xlab = 'Year of birth',  )
axis(1,at = seq(1890,1998,10),labels = TRUE,pos = 0)
axis(2, at = seq(0,160000,50000), pos = 1890)

boxplot(member$Subscriber$birthyear ~ member$Subscriber$gender)
table(member$Subscriber$birthyear)

age = 2014 - prop.table(table(a$birthyear))#how many males and females in the data set
prop.table(table(member$Subscriber$gender) ) *100 #percentage of male and female

gen = with(a, table(a$gender, a$birthyear))

#Get the trip duration distribution by age and by gender overall
plot(member$Subscriber$tripduration, member$Subscriber$birthyear)
test = with(member$Subscriber, table(member$Subscriber$tripduration, member$Subscriber$birthyear))
graph0 = margin.table(test,c(1,2))
barplot(graph0, xlab= 'Trip duration', ylab = 'Frequency of people')

forGender = split(member$Subscriber, member$Subscriber$gender)
tet = table(forGender$Female$tripduration)
barplot(margin.table(tet[50:70], 1), ylab ="Female", xlab="Station")

tet1 = table(forGender$Male$tripduration)
barplot(margin.table(tet1, 1), ylab ="Male", xlab="Station")


#For each station, get the distribution by age and by gender

byYear = split(member$Subscriber, member$Subscriber$birthyear)
kk = table(byYear$'1985'$from_station_id)
barplot(margin.table(kk[40:65],1))
byAgeFrom = table(member$Subscriber$birthyear, member$Subscriber$from_station_id)

graph = margin.table(byAgeFrom,c(1,2))
barplot(graph,col=c("yellow", "red","blue" ))

bygenderFrom = table(member$Subscriber$gender, member$Subscriber$from_station_id)
tt = margin.table(bygenderFrom[,50:75],c(1,2))
barplot(tt,legend=T, col=c("yellow", "red","blue" ))


byAgeTo = table(member$Subscriber$birthyear, member$Subscriber$to_station_id)
bygenderTo = table(member$Subscriber$gender, member$Subscriber$to_station_id)
graph3 = margin.table(bygenderTo,c(1,2))
barplot(graph3,legend=T, col=c("yellow", "red","blue" ))

