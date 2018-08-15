#Assignment 5.3

#read zip file and get it into R
getwd()
setwd("D:/Acadgild/sample dataset")
getwd()
AirQuality<-read.csv("D:/Acadgild/sample dataset/AirQualityUCI.csv")
head(AirQuality)

#Univariate for any column 

mean(AirQuality$PT08.S2.NMHC.)
median(AirQuality$PT08.S2.NMHC.)
mode(AirQuality$C6H6.GT.)
min(AirQuality$NOx.GT.)
max(AirQuality$C6H6.GT.)
barplot(AirQuality$NMHC.GT., main="first bar", col = "blue")
hist(AirQuality$RH, xlab = "RH")
boxplot(AirQuality$AH, xlab="AH")
boxplot(AirQuality$T, xlab="T")

#Check Missing value 

sum(is.na(AirQuality))

#impute missing value 

AirQuality$T[is.na(AirQuality$T)]<- median(AirQuality$T, na.rm=T)

#Bivariate Analysis

Airdata<- AirQuality[,-c(1:2)]
head(Airdata)
cor(Airdata) #corelation

cor(Airdata$PT08.S2.NMHC., Airdata$PT08.S3.NOx.) #by default method is pearson
cor(Airdata$PT08.S2.NMHC., Airdata$PT08.S3.NOx., method = "spearman")

cor.test(AirQuality$T, AirQuality$RH)

cov(Airdata$PT08.S2.NMHC., Airdata$PT08.S3.NOx.) #covariance

library(corrplot)
corrplot(cor(Airdata))

pairs(Airdata)



#trend and pattern in time series

data<- ts(AirQuality$PT08.S1.CO.) #for any one column i.e chemical PT08.S1.CO
plot(data, main= "polution")

library(forecast)
plot( AirQuality$Time, AirQuality$C6H6.GT.)

boxplot(AirQuality$CO.GT.)


#Most polluted time of the day 

#19:00 hrs
