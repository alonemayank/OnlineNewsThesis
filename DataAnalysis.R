library("rjson")

#Read Original Statistics Data from UCI
MyData <- read.csv(file="./OnlineNewsPopularity.csv", header=TRUE, sep=",")

#Read data from refined csv of news articles (Not working)
#newsArticles = read.csv(file="./newsData.csv",header=TRUE,sep=",")

#Read news text data in json format
#json = fromJSON(file = "/Users/apurvatripathi/Desktop/Thesis Cloud/OnlineNewsData/webScrapper/mashableScrapper/convertedJSON.json")

#convert json to data frame
#textData = do.call(cbind, json)


#Summary of the data
summary(MyData)


#Head of the data
head(MyData[1:5,])

#Std Deviation of the data
sd(MyData$shares,na.rm = FALSE)
options(scipen=5)


#Simple data Plot shares vs number of images
plot(MyData$shares,MyData$num_imgs,xlab= "Shares", ylab="Images")
title("Simple plot of shares vs images")

#Histogram for shares
hist(MyData$shares,breaks=200,xlab= "Shares",main="Simple Histogram of Shares")
#title("Simple histogram of shares")

#Lets look at the data once again
shares_log = log(MyData$shares)
head(shares_log)
shares_log[1:10]
median(shares_log)
max(shares_log)

#Histogram for shares log
hist(shares_log,breaks=200,xlab= "Shares",ylab="Frequency",main="Simple Histogram of log(Shares)",col = "lightgreen")
curve(dnorm(x, mean=mean(shares_log), sd=sd(shares_log)), add=TRUE, col = "red", lwd=2)

#Use ggplot2 for better plots
require(ggplot2)

#Plotting the histogram
qplot(data = MyData, x = shares_log,main ="Shares Frequency" ) + ylab("Frequency") + xlab("No of Shares")

#Using density
ggplot(data=MyData, aes(x = shares_log)) + stat_density() + ylab("Density") + xlab("No of Shares")
title("Shares Frequency" )

hist(shares_log,breaks=200,freq=FALSE,xlab= "Shares",ylab="Density",main="Density Histogram of log(Shares)",col = "lightgreen")
curve(dnorm(x, mean=mean(shares_log), sd=sd(shares_log)), add=TRUE, col = "red", lwd=2)


#Plot for positive and negative polarity

qplot(data = MyData, x = shares_log, y = MyData$avg_positive_polarity,main ="Shares vs Positive Polarity") + 
  ylab("Positive Polarity") + xlab("Shares") 

qplot(data = MyData, x = shares_log, y = MyData$avg_negative_polarity,main ="Shares vs Neg Polarity") + 
  ylab("Negative Polarity") + xlab("Shares")

#Plot for number of images
qplot(data = MyData, x = shares_log, y = MyData$num_imgs,main ="Shares vs Images") + 
  ylab("Number of Images") + xlab("Shares")


# Boxplot of shares
boxplot(shares_log,data=MyData, main="Shares Data Box Plot")

#Bar plot of shares
barplot(shares_log,main="log(Shares) Data")

#Frequency bar plot
freqTable= table(shares_log)
barplot(freqTable,main="log(Shares) Frequency Data",ylab="Frequency",xlab="log(Shares)")

#Bar plot shares vs videos
barplot(MyData$shares,MyData$num_videos,xlab= "Shares", ylab="Videos",main="log(Shares) vs Videos")

#Histogram of shares vs videos (Not working)
#hist(MyData$shares,MyData$num_videos)

#Summary statistics suggested by Dr. Golen

sapply(MyData, mean, na.rm=TRUE)

library(psych)

description = describe(MyData)

#Writing data in csv
write.csv(description, file = "DataDescription.csv", sep=",")
