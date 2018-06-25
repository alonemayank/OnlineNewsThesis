MyData <- read.csv(file="/Users/apurvatripathi/Downloads/OnlineNewsPopularity2/OnlineNewsPopularity.csv", header=TRUE, sep=",")
#Summary of the data
summary(MyData)

#Head of the data
head(MyData[1:5,])

#Std Deviation of the data
sd(MyData$shares,na.rm = FALSE)
options(scipen=5)


#Simple data Plot
plot(MyData$num_imgs,MyData$shares)

#Histogram for shares
hist(MyData$shares)

#Lets look at the data once again
shares_log = log(MyData$shares)
head(shares_log)
shares_log[1:10]
median(shares_log)
max(shares_log)

#Use ggplot2 for better plots
require(ggplot2)

#Plotting the histogram with delimiting x and y axis
qplot(data = MyData, x = shares_log) + ylab("Frequency") + xlab("No of Shares")

#Using density
ggplot(data=MyData, aes(x = shares_log)) + stat_density()

#Plot for positive and negative polarity

qplot(data = MyData, x = MyData$avg_positive_polarity, y = shares_log) + 
  ylab("Shares") + xlab("Positive Polarity") 

qplot(data = MyData, x = MyData$avg_negative_polarity, y = shares_log) + 
  ylab("Shares") + xlab("Negative Polarity")

#Plot for number of images
qplot(data = MyData, x = MyData$num_imgs, y = shares_log) + 
  ylab("Shares") + xlab("Number of Images")


# Boxplot of MPG by Car Cylinders 
boxplot(shares_log,data=MyData, main="Shares Data Box Plot")
barplot(shares_log) 
freqTable= table(shares)
barplot(freqTable)
barplot(MyData$num_videos,MyData$shares)
hist(MyData$num_videos,MyData$shares)
