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
shares = MyData$shares
median(shares)
max(shares)

#Use ggplot2 for better plots
require(ggplot2)

#Plotting the histogram with delimiting x and y axis
qplot(data = MyData, x = shares) + ylab("Frequency") + xlab("No of Shares") + 
  xlim(c(0, 5000)) + ylim(c(0,7500))

#Using density
ggplot(data=MyData, aes(x = shares)) + stat_density() + xlim(c(0, 5000))

#Plot for positive and negative polarity

qplot(data = MyData, x = MyData$avg_positive_polarity, y = shares) + 
  ylab("Shares") + xlab("Positive Polarity") + 
  ylim(c(0,7500)) + xlim(c(0,1))

qplot(data = MyData, x = MyData$avg_negative_polarity, y = shares) + 
  ylab("Shares") + xlab("Negative Polarity") + 
 ylim(c(0,7500)) + xlim(c(0,-1))

#Plot for number of images

qplot(data = MyData, x = MyData$num_imgs, y = shares) + 
  ylab("Shares") + xlab("Number of Images") + 
  ylim(c(0,7500)) + xlim(c(0,55))
