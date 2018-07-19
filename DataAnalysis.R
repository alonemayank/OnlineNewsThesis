library("rjson")
#Next line for exporting to HTML as PDF fails (it will not render any plots)
#dev.off()
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

#Summary of shares
summary(MyData$shares)


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

qplot(data = MyData, x = shares_log, y = MyData$avg_positive_polarity,
      main ="Shares vs Positive Polarity") + 
  ylab("Positive Polarity") + xlab("log(Shares)") + geom_point(color='blue', alpha = 0.1)

qplot(data = MyData, x = shares_log, y = MyData$avg_negative_polarity,main ="Shares vs Neg Polarity") + 
  ylab("Negative Polarity") + xlab("log(Shares)") + geom_point(color='blue', alpha = 0.1)

#Plot for number of images
qplot(data = MyData, x = shares_log, y = MyData$num_imgs,main ="Shares vs Images") + 
  ylab("Number of Images") + xlab("log(Shares)") + geom_point(color='blue', alpha = 0.1)


# Boxplot of shares
boxplot(shares_log,data=MyData, main="Shares Data Box Plot")

plot = ggplot(MyData, aes(y=shares_log),ylab="log(Share)") + geom_boxplot(color="blue",
fill="blue",
alpha = 0.2,
# custom outliers
outlier.colour="red",
outlier.fill="red",
outlier.size=3
)
plot + ggtitle("Box Plot of log(Shares)")

#Bar plot of shares
barplot(shares_log,main="log(Shares) Data")

#Frequency bar plot
freqTable= table(shares_log)
barplot(freqTable,main="log(Shares) Frequency Data",ylab="Frequency",xlab="log(Shares)")

#Bar plot shares vs videos
barplot(MyData$shares,MyData$num_videos,xlab= "Shares", ylab="Videos",main="log(Shares) vs Videos")

#Histogram of shares vs videos (Not working)
#hist(MyData$shares,MyData$num_videos)

#Pie Chart of News Categeories
slices = c(sum(MyData$data_channel_is_lifestyle),sum(MyData$data_channel_is_entertainment),sum(MyData$data_channel_is_bus),sum(MyData$data_channel_is_socmed),sum(MyData$data_channel_is_tech),sum(MyData$data_channel_is_world)) 
Clabels = c("Lifestyle", "Entertainment", "Bussniss", "Social Media", "Technology", "World")
percentage = round(slices/sum(slices)*100)
Clabels = paste(Clabels, percentage) # add percents to labels 
Clabels = paste(Clabels,"%",sep="") # ad % to labels 
pie(slices,labels = Clabels,main="Pie Chart of News Categeories")

#Bar chart of categories with shares
# lifestyle = sum(MyData$data_channel_is_lifestyle)
# entertainment = sum(MyData$data_channel_is_entertainment)
# business = sum(MyData$data_channel_is_bus)
# socialMedia = sum(MyData$data_channel_is_socmed)
# technology = sum(MyData$data_channel_is_tech)
# world = sum(MyData$data_channel_is_world)

lifestyle = 0
entertainment = 0
business = 0
socialMedia = 0
technology = 0
world = 0

#categories = c(lifestyle,entertainment,business,socialMedia,technology,world)





for(point in 1:nrow(MyData)){
  if(MyData[point,"data_channel_is_lifestyle"]>0){
    lifestyle = lifestyle + MyData[point,"shares"]
    dfLS[i] = MyData[point,"content"]
    i = i+1
  }
  if(MyData[point,"data_channel_is_entertainment"]>0){
    entertainment = entertainment + MyData[point,"shares"]
    dfET[j] = MyData[point,"content"]
    j = j+1
  }
  if(MyData[point,"data_channel_is_bus"]>0){
    business = business + MyData[point,"shares"]
    dfBU[k] = MyData[point,"content"]
    k = k+1
  }
  if(MyData[point,"data_channel_is_socmed"]>0){
    socialMedia = socialMedia + MyData[point,"shares"]
    dfSM[l] = MyData[point,"content"]
    l = l+1
  }
  if(MyData[point,"data_channel_is_tech"]>0){
    technology = technology + MyData[point,"shares"]
    dfTC[m] = MyData[point,"content"]
    m = m+1
  }
  if(MyData[point,"data_channel_is_world"]>0){
    world = world + MyData[point,"shares"]
    dfWO[n] = MyData[point,"content"]
    n = n+1
  }
}

#hist(dfLS,breaks=500,xlab= "Shares",ylab="Frequency",main="Simple Histogram of lLifestyle",col = "lightgreen")
#plot(dfLS)
#lifestyle = lifestyle - 843300
categories = c(lifestyle,entertainment,business,socialMedia,technology,world)

# Categories Shares bar plot in ggplot

barplot(categories, main="Categories", names.arg = Clabels)
plotFrame = data.frame(cats = Clabels, vals = categories)
ggplot(data=plotFrame, aes(x=cats,y=vals)) + 
  geom_bar(stat="identity",fill="steelblue")+
  geom_text(aes(label=vals),vjust = -0.3,size=3.5)+
  xlab("Categories") +
  ylab("Total Shares") +
  ggtitle("Categories Shares") +
  theme_get() +
  theme(plot.title = element_text(hjust = 0.5)) 





#Summary statistics suggested by Dr. Golen

sapply(MyData, mean, na.rm=TRUE)

library(psych)

description = describe(MyData)

#Writing data in csv
write.csv(description, file = "DataDescription.csv")
