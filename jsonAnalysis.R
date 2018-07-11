#install rjson if required
#install.packages("rjson")
#package required to read JSON files.
library("rjson")

# input file name to the function.
json = fromJSON(file = "/Users/apurvatripathi/Desktop/Thesis Cloud/OnlineNewsData/webScrapper/mashableScrapper/convertedJSON.json")

#Convert to r dataframe
newsText = do.call(cbind, json)

#Write data to csv file change seperators
write.csv(newsText, file = "NewsData2.csv", sep="|")
  
  
