library("rjson")

# input file name to the function.
json = fromJSON(file = "/Users/apurvatripathi/Desktop/Thesis Cloud/OnlineNewsData/webScrapper/mashableScrapper/convertedJSON.json")

#Convert to r dataframe
newsText = do.call(cbind, json)
library(tm)

#
textData = data.frame(newsText)

docs = Corpus(DataframeSource(textData))
