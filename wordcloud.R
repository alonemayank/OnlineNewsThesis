MyData2 <- read.csv(file="./ONPCSV.csv", header=TRUE, sep=",")

lifestyle = 0
entertainment = 0
business = 0
socialMedia = 0
technology = 0
world = 0
#Iterator for different categories
i=0
j=0
k=0
l=0
m=0
n=0

#Data Frames for categories

dfLS  = c()
dfET  = c()
dfBU  = c()
dfSM  = c()
dfTC  = c()
dfWO  = c()

for(point in 1:nrow(MyData2)){
  if(MyData2[point,"data_channel_is_lifestyle"]>0){
    lifestyle = lifestyle + MyData2[point,"shares"]
    dfLS[i] = as.character(MyData2[point,4])
    i = i+1
  }
  if(MyData2[point,"data_channel_is_entertainment"]>0){
    entertainment = entertainment + MyData2[point,"shares"]
    dfET[j] = as.character(MyData2[point,4])
    j = j+1
  }
  if(MyData2[point,"data_channel_is_bus"]>0){
    business = business + MyData2[point,"shares"]
    dfBU[k] = as.character(MyData2[point,4])
    k = k+1
  }
  if(MyData2[point,"data_channel_is_socmed"]>0){
    socialMedia = socialMedia + MyData2[point,"shares"]
    dfSM[l] = as.character(MyData2[point,4])
    l = l+1
  }
  if(MyData2[point,"data_channel_is_tech"]>0){
    technology = technology + MyData2[point,"shares"]
    dfTC[m] = as.character(MyData2[point,4])
    m = m+1
  }
  if(MyData2[point,"data_channel_is_world"]>0){
    world = world + MyData2[point,"shares"]
    dfWO[n] = as.character(MyData2[point,4])
    n = n+1
  }
}

#Word Cloud Section


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Entertainment
textET = paste(dfET, collapse = '')
textLS = paste(dfLS, collapse = '')
textBU = paste(dfBU, collapse = '')
textWO = paste(dfWO, collapse = '')
textTC = paste(dfTC, collapse = '')
textSM = paste(dfSM, collapse = '')

transformData = function(textData){
  docs <- Corpus(VectorSource(textData))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  #docs <- tm_map(docs, toSpace, pattern = "\\x.*\s")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <- tm_map(docs, toSpace, "xexx")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("one","also","xexx","use","imag","like","user","said")) 
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  #Word Cloud
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}

transformData(textLS)
transformData(textBU)
transformData(textSM)
transformData(textET)
transformData(textTC)
transformData(textWO)
