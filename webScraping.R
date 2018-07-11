#Loading the rvest package
library('rvest')
library('xml2')
library(doParallel)

#Specifying the list urls for data to be scrapped
#url = 'http://mashable.com/2013/01/07/amazon-instant-video-browser/'
urls = read.csv(file="./urls.csv", header=TRUE, sep=",")

#Building data frame
result = data.frame( url=rep(0, 39644), title=rep(0,39644), news=rep(0,39644))
#d[i, ] = c(x, y, z)

i=0
for(item in urls$url){
  #Reading the HTML code from the website
  page = read_html(item)
  
  #Using CSS selectors to scrap the rankings section
  htmlNews = html_nodes(page,'.article-content')
  htmlTitle = html_nodes(page,'.title')
  
  #Saving data
  scrappedData = html_text(htmlNews)
  scrappedTitle = html_text(htmlTitle)
  
  #print(item)
  #print(scrappedTitle)
  #print(scrappedData)
  
  result[i, ] = c(item, scrappedTitle, scrappedData)
  i = i+1
}

"lapply(as.character(urls$url), function(item){
  #Reading the HTML code from the website
  page = read_html(item)
  
  #Using CSS selectors to scrap the rankings section
  htmlNews = html_nodes(page,'.article-content')
  htmlTitle = html_nodes(page,'.title')
  
  #Saving data
  scrappedData = html_text(htmlNews)
  scrappedTitle = html_text(htmlTitle)
  
  result[i, ] = c(item, scrappedTitle, scrappedData)
  i = i+1
})"

i=0
foreach::foreach(item = urls$url)%do%{
  page = read_html(item)
  WS1 = html_session(i)
  #Using CSS selectors to scrap the rankings section
  htmlNews = html_nodes(WS1,page,'.article-content')
  htmlTitle = html_nodes(WS1,page,'.title')
  
  #Saving data
  scrappedData = html_text(htmlNews)
  scrappedTitle = html_text(htmlTitle)
  
  #print(item)
  #print(scrappedTitle)
  #print(scrappedData)
  
  result[i, ] = c(item, scrappedTitle, scrappedData)
  i = i+1
}



result = data.frame(result)

class(result)
