library(RCurl) 
library(XML)
library(bitops)

getWeather <- function(woeid){
  
  
 
  #http://query.yahooapis.com/v1/public/yql?q=select * from weather.forecast where woeid=2151330 and u='c'&format=xml
  #woeid <- 2151330
  #queryurl <- paste("http://query.yahooapis.com/v1/public/yql?q=select * from weather.forecast where woeid=",woeid," and u='c'&format=xml",sep="")
  
  queryurl <- paste("http://query.yahooapis.com/v1/public/yql?q=select * from weather.forecast where woeid=2151330"," and u='c'&format=xml",sep="")
  doc  <- xmlTreeParse(queryurl,useInternalNodes=TRUE)
  rootnode <- xmlRoot(doc) 
  
  ans <- rootnode[[1]][[1]][[10]]  #yweather:atmosphere
  #ans <- getNodeSet(doc,"//yweather:atmosphere")
  humidity <- as.numeric(sapply(ans,xmlGetAttr,"humidity")) 
  visibility <- as.numeric(sapply(ans,xmlGetAttr,"visibility")) 
  pressure <- as.numeric(sapply(ans,xmlGetAttr,"pressure")) 
  rising <- as.numeric(sapply(ans,xmlGetAttr,"rising")) 
  
  ans <- getNodeSet(doc,"//item/yweather:condition")
  code <- sapply(ans,xmlGetAttr,"code") 
  
  ans <- getNodeSet(doc,"//item/yweather:forecast[1]") 
  low <- as.numeric(sapply(ans,xmlGetAttr,"low")) 
  high <- as.numeric(sapply(ans,xmlGetAttr,"high")) 
  
  cbind(low,high,code,humidity,visibility,pressure,rising)
  
}

