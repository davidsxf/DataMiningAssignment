library(RCurl) 
library(XML)
library(bitops)

getWeather <- function(woeid){
  
  
 
  #http://query.yahooapis.com/v1/public/yql?q=select * from weather.forecast where woeid=2151330 and u='c'&format=xml
  #woeid <- 2151330
  #queryurl <- paste("http://query.yahooapis.com/v1/public/yql?q=select * from weather.forecast where woeid=",woeid," and u='c'&format=xml",sep="")
  #ans <- getNodeSet(doc,"//yweather:atmosphere")
  
  queryurl <- paste("http://query.yahooapis.com/v1/public/yql?q=select * from weather.forecast where woeid=2151330"," and u='c'&format=xml",sep="")
  doc  <- xmlTreeParse(queryurl,useInternalNodes=TRUE)
  rootnode <- xmlRoot(doc) 
  
  ans <- rootnode[["results"]][["channel"]][["atmosphere"]]  #yweather:atmosphere
  humidity <- xmlGetAttr(ans,"humidity") #温度
  visibility <- xmlGetAttr(ans,"visibility") #能见度
  pressure <- xmlGetAttr(ans,"pressure") #气压
  rising <- xmlGetAttr(ans,"rising") #气压变化
  
  ans <- rootnode[["results"]][["channel"]][["item"]][["condition"]]
  code <- xmlGetAttr(ans,"code") #获取当前日期的天气编码
  
  ans <- rootnode[["results"]][["channel"]][["item"]][["forecast"]]
  low <- xmlGetAttr(ans,"low")   #最低气温
  high <- xmlGetAttr(ans,"high") #最高气温
  
  cbind(low,high,code,humidity,visibility,pressure,rising)
  
}

