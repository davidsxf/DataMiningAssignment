#调用
library(RCurl) #获取天气的包
library(XML) #解析XML
library(readr) #生成csv
library(maps) #创建map
library(mapdata) #创建map
library(maptools) #创建map
library(recharts) #生成recharts
library(htmlwidgets)

filepath = chartr("/","\\",getwd())
wdata <- read.csv(file=paste(filepath,"\\weather\\","weather",getCurrentDate(),".csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")#读取详细分类的天气
recharts.emap <- createHTMLWithWeather(wdata = wdata,type="high",output=FALSE)
recharts.emap
