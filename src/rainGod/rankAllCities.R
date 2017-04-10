library(readr)
library(sqldf)
rankAllCities <- function(){
  filepath = chartr("/","\\",getwd())
  allCityWeather <- read.csv(file=paste(filepath,"\\res\\allcities.csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")
  
  #sql <- 'select substr(weather,CHARINDEX("雨",weather),CHARINDEX("天",weather,CHARINDEX("雨",weather)) ) from allCityWeather order by substr(weather,CHARINDEX("雨",weather),CHARINDEX("天",weather,CHARINDEX("雨",weather)) ) desc'
  #sql <- "select SUBSTR(weather, CHARINDEX('雨' ,weather)+1,CHARINDEX('天', weather,CHARINDEX('雨' ,weather))-CHARINDEX('雨' ,weather)-1 ) from allCityWeather order by cast(SUBSTR(weather, CHARINDEX('雨' ,weather)+1,CHARINDEX('天', weather,CHARINDEX('雨' ,weather))-CHARINDEX('雨' ,weather)-1 ) as INT) desc"
  sql <- "select  date,city,weather from allCityWeather order by cast(SUBSTR(weather, CHARINDEX('雨' ,weather)+1,CHARINDEX('天', weather,CHARINDEX('雨' ,weather))-CHARINDEX('雨' ,weather)-1 ) as INT) desc"
  result <- sqldf(sql)
  write.csv(result,file=paste(filepath,"\\res\\allcities_ranked.csv",sep=""),row.names=TRUE,fileEncoding = "utf-8")
}