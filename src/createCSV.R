library(readr)
rename <- function(){
  #为文件命名
  date = Sys.time() #获取当前时间
  paste("weather",format(date,"%Y%m%d"),".csv",sep="")
}

loadWeatherByCity <- function(){
  #读取城市列表，调用爬虫函数，合并数据保存到一个文件
  filepath = chartr("/","\\",getwd())
 
  city <- read.csv(file=paste(filepath,"\\res\\","CITYWORID.csv",sep=""),header=FALSE,fileEncoding="utf-8",encoding="utf-8")
  names(city) <- c("enname","woeid","zhname","province","long","lat") #为数据集命名
  wdata <- do.call(rbind,lapply(city$woeid,getWeather))
  w <- cbind(city,wdata)
  write.csv(w,file=paste(filepath,"\\weather\\",rename(),sep=""),row.names=FALSE,fileEncoding = "utf-8")
}