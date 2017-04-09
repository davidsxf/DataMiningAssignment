library(readr)
mergeWeatherAndLocation <- function(personname="xiaojingteng"){
  #读取城市列表，调用爬虫函数，合并数据保存到一个文件
  filepath = chartr("/","\\",getwd())
  person <- read.csv(file=paste(filepath,"\\res\\",personname,".csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")
  size = length(person$pinyin) #get row number
  i = 1
  result <- c()
  for(i in 1:size){
    wdata = getWeatherByCityNameAndDate(city=person$pinyin[i],date=person$date[i])
    result <- append(result,c(wdata))
  }
  wdataResult <- data.frame(matrix(result,nrow = size ,ncol = 6,byrow = TRUE))
  names(wdataResult) <- c("high","low","weather","winddirec","windlevel","isRainy") #为数据集命名
  w <- cbind(person,wdataResult)
  write.csv(w,file=paste(filepath,"\\weather\\",personname,"withweather.csv",sep=""),row.names=TRUE,fileEncoding = "utf-8")
}
