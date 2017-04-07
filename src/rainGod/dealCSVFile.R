library(readr)
mergeWeatherAndLocation <- function(){
  #读取城市列表，调用爬虫函数，合并数据保存到一个文件
  filepath = chartr("/","\\",getwd())
  xiaojingteng <- read.csv(file=paste(filepath,"\\res\\xiaojingteng.csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")
  size = length(xiaojingteng$pinyin) #get row number
  i = 1
  result <- c()
  for(i in 1:size){
    wdata = getWeatherByCityNameAndDate(city=xiaojingteng$pinyin[i],date=xiaojingteng$date[i])
    result <- append(result,c(wdata))
  }
  wdataResult <- data.frame(matrix(result,nrow = 40,ncol = 5,byrow = TRUE))
  names(wdataResult) <- c("high","low","weather","winddirec","windlevel") #为数据集命名
  w <- cbind(xiaojingteng,wdataResult)
  write.csv(w,file=paste(filepath,"\\weather\\xiaojingtengwithweather.csv",sep=""),row.names=TRUE,fileEncoding = "utf-8")
}