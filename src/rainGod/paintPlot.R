#画图
#折线图
library(ggplot2)
paintLineGraph <- function(wdata) {
  ggplot(wdata,aes(x = date, y = isRainy))+
  geom_point()
}
paintPieGraph <- function(wdata) {
  isRainy <- as.data.frame(table(wdata$isRainy))
  names(isRainy) <- c("isR","freq")
  noRainTimes <- isRainy$freq[1]
  rainTimes <- isRainy$freq[2]
  noRaintRate <- noRaintimes/(noRaintimes+rainTimes)
  rainRate <- rainTimes/(noRaintimes+rainTimes)
  pie(c(noRaintRate,rainRate),labels = c(paste("没雨:",noRaintRate*100,"%",sep=""),paste("有雨:",rainRate*100,"%",sep="")),
      col = rainbow(2))
}
paintPie3DGraph <- function(wdata) {
  isRainy <- as.data.frame(table(wdata$isRainy))
  names(isRainy) <- c("isR","freq")
  noRainTimes <- isRainy$freq[1]
  rainTimes <- isRainy$freq[2]
  noRaintRate <- noRaintimes/(noRaintimes+rainTimes)
  rainRate <- rainTimes/(noRaintimes+rainTimes)
  pie3D(c(noRaintRate,rainRate),labels = c(paste("没雨:",noRaintRate*100,"%",sep=""),paste("有雨:",rainRate*100,"%",sep="")),
      col = rainbow(2))
}

filepath = chartr("/","\\",getwd())
xiaojingteng <- read.csv(file=paste(filepath,"\\weather\\xiaojingtengwithweather.csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")
paintLineGraph(xiaojingteng)
paintPieGraph(xiaojingteng)
paintPie3DGraph(xiaojingteng)