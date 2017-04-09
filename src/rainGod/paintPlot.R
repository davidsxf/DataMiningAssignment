#画图
#折线图
library(ggplot2)
library(plotrix)
paintLineGraph <- function(wdata) {
  ggplot(wdata,aes(x = date, y = isRainy))+
  geom_point()
}
#饼状图
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
#3d饼状图
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

#绘制柱状图
paintBarGraph <- function(city="beijing",month="07"){
  filepath <- chartr("/","\\",getwd())
  filename <- getCityNameByPin(city,month)
  png(file=paste(filepath,"\\img\\",filename,".png",sep=""))
  rainyDays <- countRainyDaysByCityAndMonth(filename=city,month=month)#纵坐标
  years <- c(2011,2012,2013,2014,2015,2016)
  barplot(height = rainyDays,names.arg = years,
          ylim = c(0,max(rainyDays)+5),xlab = "年份",
          ylab = "下雨天数",main = filename,
          space = 1,col = c(8),width=c(1),axis.lty = 1
          )
  dev.off()
}

#根据城市，获取2001-2017年中1-12月平均每月下雨的天数
paintBarGraphAvg <- function(city="beijing"){
  filepath <- chartr("/","\\",getwd())
  filename <- getCityNameByPinAvg(city)
  png(file=paste(filepath,"\\img\\",filename,".png",sep=""))
  rainyDays <- countRainyDaysByCityAndMonthAvg(filename=city)#纵坐标
  years <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  barplot(height = rainyDays,names.arg = years,
          ylim = c(0,max(rainyDays)+5),xlab = "年份",
          ylab = "下雨天数",main = filename,
          space = 1,col = c(8),width=c(1),axis.lty = 1
  )
  dev.off()
}



getCityNameByPin <- function(city = "beijing",month = "07"){
  if(city == "beijing")
    r = paste("北京各年",month,"月份下雨天数",sep="")
  if(city == "shanghai")
    r = paste("上海各年",month,"月份下雨天数",sep="")
  if(city == "changsha")
    r = paste("长沙各年",month,"月份下雨天数",sep="")
  if(city == "tianjin")
    r = paste("天津各年",month,"月份下雨天数",sep="")
  return (r)
}
getCityNameByPinAvg <- function(city = "beijing"){
  if(city == "beijing")
    r = "北京各年平均每个月份下雨天数"
  if(city == "shanghai")
    r = "上海各年平均每个月份下雨天数"
  if(city == "changsha")
    r = "长沙各年平均每个月份下雨天数"
  if(city == "tianjin")
    r = "天津各年平均每个月份下雨天数"
  return (r)
}