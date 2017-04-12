library(arules)
library(grid)
library(arulesViz)
#'获取到文件路径
getFilePath <- function(filename="achivement"){
  return (paste(chartr("/","\\",getwd()),"\\achivement\\resource\\",filename,".csv",sep=""))
}
#'获取图片路径
getImgPath <- function(){
  return (paste(chartr("/","\\",getwd()),"\\achivement\\img\\",sep=""))
}
#'获取数据源
getDataSource <- function(){
  achivement <- read.transactions(getFilePath("achivement2"),format = "basket",sep=",")
  return (achivement)
}
#'根据支持度画图
paintItemFrequencyPlot <- function(support=0.3){
  png(filename=paste(getImgPath(),"频繁度.png"),width=1000,height=500)
  wdata <- getDataSource()
  itemFrequencyPlot(wdata,support=0.3)
  dev.off()
}
#'获取关联规则并写入文件
getAndWriteRules <- function(support=0.2,confidence=0.2,minlen=2){
  wdata <- getDataSource()
  achivementRules <- apriori(wdata,
                             parameter = list(
                               support=support,
                               confidence=confidence,
                               minlen=minlen
                             ) )
  ordered_achivementrules <- sort(achivementRules, by="lift")
  write(ordered_achivementrules, file=getFilePath("achivementRules"), sep=",", quote=TRUE, row.names=FALSE) 
}
#'绘制Scatter Plot
paintScatterPlot <- function(){
  png(filename=paste(getImgPath(),"Scatter.png"),width=1000,height=500)
  wdata <- getDataSource()
  achivementRules <- apriori(wdata,
                             parameter = list(
                               support=support,
                               confidence=confidence,
                               minlen=minlen
                             ) )
  ordered_achivementrules <- sort(achivementRules, by="lift")
  plot(ordered_achivementrules)
  dev.off()
}


#'itemFreq <- itemFrequency(achivement) 每一个item出现的频率
#'summary(achivement)
#'basketSize <- size(achivement)
#'summary(basketSize)
#'sum(itemFreq)
#'itemCount <- (itemFreq/sum(itemFreq))*sum(basketSize) #每个item出现的次数
#'summary(itemCount)  
#'orderedItem <- sort(itemCount, decreasing = T)  
#'orderedItemFreq <- sort(itemFrequency(achivement), decreasing=T)  
#'image(sample(achivement,50))  

#规则
#'achivementRules <- apriori(achivement,
#'                           parameter = list(
#'                             support=0.2,
#'                             confidence=0.2,minlen=2
#'                           ) )
#'summary(achivementRules)
#'inspect(achivementRules)

#'按照某种排序
#'ordered_achivementrules <- sort(achivementRules, by="lift")
#'inspect(ordered_achivementrules)
#'写入文件
