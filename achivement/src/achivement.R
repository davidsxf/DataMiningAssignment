library(arules)
library(grid)
library(arulesViz)
#'获取到文件路径
getFilePath <- function(filename="achivement"){
  return (paste(chartr("/","\\",getwd()),"\\achivement\\resource\\",filename,".csv",sep=""))
}
#
getAllSubject <- function(){
  subject <- c("思想道德修养与法律基础           
","中国近现代史纲要        
","程序设计基础与C程序设计","面向对象方法与C++程序设计","模拟与数字电路       
","计算机系统组装与设置","模拟与数字电路实验","工科数学分析基础1","工科数学分析基础2","线性代数与解析几何","军事理论","马克思主义基本原理","毛泽东思想和中国特色社会主义理论体系概论","数据结构与算法","离散数学","计算机组织与结构 ","操作系统","计算机组织与结构实验","大学物理","概率与统计A ","网络综合实验",
"数据库系统","软件工程","UML","系统分析与设计","军训","健康教育")
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
#'根据每个元素支持度画图
paintItemFrequencyPlot <- function(support=0.5){
  png(filename=paste(getImgPath(),"支持度.png"),width=1000,height=500)
  wdata <- getDataSource()
  itemFrequencyPlot(wdata,support=0.3)
  dev.off()
}

#'获取关联规则
getRules <- function(support=0.5,confidence=0.3,minlen=2){
  wdata <- getDataSource()
  achivementRules <- apriori(wdata,
                             parameter = list(
                               support=support,
                               confidence=confidence,
                               minlen=minlen
                             ) )
  ordered_achivementrules <- sort(achivementRules, by="lift")
  return (ordered_achivementrules)
}

#处理关联规则
dealWithRules <- function(support=0.5,confidence=0.3,minlen=2){
  rules <- getRules(support,confidence,minlen)
  writeData(rules,"achivement_rules")#
  paintScatterPlot(rules,"Scatter_rules")
  paintGroupedMatrix(rules,"Matrix_rules")
  paintGraph(rules,"Graph_rules")
}

#'获取频繁项集
getFreqItemSet <- function(support=0.5,minlen=2){
  wdata <- getDataSource()
  wdata.eclat <- eclat(wdata,parameter = list(
    support=support,
    minlen = minlen))
  return (wdata.eclat)
}

#'处理频繁项集
dealWithFreqItemSet <- function(support=0.5,minlen=2){
  freqItemSet <- getFreqItemSet(support,minlen)
  writeData(freqItemSet,"achivement_freqItemSet")#
  paintScatterPlot(freqItemSet,"Scatter_freqItemSet")
  paintGraph(freqItemSet,"Graph_freqItemSet")
}

#'获取关联规则并写入文件
writeData <- function(wdata,filename){
  write(wdata, file=getFilePath(filename), sep=",", quote=TRUE, row.names=FALSE) 
}
#'绘制Scatter Plot
paintScatterPlot <- function(wdata,filename){
  png(filename=paste(getImgPath(),filename,".png",sep=""),width=1000,height=500)
  plot(wdata)
  dev.off()
}

#'绘制Grouped Matrix
paintGroupedMatrix <- function(wdata,filename){
  png(filename=paste(getImgPath(),filename,".png",sep=""),width=1000,height=500)
  plot(wdata,method="grouped")
  dev.off()
}

#'绘制graph
paintGraph <- function(wdata,filename){
  png(filename=paste(getImgPath(),filename,".png",sep=""),width=1000,height=500)
  plot(wdata,method="graph",shading = "lift")
  dev.off()
}

#'根据achivement.csv文件绘制每一门课的平均分值
paintEveryClassAvg <- function(){
  wdata <- read.csv(getFilePath("achivement"),header = TRUE,fileEncoding="utf-8",encoding="utf-8")
  grade <- c()#存储每一列的平均值
  size <- length(wdata)
  for(col in 1:size){
    w <- wdata[,col]
    s <- length(w)
    sum <- 0
    for(every in 1:s){
      sum <- sum + w[every]
    }
    grade <- append(grade,c(sum/s))
    sum=0
  }
  subject <- c("A","B","C","D","E","F","G","H","R","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","AA")
  paintBarGraph(subject,grade)
}

#'绘制柱状图
#'subject-各科科目作为横坐标 grade-各科平均成绩作为纵坐标
paintBarGraph <- function(subject,grade){
  png(filename=paste(getImgPath(),"各科平均成绩.png",sep=""),width=1000,height=500)
  barplot(height = grade,names.arg = subject,
          ylim = c(0,100),xlab = "课程",
          ylab = "平均成绩",main = "各科平均成绩",
          space = 1,col = c(8),width=c(1),axis.lty = 1
  )
  dev.off()
}


#'绘制每个科目的层次等级
parintEveryClassByLevel <- function(){
  wdata <- read.csv(getFilePath("achivement2"),header = FALSE,fileEncoding="utf-8",encoding="utf-8")
  #统计每一列包含1的个数和包含2的个数和包含3的个数
  size <- length(wdata)
  result <- data.frame()
  for(col in 1:size){
    w <- wdata[,col]#获取到这一列所有值
    first <- 0
    second <- 0
    third <- 0
    ch <- numConvertToColName(col)
    level1 <- paste(ch,"1",sep="")
    level2 <- paste(ch,"2",sep="")
    level3 <- paste(ch,"3",sep="")
    s <- length(w)
    for(row in 1:s){
      target = w[row][1]
      if(target == level1){
        first <- first+1
      }
      if(target == level2){
        second <- second+1
      }
      if(target == level3){
        third <- third+1
      }
    }
    if(length(result) == 0){
      result <- data.frame(ch = c(first,second,third))
    }
    else{
      result <- cbind(result,ch = c(first,second,third))
    }
  }
  m <- as.matrix(result)
}
#'根据矩阵绘制多层次的柱状图
paintMultiLevel <- function(m){
  png(filename=paste(getImgPath(),"各科成绩水平分布.png",sep=""),width=1000,height=500)
  subject <- c("A","B","C","D","E","F","G","H","R","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","AA")
  #subject <- getAllSubject()
  barplot(height=m,names.arg = subject,main="各科成绩水平分布",col = c("red","green","blue"))
  dev.off()
}


#'绘制当置信度为0.3的时候,支持度从0.2-0.6折线图
#'通过支持度，获取条目
getItemsCount <- function(support,confidence=0.3,minlen=2){
  w <- getRules(support,confidence,minlen)
  writeData(w,paste("support",support,sep=""))
  return (length(w))
}
getDataSourceToPaintLineGraph <- function(){
  sRange <- c(0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7)
  result1 <- c() #统计置信度=0.3
  result2 <- c()#置信度=0.5
  result3 <- c()#置信度=0.7
  for(r in sRange){
    result1 <- append(result1,c(getItemsCount(r,0.3)))
  }
  for(r in sRange){
    result2 <- append(result2,c(getItemsCount(r,0.7)))
  }
  for(r in sRange){
    result3 <- append(result3,c(getItemsCount(r,0.8)))
  }
  paintLineGraph(y1=result1,y2=result2,y3=result3,x=sRange)
}
paintLineGraph <- function(y1,y2,y3,x){
  png(filename=paste(getImgPath(),"关联规则数目对比图.png",sep=""),width=1000,height=500)
  plot(x = x,y=y1,type="b",col="red",lty=1)
  lines(x=x,y=y2,type="b",col="green",lty=1)
  lines(x=x,y=y3,type="b",col="blue",lty=1)
  legend(legend=c("置信度:0.3","置信度:0.7","置信度:0.8"),col=c("red","green","blue"),x=0.6,y=2500,lty=1)
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
