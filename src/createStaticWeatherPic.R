library(RColorBrewer)

#mapdata-地图数据源
#prv-传入的省会名称
#ctype-天气类型
getColor <- function(mapdata,prov,ctype){
  #filepath = chartr("/","\\",getwd())  
  #city <- read.csv(file=paste(filepath,"\\res\\","CITYWORID.csv",sep=""),header=FALSE,fileEncoding="utf-8",encoding="utf-8")
  #names(city) <- c("enname","woeid","zhname","province","long","lat") #为数据集命名
  #fc <- function(x){city$city[which(x==city$province)]}  #返回x在city$province的下标
  
  f <- function(x,y){ifelse(x %in% y,which(y==x),0)}
  colIndex <- sapply(mapdata$data$NAME,f,prov)
  #ctype[which(is.na(ctype))] = 19
  #r = ctype[colIndex]
  #if(is.na(r))
   # r = 19
  colIndex
  return (colIndex)
}

#data是已经保存好的weather.csv
createStaticWeatherPic <- function(data=data,output=FALSE,path=""){
  temp<-data$code #已经创建好的weather/weather.csv里面天气的code
  colors <- c(rev(brewer.pal(9,"Blues")),
              rev(c('#b80137','#8c0287','#d93c5d','#d98698','#f6b400','#c4c4a7','#d6d6cb','#d1b747','#ffeda0'))) #对应18中天气的颜色
  map <- createMap() #创建地图
  pictitle <- '天气概况' #标题
  wpicname <- paste(format(date,"%Y%m%d"),"weather.png",sep="") 
  sign <- ''
  colors <- rev(colors) #翻转数组
  
  #读取天气代码文件
  filepath = chartr("/","\\",getwd())  
  wcode <- read.csv(file=paste(filepath,"\\res\\","WEATHERCODE.csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")#读取详细分类的天气
  wpcode <- read.csv(file=paste(filepath,"\\res\\","WEATHERTYPE.csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")#读取大类的天气
  ctype <- sapply(temp,function(x){wcode$type[which(x==wcode$code)]}) #根据天气代码获取天气的type
  
  if(output)
    png(file=paste(path,ofile,sep=""),width = 600,height=600)
  
  layout(matrix(data=c(1,2),nrow=1,ncol=2),widths=c(8,1),heights=c(1,2))
  par(mar=c(0,0,3,12),oma=c(0.2,0.2,0.2,0.2),mex=0.3)
  
  plot(map,border="white",col=colors[getColor(map,data$province,ctype)])# 地图和天气可视化
  points(data$long,data$lat,pch=19,col=rgb(0,0,0,0.3),cex=0.8)    # 标出采样城市
}