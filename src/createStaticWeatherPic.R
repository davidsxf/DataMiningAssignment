library(RColorBrewer)

#mapdata-地图数据源
#prv-传入的省会名称
#ctype-天气类型
getColor <- function(mapdata,prov,ctype){
  filepath = chartr("/","\\",getwd())  
  ADCODE99 <- read.csv(file=paste(filepath,"\\res\\","ADCODE99.csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")
  fc <- function(x){ADCODE99$ADCODE99[which(x==ADCODE99$prov)]}  #返回x在city$province的下标
  code <- sapply(prov,fc)
  
  f <- function(x,y){ifelse(x %in% y,which(y==x),0)}
  colIndex <- sapply(mapdata@data$ADCODE99,f,code)
  ctype[which(is.na(ctype))] = 19
  return (ctype[colIndex])
}


#data是已经保存好的weather.csv
createStaticWeatherPic <- function(){
  filepath = chartr("/","\\",getwd())
  output = TRUE
  path = filepath
  data <- read.csv(file=paste(filepath,"\\weather\\","weather20170328.csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")#读取详细分类的天气
  
  temp<-data$code #已经创建好的weather/weather.csv里面天气的code
  colors <- c(rev(brewer.pal(9,"Blues")),
              rev(c('#b80137','#8c0287','#d93c5d','#d98698','#f6b400','#c4c4a7','#d6d6cb','#d1b747','#ffeda0'))) #对应18中天气的颜色
  map <- createMap() #创建地图
  pictitle <- '天气概况' #标题
  wpicname <- paste(format(date,"%Y%m%d"),"weather.png",sep="") 
  sign <- ''
  colors <- rev(colors) #翻转数组
  
  #读取天气代码文件
  wcode <- read.csv(file=paste(filepath,"\\res\\","WEATHERCODE.csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")#读取详细分类的天气
  wpcode <- read.csv(file=paste(filepath,"\\res\\","WEATHERTYPE.csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")#读取大类的天气
  ctype <- sapply(temp,function(x){wcode$type[which(x==wcode$code)]}) #根据天气代码获取天气的type
  
  if(output)
    png(file=paste(path,"\\weather\\",wpicname,sep=""),width = 900,height=900)
  
  layout(matrix(data=c(1,2),nrow=1,ncol=2),widths=c(8,1),heights=c(1,2))
  par(mar=c(0,0,3,12),oma=c(0.2,0.2,0.2,0.2),mex=0.3)
  
  plot(map,border="white",col=colors[getColor(map,data$province,ctype)])# 地图和天气可视化
  points(data$long,data$lat,pch=19,col=rgb(0,0,0,0.3),cex=0.8)    # 标出采样城市
  text(data$long,data$lat,data$zhname,ps=0.1)
  
  #===============图片中辅助文字====================# 
  if(FALSE){
    grid()
    axis(1,lwd = 0);axis(2,lwd = 0);axis(3,lwd = 0);axis(4,lwd = 0)
  }
  text(105,54,format(date,"%Y-%m-%d"))
  text(98,65,'Data mining Assignment')
  text(120,-8,paste('',format(date, "%Y-%m-%d %H:%M")),cex=0.8)

  #===============文字说明====================# 
  for(row in 1:nrow(data)){
    name <- as.character(data$zhname[row])
    label <- wpcode$alias[wpcode$type==ctype[row]]
    x1 <- ceiling(row/7)
    x2 <- ifelse(row%%7==0,7,row%%7)
    x3 <- ctype[row]
    fontCol <- '#000000'
    if(x3<=5) fontCol <- head(colors,1)
    if(x3>=12) fontCol <- tail(colors,1)
    text(68+x1*11,17-x2*3,paste(name,' ',label,sign,sep=""),col=fontCol)
  }
  
  #===============图例====================#
  par(mar=c(5,0,15,10))
  image(x=1,y=1:length(colors),z=t(matrix(1:length(colors))),col=rev(colors),axes=FALSE,xlab = "",ylab = "",xaxt="n")
  axis(4,at=1:(nrow(wpcode)-1),labels = rev(wpcode$alias)[-1],col="white",las=1)
  abline(h=c(1:(nrow(wpcode)-2)+0.5),col="white",lwd=2,xpd=FALSE)
  if(output)
    dev.off()
  
  
  
}