library(recharts)
library(htmlwidgets)
#'wdata:天气数据
#'type:high-白天 low-夜间
#'output:false
#'path-输出路径
createHTMLWithWeather <- function(wdata,type="high",output=FALSE){
  path = paste(chartr("/","\\",getwd()),"\\html\\",sep="");
  date <- format(Sys.time(),"%Y-%m-%d")
  if(type == 'day'){
    #白天
    df <- wdata[,c('province','high')]
    names(df) <- c('province','气温')
    title <- paste(date,"中国各省白天天气",sep="")
    ofile <- paste(date,"dayweather.html",sep="")
  } else if(type=="low"){
    df <- wdata[,c('province','low')]
    names(df) <- c('province','气温')
    title <- paste(date,"中国各省夜间天气",sep="")
    ofile <- paste(date,"nightweather.html",sep="")
  }
  
  df[,1] <- substr(df[,1],0,2) #数据格式整理
  #df[which(df$province == '黑龙'),]$province <- '黑龙江'
  #df[which(df$province == '内蒙'),]$province <- '内蒙古'
  df$province[which(df$province == '黑龙')] <- '黑龙江'
  df$province[which(df$province == '内蒙')] <- '内蒙古'
  
  recharts.emap <- eMap(df,namevar=1,datavar=2,title=title) #数据JSON化处理
  if(output){
    #recharts.emap$outList[c('chartid','type')] <- NULL
    #writeLines(unlist(recharts.emap$outList),paste(path,ofile,sep=""))
    
    #htmlwidgets::saveWidget(recharts.emap,paste(path,ofile,sep=""))
  }else{
    #plot(recharts.emap)
    recharts.emap
  }
}