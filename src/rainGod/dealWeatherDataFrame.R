#'获取2011 2012 2013 2014 2015 2016这几年的month月份下雨的日子
library(sqldf)


#'beijing tongji 7
#'shanghai 8
#'changsha 3
#'tianjin 11
countRainyDaysByCityAndMonth <- function(filename="beijing",month){
  filepath = chartr("/","\\",getwd())
  wdata <- read.csv(file=paste(filepath,"\\res\\",filename,".csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")
  count2011 <- sqldf(getSQL(year = "2011",month))[[1]]
  count2012 <- sqldf(getSQL(year = "2012",month))[[1]]
  count2013 <- sqldf(getSQL(year = "2013",month))[[1]]
  count2014 <- sqldf(getSQL(year = "2014",month))[[1]]
  count2015 <- sqldf(getSQL(year = "2015",month))[[1]]
  count2016 <- sqldf(getSQL(year = "2016",month))[[1]]
  return (c(count2011,count2012,count2013,count2014,count2015,count2016))
}

getSQL <- function(year="2011",month="08"){
  return (paste("select count(*) from wdata where date like '%",year,"-",month,"-%' and weather like '%雨%'",sep=""))
}