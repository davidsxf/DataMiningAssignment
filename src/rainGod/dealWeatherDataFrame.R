#'获取2011 2012 2013 2014 2015 2016这几年的month月份下雨的日子
library(sqldf)


#'beijing tongji 7
#'shanghai 8
#'changsha 3
#'tianjin 11
#'根据城市获取这几年中month月中下雨的天数
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

getSQL2 <- function(month="07"){
  return (paste("select count(*) from wdata where date like '%-",month,"-%' and weather like '%雨%'",sep=""))
}

#根据城市，获取2001-2017年中1-12月平均每月下雨的天数
countRainyDaysByCityAndMonthAvg <- function(filename="beijing"){
  filepath = chartr("/","\\",getwd())
  wdata <- read.csv(file=paste(filepath,"\\res\\",filename,".csv",sep=""),header=TRUE,fileEncoding="utf-8",encoding="utf-8")
  count01 <- sqldf(getSQL2("01"))[[1]] / 7 #2011-2017年1月份所有下雨的日期除以7
  count02 <- sqldf(getSQL2("02"))[[1]] / 7 #2011-2017年2月份所有下雨的日期除以7
  count03 <- sqldf(getSQL2("03"))[[1]] / 7 #2011-2017年3月份所有下雨的日期除以7
  count04 <- sqldf(getSQL2("04"))[[1]] / 6 #2011-2017年4月份所有下雨的日期除以7
  count05 <- sqldf(getSQL2("05"))[[1]] / 6 #2011-2017年5月份所有下雨的日期除以7
  count06 <- sqldf(getSQL2("06"))[[1]] / 6 #2011-2017年6月份所有下雨的日期除以7
  count07 <- sqldf(getSQL2("07"))[[1]] / 6 #2011-2017年7月份所有下雨的日期除以7
  count08 <- sqldf(getSQL2("08"))[[1]] / 6 #2011-2017年8月份所有下雨的日期除以7
  count09 <- sqldf(getSQL2("09"))[[1]] / 6 #2011-2017年9月份所有下雨的日期除以7
  count10 <- sqldf(getSQL2("10"))[[1]] / 6 #2011-2017年10月份所有下雨的日期除以7
  count11<- sqldf(getSQL2("11"))[[1]] / 6 #2011-2017年11月份所有下雨的日期除以7
  count12<- sqldf(getSQL2("12"))[[1]] / 6 #2011-2017年12月份所有下雨的日期除以7
  return (c(count01,count02,count03,count04,count05,count06,count07,count08,count09,count10,count11,count12))
}