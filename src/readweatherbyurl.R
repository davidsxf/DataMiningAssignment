library(RCurl) 
library(XML)
getWeatherData <- function(){
  freak <- html_session("http://15tianqi.cn/2011beijing1yuetianqi/")
  table <- freak %>% html_nodes("table[class='tablelist']")
  table %>% html_nodes("td:nth-child(2)") %>% html_text()
}