library(rvest)
#通过城市拼音和日期查询天气
getWeatherByCityNameAndDate <- function(city="beijing",date="2011.01.01"){
    row <- substr(date,9,10)
    row <- as.numeric(row) + 1 #get the rowth number
    date <- paste(substr(date,1,4),substr(date,6,7),sep="")
    ulchild <- paste("ul:nth-child(",row,")",sep="")
    url <- paste("http://lishi.tianqi.com/",city,"/",date,".html",sep="");
    freak <- html_session(url)
    table <- freak %>% html_nodes("div[id='tool_site']")
    date <- table %>% html_nodes(ulchild) %>% html_nodes("li:nth-child(1)") %>%  html_text() #日期
    high <- table %>% html_nodes(ulchild) %>% html_nodes("li:nth-child(2)") %>%  html_text() #最高气温
    low <- table %>% html_nodes(ulchild) %>% html_nodes("li:nth-child(3)") %>%  html_text() #最低气温
    weather <- table %>% html_nodes(ulchild) %>% html_nodes("li:nth-child(4)") %>%  html_text() #天气
    windDirec <- table %>% html_nodes(ulchild) %>% html_nodes("li:nth-child(5)") %>%  html_text() #风向
    windlevel <- table %>% html_nodes(ulchild) %>% html_nodes("li:nth-child(6)") %>%  html_text() #风力
    isRainy <- grep("雨",weather)
    if(length(isRainy) > 0){
      isRainy = "有雨"
    }else{
      isRainy = "没雨"
    }
    cbind(high,low,weather,windDirec,windlevel,isRainy)
}