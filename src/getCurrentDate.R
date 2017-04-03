#获取当前时间
getCurrentDate <- function(){
  date <- Sys.time()
  now <- format(date,"%Y%m%d");
  return (now)
}