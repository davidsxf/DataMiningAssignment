sortByData <- function(){
  wdata <- read.csv(getFilePath(),header = FALSE,fileEncoding = "utf-8",encoding = "utf-8")
  col <- length(wdata) #列数
  row <- length(wdata[,1])#行数
  everyItems <- c()
  for(i in 1:row){
    for(j in 1:col){
      everyItems <- append(everyItems,c(wdata[i,][,j]))
    }
  }
  sortedGrade <- sort(everyItems)
  write.csv(sortedGrade, file=getFilePath("sortedGrade")) 
}