dealWithRules <- function(){
  sRange <- c(0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7)
  result <- c()
  for(s in sRange){
    filename <- paste("support",s,sep="")
    wdata <- read.csv(getFilePath(filename),header = TRUE,fileEncoding = "utf-8",encoding = "utf-8")
    rules <- wdata["rules"] #选择关联规则这一列
    len <- length(rules[,1])#获取关联规则个数
    for(row in 1:len){
      rule <- as.character(rules[row,])
    }
  }
  
}


isValid <- function(ruleList){
  left <- ruleList[[1]][1]#获取LSH
  right <- ruleList[[1]][2]#获取RSH
  left <- substr(left,2,nchar(left)-1)#去除了花括号
  right <- substr(right,2,nchar(right)-1)#出除了花括号
  leftList <- unlist(strsplit(left,","))#选取左侧的最小值
  if(min(leftList) < right)
    return (FALSE)
  else
    return (TRUE)
}