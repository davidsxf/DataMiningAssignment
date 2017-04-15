library(readr)
#'若考试成绩（kscj）<60，量化为1；
#'60≤kscj<70量化为2；
#'70≤kscj<80量化为3；
#'kscj≥80量化为4
makeLevel <- function(score = 60,ch = "A"){
  if(score <= 70)
    return (paste(ch,"1",sep=""))
  if(score > 70 && score < 89)
    return (paste(ch,"2",sep=""))
  if(score >= 89)
    return (paste(ch,"3",sep=""))
  return (0)
}

#'类似于excel的列数和列明的转化
numConvertToColName <- function(n=1){
  if(n==1){return ("A")}
  if(n==2){return ("B")}
  if(n==3){return ("C")}
  if(n==4){return ("D")}
  if(n==5){return ("E")}
  if(n==6){return ("F")}
  if(n==7){return ("G")}
  if(n==8){return ("H")}
  if(n==9){return ("I")}
  if(n==10){return ("J")}
  if(n==11){return ("K")}
  if(n==12){return ("L")}
  if(n==13){return ("M")}
  if(n==14){return ("N")}
  if(n==15){return ("O")}
  if(n==16){return ("P")}
  if(n==17){return ("Q")}
  if(n==18){return ("R")}
  if(n==19){return ("S")}
  if(n==20){return ("T")}
  if(n==21){return ("U")}
  if(n==22){return ("V")}
  if(n==23){return ("W")}
  if(n==24){return ("X")}
  if(n==25){return ("Y")}
  if(n==26){return ("Z")}
  if(n==27){return ("AA")}
  if(n==28){return ("AB")}
  if(n==29){return ("AC")}
  if(n==30){return ("AD")}
  if(n==31){return ("AE")}
  if(n==32){return ("AF")}
  if(n==33){return ("AG")}
  if(n==34){return ("AH")}
  if(n==35){return ("AI")}
  if(n==36){return ("AJ")}
}

#'判断一个data.frame是不是包含非法字符
isContainsUnNum <- function(wdata){
  size <- length(wdata)
  for(c in 1:size){
    if("integer" == class(wdata[,c])){
      
    }else{
      print(c)
      return (FALSE)
    }
  }
  return (TRUE)
}

#'如果一行不够36或者包含非法字符，则去除该行
filterData <- function(wdata){
  size <- length(wdata)
  for(row in 1:size){
    w <- wdata[row,]#一行数据
    if(length(w) != 34 || !isContainsUnNum(w)){
      drop(w)
    }
  }
  return (wdata)
}

achivement <- read.csv(getFilePath("achivement"),header=TRUE,fileEncoding="utf-8",encoding="utf-8")
#achivement <- filterData(achivement)
size <- length(achivement)
result <- do.call(rbind,lapply(achivement[,1],makeLevel,numConvertToColName(1)))
for(col in 2:size){
  wdata <- do.call(rbind,lapply(achivement[,col],makeLevel,numConvertToColName(col)))
  result <- cbind(result,wdata)
}

colsname = c()
for(i in 1:size){
  colsname = append(colsname,c(numConvertToColName(i)))
}

write.csv(result,getFilePath("achivement2"),row.names=TRUE,fileEncoding = "utf-8")