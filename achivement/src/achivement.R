library(arules)
getFilePath <- function(filename="achivement"){
  return (paste(chartr("/","\\",getwd()),"\\achivement\\resource\\",filename,".csv",sep=""))
}

achivement <- read.transactions(getFilePath("achivement2"),format = "basket",sep=",")
itemFreq <- itemFrequency(achivement)
summary(achivement)
itemFrequencyPlot(achivement, support=0.5)  
basketSize <- size(achivement)
summary(basketSize)
sum(itemFreq)
itemCount <- (itemFreq/sum(itemFreq))*sum(basketSize) #每个item出现的次数
summary(itemCount)  
orderedItem <- sort(itemCount, decreasing = T)  
orderedItemFreq <- sort(itemFrequency(achivement), decreasing=T)  
image(sample(achivement,50))  

#'规则
achivementRules <- apriori(achivement,
                           parameter = list(
                             support=0.3,
                             confidence=0.6,minlen=1
                           ) )
summary(achivementRules)
inspect(achivementRules)