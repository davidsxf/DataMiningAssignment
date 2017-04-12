library(arules)
getFilePath <- function(){
  return (paste(chartr("/","\\",getwd()),"\\",sep=""))
}
filepath <- paste(getFilePath(),"achivement\\resource\\groceries.csv",sep="")
groceries <- read.transactions(filepath,format = "basket",sep=",")
summary(groceries)
class(groceries)
dim(groceries)
basketSize <- size(groceries)
summary(basketSize)
sum(basketSize)
itemFreq <- itemFrequency(groceries)
itemFreq[1:5]
sum(itemFreq)
itemCount <- (itemFreq/sum(itemFreq)) * sum(basketSize)
orderedItem <- sort(itemCount,decreasing = )
orderedItem <- sort(itemCount,decreasing = T)
orderedItem[1:10]
orderedItemFreq <- sort(itemFrequency(groceries),decreasing = T)
orderedItemFreq[1:10]
itemFrequency(groceries[100:800,1:3])  
itemFrequencyPlot(groceries, support=0.07)  
itemFrequencyPlot(groceries, topN=10, horiz=T,support=0.08)  
groceries_use <- groceries[basketSize > 1]  
dim(groceries_use)
inspect(groceries[1:5]) 
image(groceries[1:10])  
image(sample(groceries,100)) 
groceryrules <- apriori(groceries,
                        parameter = 
                          list(support=0.006,confidence = 0.25, minlen = 2))
summary(groceryrules) 
inspect(groceryrules)
ordered_groceryrules <- sort(groceryrules, by="lift") 
inspect(ordered_groceryrules)
ordered_groceryrules <- sort(groceryrules, by="support") 
inspect(ordered_groceryrules)