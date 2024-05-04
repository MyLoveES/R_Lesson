library("arules") # processing of regression output 
library("arulesViz") # used for report compilation and table display 
library("grid") # very popular plotting library ggplot2 
library("car") # multinomial logit
library("plotly") # ConfusionMatrix

# 获取当前已加载文件的目录
file_dir <- dirname(parent.frame(2)$ofile)
print(file_dir)
# 将工作目录设置为当前已加载文件的目录
setwd(file_dir)

data("Groceries")
summary(Groceries)


inspect(head(Groceries,3))

groc.rules <- apriori(Groceries, parameter = list(supp=0.01, conf=0.3, target="rules"))

inspect(subset(groc.rules, lift > 3))

plot(groc.rules)

plot(groc.rules, engine = "plotly")

groc.hi <- head(sort(groc.rules, by="lift"), 15) 
inspect(groc.hi)

plot(groc.hi, method="graph")

retail.raw <- readLines("retail.dat")
head(retail.raw)
tail(retail.raw)

summary(retail.raw)

retail.list <- strsplit(retail.raw, " ")

names(retail.list) <- paste("Trans", 1:length(retail.list))

str(retail.list)

some(retail.list) #note: random sample; your results may vary

rm(retail.raw)

retail.trans <- as(retail.list, "transactions") #takes a few seconds 
summary(retail.trans)

rm(retail.list) #remove retail.list as it is not needed anymore.