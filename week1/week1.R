# 获取当前已加载文件的目录
file_dir <- dirname(parent.frame(2)$ofile)
print(file_dir)
# 将工作目录设置为当前已加载文件的目录
setwd(file_dir)

# define some variables
x <- c(2, 4, 6, 8)
xNum <- c(1, 3.14159, 5, 7)
xLog <- c(TRUE, FALSE, TRUE, TRUE)
xChar <- c("foo", "bar", "boo", "far")
xMix <- c(1, TRUE, 3, "Hello, world!")

xNum

x2 <- c(xNum, xMix) 
x2

xNum[2]


x2 <- c(x, x) 
x2+1

x2 * pi

(x+cos(0.5)) * x2

xSeq <- 1:10 # use 1:10 instead of typing 1,2,3,4 ...10.

xNum

xNum[2:4]

myStart <- 2
xNum[myStart:sqrt(myStart+7)]

xSeq

xSeq[-5:-7]

xSub <- xNum[2:4] 
xSub

xNum[c(FALSE, TRUE, TRUE, TRUE)]

xNum[xNum > 3]

my.test.scores <- c(91, NA, NA)

mean(my.test.scores)

max(my.test.scores)

mean(my.test.scores, na.rm=TRUE)

max(my.test.scores, na.rm=TRUE)

mean(na.omit(my.test.scores))

is.na(my.test.scores)

my.test.scores[!is.na(my.test.scores)]

str(xChar)

xList <- list(xNum, xChar) 
xList

str(xList)

summary(xList[[1]])

xList <- list(xNum, xChar) # method 1: create, then name 
names(xList) <- c("itemnum", "itemchar")
xList

xList <- list(itemnum=xNum, itemchar=xChar)
names(xList)
xList

xList$itemnum # method 2: $name reference

x.df <- data.frame(xNum, xLog, xChar)
x.df

x.df[2,1]

x.df1 <- data.frame(xNum, xLog, xChar, stringsAsFactors=TRUE) 
x.df1
str(x.df1)

x.df[2, ] # all of row 2 
x.df[ ,3] # all of column 3

x.df[2:3, ]
x.df[ ,1:2] # two columns
x.df[-3, ] # omit the third observation
x.df[, -2] # omit the second column

rm(list=ls()) # caution, deletes all objects!
store.num <- factor(c(3, 14, 21, 32, 54)) # store id
store.rev <- c(543, 654, 345, 678, 234) # store revenue, $1000 
store.visits <- c(45, 78, 32, 56, 34) # visits, 1000s 
store.manager <- c("Annie", "Bert", "Carla", "Dave", "Ella") 
(store.df <- data.frame(store.num, store.rev, store.visits,store.manager))

store.df$store.manager

mean(store.df$store.rev)

cor(store.df$store.rev, store.df$store.visits)

summary(store.df)

save(store.df, file="store-df-backup.RData")

rm(store.df) # caution, only if save() gave no error 
load("store-df-backup.RData")

save.image() # saves file ".RData" 
save.image("mywork_week1.RData")

load("mywork_week1.RData")

# install.packages ("readxl")
library(readxl)

deospray.data <- read_excel(path = "deospray sales.xls", sheet = "deospray") # filename, sheet name

str(deospray.data)

# View(deospray.data)

se <- function(x){sd(x) / sqrt(length(x))}

se(store.df$store.visits)
