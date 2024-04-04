# install.packages ("readxl")
# install.packages ("psych")
# install.packages ("car")
# install.packages ("gpairs")
# install.packages ("grid")
# install.packages ("lattice")
# install.packages ("corrplot")
# install.packages ("gplots")

library("readxl")
library("psych")
library("car")
library("gpairs")
library("grid")
library("lattice")
library("corrplot")
library("gplots")

# 获取当前已加载文件的目录
file_dir <- dirname(parent.frame(2)$ofile)
print(file_dir)
# 将工作目录设置为当前已加载文件的目录
setwd(file_dir)

store.df <- read.csv("Data_Descriptive.csv", stringsAsFactors=TRUE)
str(store.df)

table(store.df$p1price)

p1.table <- table(store.df$p1price) 
p1.table

str(p1.table)

plot(p1.table)

table(store.df$p1price, store.df$p1prom)

p1.table2 <- table(store.df$p1price, store.df$p1prom) 
p1.table2[ ,2] / (p1.table2[ ,1] + p1.table2[ ,2])

plot(p1.table2)

min(store.df$p1sales)
max(store.df$p1sales)
mean(store.df$p1prom)
median(store.df$p2sales)
var(store.df$p1sales)
sd(store.df$p1sales)
IQR(store.df$p1sales)
mad(store.df$p1sales)

quantile(store.df$p1sales, probs = c(0.25, 0.5, 0.75))
quantile(store.df$p1sales, probs = c(0.05, 0.95)) # central 90% data
quantile(store.df$p1sales, probs = 0:10/10)
quantile(store.df$p1sales, probs = seq(from=0, to=1, by=0.1))

mysummary.df <- data.frame(matrix(NA, nrow=2, ncol=2)) # 2 by 2 empty matrix 
names(mysummary.df) <- c("Median Sales", "IQR") # name columns 
rownames(mysummary.df) <- c("Product 1", "Product 2") # name rows 
mysummary.df["Product 1", "Median Sales"] <- median(store.df$p1sales) 
mysummary.df["Product 2", "Median Sales"] <- median(store.df$p2sales) 
mysummary.df["Product 1", "IQR"] <- IQR(store.df$p1sales) 
mysummary.df["Product 2", "IQR"] <- IQR(store.df$p2sales)
mysummary.df

summary(store.df)

summary(store.df$Year)

summary(store.df, digits = 2) # 保留小数点后两位

library(psych) # install if needed 
describe(store.df)

describe(store.df[,c(2, 4:9)])
dim(store.df)
head(store.df)
tail(store.df)
some(store.df)
str(store.df)
summary(store.df)
describe(store.df)

apply(store.df[ 2:9], MARGIN = 2, FUN = mean)
apply(store.df[ , 2:9], 2, sum)
apply(store.df[ , 2:9], 2, sd)

hist(store.df$p1sales)

# 添加标题、x轴y轴说明
hist(store.df$p1sales,
     main = "Product 1 Weekly Sales Frequencies, All Stores", 
     xlab = "Product 1 Sales (Units)",
     ylab = "Count")

colors()

hist(store.df$p1sales,
     main = "Product 1 Weekly Sales Frequencies, All Stores", xlab = "Product 1 Sales (Units)",
     ylab = "Count",
     breaks = 30, # more columns
     col = "lightblue" # colore the bars
)

hist(store.df$p1sales,
     main = "Product 1 Weekly Sales Frequencies, All Stores", xlab = "Product 1 Sales (Units)",
     ylab = "Count",
     breaks = 30,
     col = "lightblue",
     freq = FALSE, # means plot density, not counts
     xaxt="n" # means x-axis tick mark is set to "none"
)

hist(store.df$p1sales,
     main = "Product 1 Weekly Sales Frequencies, All Stores", xlab = "Product 1 Sales (Units)",
     ylab = "Count",
     breaks = 30,
     col = "lightblue",
     freq = FALSE, # means plot density, not counts
     xaxt="n" # means x-axis tick mark is set to "none"
)

# side=1 x轴; side=2 y轴; at=sqp() 修改间隔
axis(side = 1, at=seq(60, 300, by=20)) # add "60", "80", ...

lines(density(store.df$p1sales, bw=10), # "bw=..." adjusts the smoothing
      type="l", col = "darkred", lwd=2) # lwd=line width

lines(density(store.df$p1sales, bw=10), # "bw=..." adjusts the smoothing
      type="o", col = "darkred", lwd=2) # lwd=line width
