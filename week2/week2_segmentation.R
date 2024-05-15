# install.packages ("readxl")
# install.packages ("psych")
# install.packages ("car")
# install.packages ("gpairs")
# install.packages ("grid")
# install.packages ("lattice")
# install.packages ("corrplot")
# install.packages ("gplots")
# install.packages ("mclust")
# install.packages ("cluster")

library("readxl")
library("psych")
library("car")
library("gpairs")
library("grid")
library("lattice")
library("corrplot")
library("gplots")
library("mclust")
library("cluster")

# 获取当前已加载文件的目录
file_dir <- dirname(parent.frame(2)$ofile)
print(file_dir)
# 将工作目录设置为当前已加载文件的目录
setwd(file_dir)

seg.df <- read.csv("Data_Segmentation.csv", stringsAsFactors = TRUE)

head(seg.df, n=8)

seg.df$gender <- ifelse(seg.df$gender=="Male",0,1)
seg.df$ownHome <- ifelse(seg.df$ownHome == "ownNo", 0,1)
seg.df$subscribe <- ifelse(seg.df$subscribe == "subNo", 0,1)
head(seg.df)
summary(seg.df, digits = 2)
str(seg.df)

seg.df.sc <- seg.df
seg.df.sc[, c(1,3,4)] <- scale(seg.df[, c(1,3,4)])
# We only need to standardize continuous variables.
head(seg.df.sc)
str(seg.df.sc)

summary(seg.df.sc, digits = 2)

seg.dist <- dist(seg.df.sc) 
as.matrix(seg.dist)[1:5, 1:5]

seg.hc <- hclust(seg.dist, method="complete") 
seg.hc

plot(seg.hc)

plot(cut(as.dendrogram(seg.hc), h = 4)$lower[[1]])

seg.df[c(156, 152),] #similar
seg.df[c(156, 183),] #less similar

plot(seg.hc)
rect.hclust(seg.hc, k=4, border = "red")

seg.hc.segment <- cutree(seg.hc, k=4) #membership vector for 4 groups 
table(seg.hc.segment) #counts

library(cluster) 
clusplot(seg.df, seg.hc.segment,
         color = TRUE, #color the groups
         shade = TRUE, #shade the ellipses for group membership
         labels = 4, #label only the groups, not the individual points 
         lines = 0, #omit distance lines between groups
         main = "Hierarchical cluster plot" # figure title
)

aggregate(seg.df, list(seg.hc.segment), mean)

boxplot(seg.df$income ~ seg.hc.segment, ylab = "Income", xlab = "Cluster")

set.seed(96743)
seg.k <- kmeans(seg.df.sc, centers = 4) #use standardized variables

aggregate(seg.df, list(seg.k$cluster), mean)

boxplot(seg.df$income ~ seg.k$cluster, ylab = "Income", xlab = "Cluster")
boxplot(seg.df$kids ~ seg.k$cluster, ylab = "kids", xlab = "Cluster")

clusplot(seg.df, seg.k$cluster,
         color = TRUE,
         shade = TRUE,
         labels = 4,
         lines = 0,
         main = "K_means cluster plot",
)

library(mclust) #install if needed
seg.mc <- Mclust(seg.df.sc) 
summary(seg.mc)

seg.mc4 <- Mclust(seg.df.sc, G =4) #specifying the number of clusters 
summary(seg.mc4)

BIC(seg.mc, seg.mc4)

aggregate(seg.df, list(seg.mc$classification), mean)
library(cluster)
clusplot(seg.df, seg.mc$classification, color = TRUE, shade = TRUE,
         labels = 4, lines = 0, main = "Model-based cluster plot")