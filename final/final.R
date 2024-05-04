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
library("corrplot")
library("gplots")
library("nFactors")
library("ggplot2") # very popular plotting library ggplot2 
library("ggthemes") # themes for ggplot2
library("xtable") # processing of regression output 
library("knitr") # used for report compilation and table display 
library("caret") # confusion matrix
library("pROC") # confusion matrix
library("readxl") # used for report compilation and table display 
library("ggplot2") # confusion matrix
library("dplyr") # confusion matrix
library("xtable") # processing of regression output 
library("knitr") # used for report compilation and table display 
library("ggplot2") # very popular plotting library ggplot2 
library("mlogit") # multinomial logit
library("caret") # ConfusionMatrix
library("arules") # processing of regression output 
library("arulesViz") # used for report compilation and table display 
library("grid") # very popular plotting library ggplot2 
library("car") # multinomial logit
library("plotly") # ConfusionMatrix
library('readxl') 
library('dplyr')

# set cur dir as workdir
file_dir <- dirname(parent.frame(2)$ofile)
print(file_dir)
setwd(file_dir)

# 1. manage customer hierarchical

## 1.1 segmentation
seg.df <- read.csv("1_demographics.csv", stringsAsFactors = TRUE)
head(seg.df, n = 5)
summary(seg.df, digits = 2)

### 1.1.1 remove consumer_id in data set, and set consumer_id as row name
rownames(seg.df) <- seg.df[, 1]
seg.df <- seg.df[, -1]
head(seg.df, n = 5)

### 1.1.2 Hierarchical clustering: hclust()
seg.dist <- dist(seg.df)
as.matrix(seg.dist)[1:7, 1:7]
seg.hc <- hclust(seg.dist, method="complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h = 4)$lower[[1]])
plot(seg.hc)
rect.hclust(seg.hc, k=4, border = "red")
seg.hc.segment <- cutree(seg.hc, k=4) #membership vector for 4 groups 
table(seg.hc.segment) #counts
clusplot(seg.df, seg.hc.segment,
         color = TRUE, #color the groups
         shade = TRUE, #shade the ellipses for group membership
         labels = 4, #label only the groups, not the individual points 
         lines = 0, #omit distance lines between groups
         main = "Hierarchical cluster plot" # figure title
)
aggregate(seg.df, list(seg.hc.segment), mean)
boxplot(seg.df$Salary ~ seg.hc.segment, ylab = "Salary", xlab = "Cluster")
boxplot(seg.df$Employment ~ seg.hc.segment, ylab = "Employment", xlab = "Cluster")

### 1.1.3 Mean-based clustering: kmeans()

set.seed(96743)
seg.k <- kmeans(seg.df, centers = 4) #use standardized variables
table(seg.k$cluster)
aggregate(seg.df, list(seg.k$cluster), mean)
boxplot(seg.df$Employment ~ seg.k$cluster, ylab = "Employment", xlab = "Cluster")
boxplot(seg.df$Education ~ seg.k$cluster, ylab = "Education", xlab = "Cluster")
clusplot(seg.df, seg.k$cluster,
         color = TRUE,
         shade = TRUE,
         labels = 4,
         lines = 0,
         main = "K_means cluster plot",
)

### 1.1.4 Model-based clustering: Mclust()
seg.mc <- Mclust(seg.df) 
summary(seg.mc)
seg.mc6 <- Mclust(seg.df, G=6) 
summary(seg.mc6)
BIC(seg.mc, seg.mc6)
seg.mc4 <- Mclust(seg.df, G=4) 
summary(seg.mc4)
BIC(seg.mc, seg.mc4)
table(seg.mc$classification)
aggregate(seg.df, list(seg.mc$classification), mean)
clusplot(seg.df, seg.mc$classification, color = TRUE, shade = TRUE,
         labels = 4, lines = 0, main = "Model-based cluster plot")
boxplot(seg.df$Education ~ seg.mc$classification, ylab = "Education", xlab = "Cluster")

