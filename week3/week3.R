library("corrplot")
library("gplots")
library("nFactors")

brand.ratings <- read.csv("Data_Factor_Analysis.csv", stringsAsFactors = TRUE) 
head(brand.ratings)

summary(brand.ratings)

str(brand.ratings)

brand.sc <- brand.ratings
brand.sc[,1:9] <- scale (brand.ratings[,1:9])
#we select all rows and the first 9 columns as the 10th column is a factor variable. 
summary(brand.sc)

cor(brand.sc[,1:9])

corrplot(cor(brand.sc[,1:9]))

corrplot(cor(brand.sc[,1:9]), order = "hclust")

brand.mean <- aggregate(. ~ brand, data=brand.sc, mean) 

brand.mean

rownames(brand.mean) <- brand.mean[, 1]
# Use brand for the row name
brand.mean <- brand.mean [, -1]
# Remove the brand name column by not selecting the first column # Negative index is used to exclude the variable
brand.mean

heatmap.2(as.matrix(brand.mean),main = "Brand attributes", trace = "none", key = FALSE, dend = "none") #turn off some options

brand.pc<- princomp(brand.mean, cor = TRUE)
#We added "cor =TRUE" to use correlation-based one. 
summary(brand.pc)

plot(brand.pc,type="l") # scree plot

loadings(brand.pc) # pc loadings

biplot(brand.pc, main = "Brand positioning")

brand.mean["c",] - brand.mean["e",]

colMeans(brand.mean[c("b","c","f","g"),]) - brand.mean["e",]

nScree(brand.mean)

eigen(cor(brand.mean))

brand.fa <- factanal(brand.mean, factors = 2, rotation = "varimax", scores = "regression")

brand.fl<- brand.fa$loadings[, 1:2]

plot(brand.fl,type="n") # set up plot 

text(brand.fl,labels=names(brand.mean),cex=.7)

plot(brand.fl,type="n") # set up plot 
text(brand.fl,labels=rownames(brand.mean),cex=.7)