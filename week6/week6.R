library("xtable") # processing of regression output 
library("knitr") # used for report compilation and table display 
library("ggplot2") # very popular plotting library ggplot2 
library("mlogit") # multinomial logit
library("caret") # ConfusionMatrix

# 获取当前已加载文件的目录
file_dir <- dirname(parent.frame(2)$ofile)
print(file_dir)
# 将工作目录设置为当前已加载文件的目录
setwd(file_dir)

cbc.df<-read.csv("Data_Conjoint_Choice.csv", stringsAsFactors = TRUE) 
str(cbc.df)
head(cbc.df)

summary(cbc.df)

xtabs(Choice~Price, data=cbc.df)

xtabs(Choice~Size, data=cbc.df)

cbc.df$Brand <- relevel(cbc.df$Brand, ref = "Nexus") 
cbc.df$Size <- relevel(cbc.df$Size, ref = "sz7inch") 
cbc.df$Storage <- relevel(cbc.df$Storage, ref = "st16gb") 
cbc.df$Ram <- relevel(cbc.df$Ram, ref = "r1gb") 
cbc.df$Battery <- relevel(cbc.df$Battery, ref = "b7h")

library(dfidx) #install if needed 
cbc.mlogit <- dfidx(cbc.df, choice="Choice", idx=list(c("ChoiceSetId", "ConsumerId"), "AlternativeIdInSet"))

model<-mlogit(Choice ~ 0+Brand+Size+Storage+Ram+Battery+Price, data=cbc.mlogit) 
kable(summary(model)$CoefTable)

model.constraint <-mlogit(Choice ~ 0+Brand, data = cbc.mlogit)

lrtest(model, model.constraint)

kable(head(predict(model,cbc.mlogit)))

predicted_alternative <- apply(predict(model,cbc.mlogit),1,which.max) 
selected_alternative <- cbc.mlogit$AlternativeIdInSet[cbc.mlogit$Choice>0] 
confusionMatrix(table(predicted_alternative,selected_alternative),positive = "1")

predict.share <- function(model, d) {
  temp <- model.matrix(update(model$formula, 0 ~ .), data = d)[,-1] # generate dummy matri 
  u <- temp%*%model$coef[colnames(temp)] # calculate utilities; %*% is matrix multiplicati 
  probs <- t(exp(u)/sum(exp(u))) # calculate probabilities
  colnames(probs) <- paste("alternative", colnames(probs))
  return(probs)
}
# hypothetical base market structure with 4 alternatives in the market
d.base <- cbc.df[c(44,34,33,40),c("Brand","Size","Storage","Ram", "Battery","Price")] 
d.base <- cbind(d.base,as.vector(predict.share(model,d.base)))
colnames(d.base)[7] <- "Predicted.Share" 
rownames(d.base) <- c()
kable(d.base)

# hypothetical market structure after Galaxy gets a RAM upgrade
d.new <- d.base
d.new[2, 'Ram'] <- "r4gb"
d.new$Predicted.Share <- as.vector(predict.share(model,d.new)) 
kable(d.new)

(coef(model)["BrandiPad"]-coef(model)["BrandGalaxy"]) / (-coef(model)["Price"])


coef(model)["Sizesz9inch"] / (-coef(model)["Price"])