# install.packages("ggplot2") # very popular plotting library ggplot2 
# install.packages("ggthemes") # themes for ggplot2
# install.packages("xtable") # processing of regression output 
# install.packages("knitr") # used for report compilation and table display 
# install.packages("caret") # confusion matrix
# install.packages("pROC") # confusion matrix

library("ggplot2") # very popular plotting library ggplot2 
library("ggthemes") # themes for ggplot2
library("xtable") # processing of regression output 
library("knitr") # used for report compilation and table display 
library("caret") # confusion matrix
library("pROC") # confusion matrix

# 获取当前已加载文件的目录
file_dir <- dirname(parent.frame(2)$ofile)
print(file_dir)
# 将工作目录设置为当前已加载文件的目录
setwd(file_dir)

exp(0) / exp(0)+1 # computing logistic by hand, or using plogis()

plogis(-Inf) #infinitely low = likelihood 0

plogis(2) #moderate probability = 88% chance of outcome

plogis(-0.2) # weak likelihood

log(0.88 / (1-0.88)) # moderate high likelihood

qlogis(0.88) # equivalent to hand computation

RFMdata <- read.csv(file = "RFMData.csv",row.names=1) 

head(RFMdata,5)

model <- glm(Purchase~Recency+Frequency+Monetary, data=RFMdata, family = "binomial") 
output <- cbind(coef(summary(model))[, 1:4],exp(coef(model)))
colnames(output) <- c("beta","SE","z val.","Pr(>|z|)",'exp(beta)') 
kable(output,caption = "Logistic regression estimates")

# likelihood ratio test
reduced.model <- glm(Purchase ~ 1, data=RFMdata, family = "binomial") 
kable(xtable(anova(reduced.model, model, test = "Chisq")),caption = "Likelihood ratio test")

# calculate logit probabilities
RFMdata$Base.Probability <- predict(model, RFMdata, type="response") 
kable(head(RFMdata,5),row.names = TRUE)

# purchase vs. no purchase <-> p>0.5 or p<0.5
RFMdata$Predicted.Purchase <- 1*(RFMdata$Base.Probability>=0.5) 
kable(head(RFMdata,5),row.names = TRUE)

confusionMatrix(table(RFMdata$Predicted.Purchase,RFMdata$Purchase),positive = "1")

rocobj <- roc(RFMdata$Purchase, RFMdata$Base.Probability)

{plot(rocobj,legacy.axes=TRUE)
  text(0.5, 0.8, labels = sprintf("AUC = %.5f",rocobj$auc))}

# calculate new logit probabilities (Monetary+1)
RFMdata_new <- RFMdata
RFMdata_new$Monetary <- RFMdata_new$Monetary + 1 
RFMdata$New.Probability <- predict(model, RFMdata_new, type="response")

# mean predicted base probability
mean(RFMdata$Base.Probability)

# mean new predicted probability
mean(RFMdata$New.Probability)

# lift
(mean(RFMdata$New.Probability) - mean(RFMdata$Base.Probability))/mean(RFMdata$Base.Probability)

# remove predicted purchase variable
RFMdata$Predicted.Purchase <- NULL
# data
kable(head(RFMdata,5),row.names = TRUE)