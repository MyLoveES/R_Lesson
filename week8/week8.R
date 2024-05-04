library(readxl) 
library(dplyr)

# 获取当前已加载文件的目录
file_dir <- dirname(parent.frame(2)$ofile)
print(file_dir)
# 将工作目录设置为当前已加载文件的目录
setwd(file_dir)

spending.data <- read_xls("advertising spending 1.xls")

str(spending.data)

# scatter plot
plot(spending.data$radio, spending.data$sales)

# Correlation between radio advertising and sales
cor(spending.data$radio, spending.data$sales)

regression <- lm(sales ~ radio, data = spending.data) 
summary(regression)

regression <- lm(sales ~ radio + tv, data=spending.data) 
summary(regression)

mean(spending.data$radio)

mean(spending.data$sales)

0.19 * (23.26/14.02)

plot(spending.data$tv, spending.data$sales)

summary(spending.data$sales)

summary(spending.data$tv)

summary(spending.data$radio)

summary(spending.data$newspaper)

regression <- lm(log(sales) ~ log(radio+0.01) + log(tv) + log(newspaper), data=spending.data)
summary(regression)

regression <- lm(log(sales) ~ log(radio+0.01) + log(tv) + log(newspaper) + log(radio+0.01)*log(tv), data=spending.data)
summary(regression)

center <- function(x) { scale(x, scale = F)} # scale = F, means only center not standardize
spending.data <- spending.data %>% mutate(radio_log_centered = center(log(radio+0.01)),
                                          tv_log_centered = center(log(tv)), newspaper_log_centered = center(log(newspaper)))
regression <- lm(log(sales) ~ radio_log_centered + tv_log_centered + newspaper_log_centered +
                   radio_log_centered*tv_log_centered, data=spending.data)
summary(regression)

adstock <- function(x, rate){
  return(as.numeric(stats::filter(x=x, filter=rate, method="recursive")))
}
# filter() function from stats package applies linear filtering to a univariate time series

spending.data <- spending.data %>% mutate(tv_adstock = adstock(tv,0.1),
                                          newspaper_adstock = adstock(newspaper, 0.1), radio_adstock = adstock(radio, 0.1))

regression <- lm(log(sales) ~ log(radio+0.01) + log(tv_adstock) + log(newspaper), data=spending.data)
summary(regression)

# Total number of rows in the data frame
n <- nrow(spending.data)
# Number of rows for the training set (80% of the dataset)
n_train <- round(0.80 * n) # Training data
spending.data.train <- subset(spending.data, week <= n_train) # Holdout data
spending.data.holdout <- subset(spending.data, week > n_train)
# Estimation on training data
regression <- lm(log(sales) ~ log(radio_adstock) + log(tv_adstock) + log(newspaper_adstock), data=spending.data.train)
summary(regression)

# Predict sales on holdout data
spending.data.holdout$predicted_sales_log <-
  predict(object =regression, newdata = spending.data.holdout)
# Convert predicted log sales to actual sales
spending.data.holdout$predicted_sales <- exp(spending.data.holdout$predicted_sales_log)
# Quantify predictive accuracy: Mean Average Percentage Error (MAPE)
mape <- mean(abs((spending.data.holdout$sales -spending.data.holdout$predicted_sales)
                 /spending.data.holdout$sales))
mape # Reflects the average percentage error in a given week

# Plot actual versus predicted sales
plot(spending.data.holdout$week, spending.data.holdout$sales, type="l", col="blue") # Plot actual sales

lines(spending.data.holdout$week,spending.data.holdout$predicted_sales, type = "l", col = "red") # Add predicted sales

legend("topleft", legend=c("Actual sales", "Predicted sales"), col=c("blue", "red"), lty = 1:2, cex=0.6)