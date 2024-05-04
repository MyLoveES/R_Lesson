library(lattice) 
library(multcomp)
library(dplyr)

# 获取当前已加载文件的目录
file_dir <- dirname(parent.frame(2)$ofile)
print(file_dir)
# 将工作目录设置为当前已加载文件的目录
setwd(file_dir)

ad.df <- read.csv("Data_Compare_Groups.csv", stringsAsFactors = TRUE) 

summary(ad.df)

str(ad.df)

mean(ad.df$seconds_spent[ad.df$condition == "treatment"])

# We could further narrow the cases to Moving up respondents who are in the treatment condition:
mean(ad.df$seconds_spent[ad.df$condition == "treatment" & ad.df$segment == "Moving up"])

aggregate(ad.df$seconds_spent, list(ad.df$condition), mean)

aggregate(seconds_spent ~ condition, data = ad.df, mean)

aggregate(seconds_spent ~ segment + condition, data = ad.df, mean)

agg.data <- aggregate(seconds_spent ~ segment + condition, data = ad.df, mean)

table(ad.df$condition, ad.df$like)

table(ad.df$segment, ad.df$like)

aggregate(kids ~ segment, data = ad.df, sum)

histogram(~ like | condition, data = ad.df)

histogram(~ like | condition, data = ad.df, type = "count",
          col = c("burlywood", "darkolivegreen") # add colours
)

histogram(~ like | segment + condition, data = ad.df)

ad.mean <- aggregate(seconds_spent ~ condition, data = ad.df, mean)
barchart(seconds_spent ~ condition, data = ad.mean, col = "grey")

ad.seconds.agg <- aggregate (seconds_spent ~ condition + segment, data = ad.df, mean) 
barchart(seconds_spent ~ condition, data = ad.seconds.agg ,groups = segment, auto.key=TRUE)

boxplot(seconds_spent ~ segment, data= ad.df, ylab = "s=total seconds spent (s)")

bwplot(condition ~ seconds_spent, data = ad.df, horizontal = TRUE, xlab = "Total seconds spent")

bwplot(condition ~ seconds_spent | segment, data = ad.df, horizontal = TRUE, xlab = "seconds_spent")

table(ad.df$like, ad.df$condition)

chisq.test(table(ad.df$like, ad.df$condition))

t.test(seconds_spent ~ condition, data = ad.df)

t.test(seconds_spent ~ condition, data = subset(ad.df, segment == "Travelers"))

ad.aov.con <- aov(seconds_spent ~ condition, data = ad.df) 
anova(ad.aov.con)

anova(aov(seconds_spent ~ segment + condition, data = ad.df)) # combine two commands

ad.aov <- aov (seconds_spent ~ 0 + segment, data = ad.df) 
glht(ad.aov)

plot(glht(ad.aov),
     xlab = "Total seconds spent", main = "Average seconds spent by Segment (95% CI)", cex.axis = 0.8)

ad.df.reg <- ad.df %>%
  mutate(dummy_condition = ifelse(condition == "treatment",1,0),
         dummy_s = ifelse(segment == "Suburb mix",1,0), dummy_u = ifelse(segment == "Urban hip",1,0), dummy_t = ifelse(segment == "Travelers",1,0))

regression <- lm(seconds_spent ~ dummy_condition, data = ad.df.reg) 
summary(regression)

regression <- lm(seconds_spent ~ dummy_s + dummy_u + dummy_t, data = ad.df.reg) 
summary(regression)

panel.df.raw <- read.csv("Data_Panel.csv", stringsAsFactors = TRUE) 
str(panel.df.raw)

panel.df<-panel.df.raw %>%
  mutate(time = ifelse(panel.df.raw$year >= 2014, 1, 0))

panel.df<-panel.df %>%
  mutate(treatment = ifelse(panel.df$market == "E" |
                              panel.df$market== "F" |
                              panel.df$market == "G", 1, 0))

panel.df<-panel.df %>% mutate(did = time * treatment)
str(panel.df)

didreg <- lm(profit ~ treatment + time + did, data = panel.df) 
summary(didreg)