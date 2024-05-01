library("readxl") # used for report compilation and table display 
library("ggplot2") # confusion matrix
library("dplyr") # confusion matrix

# 获取当前已加载文件的目录
file_dir <- dirname(parent.frame(2)$ofile)
print(file_dir)
# 将工作目录设置为当前已加载文件的目录
setwd(file_dir)

CLV.df <- read_excel("Data_CLV.xlsx", sheet = "Ex2") 
CLV.df

summary(CLV.df)

ggplot(CLV.df, aes(x = t, y = active)) +
  geom_line() + ylab("Customer") +
  xlab("Period") + ggtitle("Active Customer Evolution")

ggplot(CLV.df, aes(x = t, y = r)) + geom_line() + ylab("Customer") + xlab("Period") +
  ggtitle("Retention Ratio Evolution")

CLV.df$CLV <- (CLV.df$p-CLV.df$c)*CLV.df$r^CLV.df$t/(1+CLV.df$i)^CLV.df$t 
CLV.df

ggplot(CLV.df, aes(x = t, y = CLV)) + geom_line() + ggtitle("CLV evolution") + ylab("CLV") + xlab("Period")

CLV <- apply(CLV.df, 2, sum) 
CLV[7]

CLV.df$CLV2 <- (CLV.df$p - CLV.df$c) * 0.8 / (1 + CLV.df$i)^(CLV.df$t - 1)

ggplot(CLV.df, aes(x = t, y = CLV2)) + geom_line() + ylab("CLV2") + xlab("Period") + labs(title = "CLV 2 Evolution")

CLV <- apply(CLV.df, 2, sum) 
CLV[7]
CLV[8]

head(mtcars) 

str(mtcars)

filter(mtcars, gear == 4) # 过滤器可与所有标准逻辑运算符一起使用
filter(mtcars, gear == 3 | gear == 4) # 即 >、<、=>、<=、!=、==
filter(mtcars, mpg > 21)

arrange(mtcars, gear)
arrange(mtcars, gear, mpg)
arrange(mtcars, desc(mpg)) # desc 以降序排序

select(mtcars, gear, mpg, hp)
select(mtcars, -drat) # - 符号表示删除变量/列

slice(mtcars, 1:3)

sample_n(mtcars, 5) # 选择的行数
sample_frac(mtcars, .1) # 选择的行数占总行数的比例

mutate(mtcars, wt_mpg = wt * mpg)
mutate(mtcars, mpg_mean = mean(mpg), mpg_diff_mean = mpg - mpg_mean)
mutate(mtcars, mpg_mean_diff = mpg - mean(mpg))

summarise(mtcars, sd(disp))
summarise(mtcars, median(mpg), mean(mpg), max(mpg), min(mpg))

group_by(mtcars, cyl)

mtcars %>% # take dataframe, then
  group_by(gear) %>% # group it by gear, then
  summarise(mean(mpg)) # summarise the mean mpg for each level of gear

mtcars %>%
  filter(mpg > 21) %>% select(gear, mpg, hp) %>% arrange(gear)

mtcars_mpg <- mtcars %>% filter(mpg > 21) %>% select(gear, mpg, hp) %>% arrange(gear)
mtcars_mpg

# Excercise
redwine <- read.csv("Data_redwine.csv", header=TRUE) 
head(redwine)

wines_fixed_acidity_8_or_greater <- redwine %>%
  filter(fixed.acidity >= 8)

wines_fixed_acidity_8_or_greater

# 使用filter函数筛选品质为7的葡萄酒
redwine_quality_7 <- redwine %>% 
  filter(quality == 7)

# 显示新数据框的前几行
head(redwine_quality_7)

# 使用select函数选择指定的两列，然后使用arrange函数按照pH的升序排序
redwine_selected_and_sorted <- redwine %>% 
  select(pH, quality) %>%
  arrange(pH)

# 显示新数据框的前几行
head(redwine_selected_and_sorted)

# 使用select函数选择指定的两列，然后使用arrange函数按照品质的降序排序
redwine_selected_and_sorted <- redwine %>% 
  select(pH, quality) %>%
  arrange(desc(quality))

# 显示新数据框的前几行
head(redwine_selected_and_sorted)

# 使用filter函数筛选没有柠檬酸的葡萄酒，然后使用arrange函数按照品质的降序排序
redwine_no_citric_acid <- redwine %>% 
  filter(citric.acid == 0) %>%
  arrange(desc(quality))

# 显示新数据框的前几行
head(redwine_no_citric_acid)

# 使用summary函数对红葡萄酒数据集进行总结
redwine_summary <- summary(redwine)

# 显示总结结果
redwine_summary
