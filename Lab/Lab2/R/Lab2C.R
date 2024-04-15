#Lab2####
library(ggplot2)
library(dplyr)
load("Data/Pb_all.rda")
summary(Pb_all)

#### a ####

Pb_log_region<- lm(log(Pb) ~ I(year - 1975)+ region, data = Pb_all)
summary(Pb_log_region)
ggplot(Pb_all, aes(x = year, y = log(Pb)))+ facet_wrap(~ region) + geom_point()

Pb_log_region_sum <- summary(Pb_log_region)

Pb_log_region_sum$coefficients
Pb_log_region_sum$coefficients["I(year - 1975)",]

# Pr(>|t|越小越好，t越大越好

# P value
Pb_log_region_sum$coefficients["I(year - 1975)", "Pr(>|t|)"]

# t-value and t-quantile
tvalue_region <- Pb_log_region_sum$coefficients["I(year - 1975)", "Pr(>|t|)"]

qt(p = 0.05/2, 
   df = Pb_log_region$df.residual, 
   lower.tail = FALSE)

abs(tvalue_region)



#### b ####
#  A(b)  Pb_time
Pb_time<- lm(log(Pb) ~ I(year - 1975), data = Pb_all)
summary(Pb_time)
confint(Pb_time)


Pb_selected <- mutate(Pb_all, region = relevel(region, "Jamtland"))
Pb_log_selected<- lm(log(Pb) ~ I(year - 1975)+ region, data = Pb_selected)
summary(Pb_log_selected)
confint(Pb_log_selected)


model_comparison <- anova(Pb_time, Pb_log_selected)

# 打印比较结果
print(model_comparison)

# 提取并报告F检验的结果
F_statistic <- model_comparison[["F"]][2]  # 提取第二个模型的F统计量
df1 <- model_comparison[["Df"]][2]         # 第二个模型的自由度
df2 <- model_comparison[["Df"]][1] - model_comparison[["Df"]][2]  # 计算第二个模型与第一个模型自由度的差
p_value <- model_comparison[["Pr(>F)"]][2] # 提取第二个模型的P值

cat("F statistic:", F_statistic, "\n")
cat("Degrees of Freedom for Model 2:", df1, "; Difference in DF:", df2, "\n")
cat("P-value:", p_value, "\n")

#### c ######
# 对原始Pb数据绘制散点图，加上拟合线和置信区间
ggplot(Pb_all, aes(x = year, y = Pb)) +
  facet_wrap(~ region) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = region), color = "blue")

# 对log-transformed的Pb数据绘制散点图，加上拟合线和置信区间
ggplot(Pb_all, aes(x = year, y = log(Pb))) +
  facet_wrap(~ region) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = region), color = "blue")

####  d #########
e_region = Pb_log_region$residuals

ggplot(data = Pb_all, aes(sample = e_region)) +
  geom_qq(size = 3) + geom_qq_line() 
labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

ggplot(data = Pb_all, aes(x = e_region)) + geom_histogram(bins = 10)
####  e #########
ggplot(data = Pb_all, aes(sample = e_region)) +
  geom_qq(size = 3) + geom_qq_line() + facet_wrap(~ region)
labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))
