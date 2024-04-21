### PROJECT 1 ###
## Part 2 ##
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(GGally)
library(patchwork)
# 导入数据
data <- read_excel("Data/kommuner.xlsx")
summary(data)
# a #####
data <- read_excel("Data/kommuner.xlsx")
model_1b <- lm(log(PM10) ~ log(Vehicles), data=data) 

summary(model_1b)
confint(model_1b)

# t-value is 21.68 for vehicles
# 1 and 288 degrees of freedom
# P-value is very small ~ 0, smaller than alpha = 0.05
# we reject H0, vehicles have an impact on PM10

# b #####

coast <- factor(data$Coastal, labels = c("No", "Yes"))
data <- mutate(data, Coastal = coast) # 替代原来的Coastal
data$Part <- factor(data$Part, labels = c("Gotaland", "Svealand", "Norrland"))

model_2b = lm(log(PM10) ~ log(Vehicles) + Coastal*Part, data=data)
model_2b$coefficients

count(data, Part == "Gotaland", Coastal == "Yes")
count(data, Part == "Svealand", Coastal == "Yes")
count(data, Part == "Norrland", Coastal == "Yes")

confint(model_2b) # 计算置信区间

# to test if any of the added parameters are different from 0
# partial F-test is used, ANOVA
# HO all of the added coefficients are zero
# p-value is 0.0001194 < 0.05, reject HO, not all are 0

anova(model_1b,model_2b) # 比较模型, 测试Coastal*Part是否显著




#model without interaction
model_2b_ <- lm(log(PM10) ~ log(Vehicles) + Coastal+Part, data=data)
#anova with and without interaction to see if the interaction
#is significant 
# 测试交互作用是否显著
anova(model_2b_,model_2b)
# P-value is 0.003839 < 0.05, reject H0, interaction is
# significant according to test



# define testing data
# 定义了一个测试数据集，包含不同地区和沿海状态下的虚拟车辆数，
# 然后对这些数据进行预测，并打印出对数尺度及原尺度上的置信区间。
test_data <- data.frame(
  Vehicles = 1000,
  Coastal = c("Yes", "Yes", "Yes", "No", "No", "No"),
  Part = c("Gotaland", "Svealand", "Norrland", "Gotaland", "Svealand", "Norrland")
)

# Iterate over each test data
for (i in 1:nrow(test_data)) {
  prediction <- predict(model_2b, newdata = test_data[i,], interval = "confidence")
  cat("Confidence interval for", test_data[i, "Coastal"], test_data[i, "Part"], ":", prediction, "\n")
  cat("Exponential confidence interval for", test_data[i, "Coastal"], test_data[i, "Part"], ":", exp(prediction), "\n \n")
}


# c #####
kommuner <- read_excel("Data/kommuner.xlsx")
kommuner <- mutate(kommuner, Coastal = coast) # 替代原来的Coastal
kommuner$Part <- factor(kommuner$Part, labels = c("Gotaland", "Svealand", "Norrland"))
kommuner <- mutate(kommuner, Coastal = relevel(Coastal,"Yes"))

confint(model_2b)
summary(model_2b)

## göra anova med den nya reducerade modellen, stort F värden 
# så är den större modellen sämre/onödig

kommuner <-
  mutate(kommuner, NewParts =
           as.numeric(Part == "Gotaland" & Coastal == "No") +
           2*as.numeric(Part == "Svealand" & Coastal == "No") +
           3*as.numeric(Part == "Norrland" | Coastal == "Yes"))
kommuner$NewParts <- factor(kommuner$NewParts, labels = c("GotalandNo", "SvealandNo", "NorrlandYes"))
model_2c <- lm(log(PM10)~ log(Vehicles) + NewParts, data=kommuner)
summary(model_2c)
anova(model_2c,model_2b)

# 结果：F: 7.3241 Pr(>F): 9.575e-05

# F value：F 统计量，用于比较模型间的变异比例。这个值是检验两个模型拟合数据能力是否存在显著差异的统计量。
# Pr(>F)：F 统计量对应的 p 值，用于决定拒绝还是不拒绝零假设。
# F 值较高且对应的 p 值较低通常表明 model_2b 在统计上显著优于 model_2c。

confint(model_2c)

# d ####

# 创建各个图形
p1 <- ggplot(kommuner, aes(x = Vehicles, y = log(PM10))) + geom_point()
p2 <- ggplot(kommuner, aes(x = log(Vehicles), y = log(PM10))) + geom_point()
p3 <- ggplot(kommuner, aes(x = log(Builton), y = log(PM10))) + geom_point()
p4 <- ggplot(kommuner, aes(x = log(Higheds), y = log(PM10))) + geom_point()
p5 <- ggplot(kommuner, aes(x = log(GRP), y = log(PM10))) + geom_point()
p6 <- ggplot(kommuner, aes(x = Income, y = log(PM10))) + geom_point()
p7 <- ggplot(kommuner, aes(x = Part, y = log(PM10))) + geom_point()
p8 <- ggplot(kommuner, aes(x = County, y = log(PM10))) + geom_point()
# 使用 patchwork 组合图形
plot_grid <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + 
  plot_layout(nrow = 2, ncol = 4)  # 定义布局为 2 行 4 列

# 打印组合后的图形
print(plot_grid)

model_2d <- lm(log(PM10)~ log(Vehicles) + log(Builton) + log(Higheds), data=kommuner)

summary(model_2d)
confint(model_2d)
vif(model_2d)

# VIF = 1：没有多重共线性。
# 1 < VIF < 5：通常认为这个范围内的多重共线性是可接受的。
# VIF ≥ 5：表明预测变量之间存在较高程度的多重共线性，可能需要进一步的分析和处理。
# VIF ≥ 10：通常被认为存在严重的多重共线性问题，可能会严重影响回归系数的估计和模型的稳定性。

# log(Vehicles)  log(Builton)  log(Higheds) 
#    8.546203     10.300020      2.005061 

# e #####
model_2e_ <- lm(log(PM10)~log(Vehicles)+log(Higheds)+Children+Seniors+log(Income)+log(GRP)+NewParts, data=kommuner)
vif(model_2e_)
ggpairs(kommuner,columns=c(8)) #remove seniors
model_2e <- lm(log(PM10)~log(Vehicles)+log(Higheds)+Children+log(Income)+log(GRP)+NewParts, data=kommuner)
vif(model_2e)

