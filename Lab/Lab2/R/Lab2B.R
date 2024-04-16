#Lab2####
library(ggplot2)
library(dplyr)
library(caret)
load("Data/Pb_all.rda")
summary(Pb_all)

####  a  ####
# Plot Pb
ggplot(Pb_all, aes(x = year, y = Pb))+ facet_wrap(~ region) + geom_point()

####  b  ####
ggplot(Pb_all, aes(x = year, y = log(Pb)))+ facet_wrap(~ region) + geom_point()

####  c  ####
Pb_log_region<- lm(log(Pb) ~ I(year - 1975)+ region, data = Pb_all)
summary(Pb_log_region)
confint(Pb_log_region)


####  d  ####
Pb_all <- mutate(Pb_all, region = relevel(region, "Norrbotten"))
Pb_log_selected<- lm(log(Pb) ~ I(year - 1975)+ region, data = Pb_all)
summary(Pb_log_selected)
confint(Pb_log_selected)

new_data <- data.frame(year = 1975,region= "Orebro")
predicted_with_confidence <- predict(Pb_log_selected, newdata = new_data, interval = "confidence")
print(predicted_with_confidence)

exp_fit <- exp(predicted_with_confidence[,"fit"])
exp_lwr <- exp(predicted_with_confidence[,"lwr"])
exp_upr <- exp(predicted_with_confidence[,"upr"])

# 输出结果
predicted_with_confidence_exp <- data.frame(
  fit = exp_fit,
  lwr = exp_lwr,
  upr = exp_upr
)
print(predicted_with_confidence_exp)


predicted_with_prediction <- predict(Pb_log_selected, newdata = new_data, interval = "prediction")
print(predicted_with_prediction)

exp_prediction_lwr <- exp(predicted_with_prediction[,"lwr"])
exp_prediction_upr <- exp(predicted_with_prediction[,"upr"])

predicted_with_prediction_exp <- data.frame(
  lwr = exp_prediction_lwr,
  upr = exp_prediction_upr
)
print(predicted_with_prediction_exp)



# 获取时间系数的估计值
beta1_estimate <- exp(coef(Pb_log_selected)["I(year - 1975)"])

# 获取时间系数的95%置信区间
beta1_confint <- exp (confint(Pb_log_selected, "I(year - 1975)"))

# 提取置信区间的上限和下限
beta1_lwr <- beta1_confint[1]
beta1_upr <- beta1_confint[2]

# 打印结果
print(paste("Estimate: ", beta1_estimate))
print(paste("95% CI Lower Limit: ", beta1_lwr))
print(paste("95% CI Upper Limit: ", beta1_upr))


####  e  ####
# Pb_selected_Orebro <- mutate(Pb_all, region = relevel(region, "Orebro"))
# Pb_log_selected_Orebro<- lm(log(Pb) ~ I(year - 1975), data = Pb_selected_Orebro)
Pb_all <- mutate(Pb_all, region = relevel(region, "Orebro"))
Pb_log_selected_Orebro<- lm(log(Pb) ~ I(year - 1975)+ region, data = Pb_all)
summary(Pb_log_selected_Orebro)
confint(Pb_log_selected_Orebro)

new_data <- data.frame(year = 2015,region= "Orebro")
predicted_values_Orebro <- predict(Pb_log_selected_Orebro, newdata = new_data)
print(predicted_values_Orebro) # 3.642157 

predicted_Orebro_with_confidence <- predict(predicted_values_Orebro, newdata = new_data, interval = "confidence")
print(predicted_Orebro_with_confidence)

exp_fit <- exp(predicted_Orebro_with_confidence[,"fit"])
exp_lwr <- exp(predicted_Orebro_with_confidence[,"lwr"])
exp_upr <- exp(predicted_Orebro_with_confidence[,"upr"])

# 输出结果
predicted_Orebro_with_confidence_exp <- data.frame(
  fit = exp_fit,
  lwr = exp_lwr,
  upr = exp_upr
)
print(predicted_Orebro_with_confidence_exp)


predicted_Orebro_with_prediction <- predict(predicted_values_Orebro, newdata = new_data, interval = "prediction")
print(predicted_Orebro_with_prediction)

exp_prediction_lwr <- exp(predicted_Orebro_with_prediction[,"lwr"])
exp_prediction_upr <- exp(predicted_Orebro_with_prediction[,"upr"])

predicted_Orebro_with_prediction_exp <- data.frame(
  lwr = exp_prediction_lwr,
  upr = exp_prediction_upr
)
print(predicted_Orebro_with_prediction_exp)


#### f #########

# 获取时间系数的估计值
beta1_estimate <- exp(coef(Pb_log_selected)["regionOrebro"])

# 获取时间系数的95%置信区间
beta1_confint <- exp (confint(Pb_log_selected, "regionOrebro"))

# 提取置信区间的上限和下限
beta1_lwr <- beta1_confint[1]
beta1_upr <- beta1_confint[2]

# 打印结果
print(paste("Estimate: ", beta1_estimate))
print(paste("95% CI Lower Limit: ", beta1_lwr))
print(paste("95% CI Upper Limit: ", beta1_upr))


#### g ####
Pb_all <- mutate(Pb_all, region = relevel(region, "Norrbotten"))
Pb_log_selected<- lm(log(Pb) ~ I(year - 1975)+ region, data = Pb_all)
summary(Pb_log_selected)
confint(Pb_log_selected)

new_data <- data.frame(year = 1975,region= "Orebro")
predicted_with_confidence <- predict(Pb_log_selected, newdata = new_data, interval = "confidence")
print(predicted_with_confidence)

exp_fit <- exp(predicted_with_confidence[,"fit"])
exp_lwr <- exp(predicted_with_confidence[,"lwr"])
exp_upr <- exp(predicted_with_confidence[,"upr"])

# 输出结果
predicted_with_confidence_exp <- data.frame(
  fit = exp_fit,
  lwr = exp_lwr,
  upr = exp_upr
)
print(predicted_with_confidence_exp)


predicted_with_prediction <- predict(Pb_log_selected, newdata = new_data, interval = "prediction")
print(predicted_with_prediction)

exp_prediction_lwr <- exp(predicted_with_prediction[,"lwr"])
exp_prediction_upr <- exp(predicted_with_prediction[,"upr"])

predicted_with_prediction_exp <- data.frame(
  lwr = exp_prediction_lwr,
  upr = exp_prediction_upr
)
print(predicted_with_prediction_exp)

#### h ####
Pb_all <- mutate(Pb_all, region = relevel(region, "Norrbotten"))
Pb_log_selected<- lm(log(Pb) ~ I(year - 1975)+ region, data = Pb_all)
summary(Pb_log_selected)
confint(Pb_log_selected)

new_data <- data.frame(year = 2015,region= "Orebro")
predicted_with_confidence <- predict(Pb_log_selected, newdata = new_data, interval = "confidence")
print(predicted_with_confidence)

exp_fit <- exp(predicted_with_confidence[,"fit"])
exp_lwr <- exp(predicted_with_confidence[,"lwr"])
exp_upr <- exp(predicted_with_confidence[,"upr"])

# 输出结果
predicted_with_confidence_exp <- data.frame(
  fit = exp_fit,
  lwr = exp_lwr,
  upr = exp_upr
)
print(predicted_with_confidence_exp)


predicted_with_prediction <- predict(Pb_log_selected, newdata = new_data, interval = "prediction")
print(predicted_with_prediction)

exp_prediction_lwr <- exp(predicted_with_prediction[,"lwr"])
exp_prediction_upr <- exp(predicted_with_prediction[,"upr"])

predicted_with_prediction_exp <- data.frame(
  lwr = exp_prediction_lwr,
  upr = exp_prediction_upr
)
print(predicted_with_prediction_exp)

