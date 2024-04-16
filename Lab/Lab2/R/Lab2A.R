#Lab2####
library(ggplot2)
library(dplyr)
load("Data/Pb_all.rda")
summary(Pb_all)

####  a  ####
# Plot Pb
ggplot(Pb_all, aes(x = year, y = Pb)) + geom_point()

####  b  ####
#  Fit a linear model
Pb_log_lm<- lm(log(Pb) ~ I(year - 1975), data = Pb_all)
summary(Pb_log_lm)
confint(Pb_log_lm)


####  c  ####
# test
Pb_log_sum <- summary(Pb_log_lm)

Pb_log_sum$coefficients
Pb_log_sum$coefficients["I(year - 1975)",]

# P value
Pb_log_sum$coefficients["I(year - 1975)", "Pr(>|t|)"]

# t-value and t-quantile
tvalue <- Pb_log_sum$coefficients["I(year - 1975)", "Pr(>|t|)"]

qt(p = 0.05/2, 
   df = Pb_log_lm$df.residual, 
   lower.tail = FALSE)

abs(tvalue)

####  d  ####
# predict
new_data <- data.frame(year = 2015)
predicted_with_confidence <- predict(Pb_log_lm, newdata = new_data, interval = "confidence")
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


predicted_with_prediction <- predict(Pb_log_lm, newdata = new_data, interval = "prediction")
print(predicted_with_prediction)

exp_prediction_lwr <- exp(predicted_with_prediction[,"lwr"])
exp_prediction_upr <- exp(predicted_with_prediction[,"upr"])

predicted_with_prediction_exp <- data.frame(
  lwr = exp_prediction_lwr,
  upr = exp_prediction_upr
)
print(predicted_with_prediction_exp)


####  e  ####

# Calculate the residuals and plot them against the predicted log values. 
# Also make a Q-Q-plot.
e = Pb_log_lm$residuals

ggplot(data = Pb_all, aes(sample = e)) +
  geom_qq(size = 3) + geom_qq_line() 
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

ggplot(data = Pb_all, aes(x = e)) + geom_histogram(bins = 10)


####  f  ####
# Redo the residual plots separately for each region
ggplot(data = Pb_all, aes(sample = e)) +
  geom_qq(size = 3) + geom_qq_line() + facet_wrap(~ region)
labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

