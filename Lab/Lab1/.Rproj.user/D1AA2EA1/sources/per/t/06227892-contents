library(ggplot2)
library(dplyr)
load("Data/Pb_all.rda")

# a Exponential form

# -- b --
ggplot(Pb_myregion, aes(x = year - 1975, y = log(Pb))) + geom_point()

# -- c --
Pb_log_lm<- lm(log(Pb) ~ I(year - 1975), data = Pb_all)
summary(Pb_log_lm)
confint(Pb_log_lm)

# -- d --
Pb_log_region_lm<- lm(log(Pb) ~ I(year - 1975), data = Pb_myregion)
new_data <- data.frame(year = 2015)  # 这里是45，表示2020年与1975年的差
predicted_values <- predict(Pb_log_region_lm, newdata = new_data)
print(predicted_values)

predicted_with_confidence <- predict(Pb_log_region_lm, newdata = new_data, interval = "confidence")
print(predicted_with_confidence)

predicted_with_prediction <- predict(Pb_log_region_lm, newdata = new_data, interval = "prediction")
print(predicted_with_prediction)

# e decrease

cbind(Pb_log_region_lm$coefficients["I(year - 1975)"],
      confint(Pb_log_region_lm, parm = "I(year - 1975)")) |>
  round(digits = 4)

# -- i --
Pb_seq <- data.frame(year = seq(1975, 2010))

cbind(Pb_seq, 
      fit = predict(Pb_log_lm, newdata = Pb_seq),
      conf = predict(Pb_log_lm, newdata = Pb_seq, 
                     interval = "confidence"),
      pred = predict(Pb_log_lm, newdata = Pb_seq, 
                     interval = "prediction")) ->
  Pb_ints
glimpse(Pb_ints)

mutate(Pb_ints, conf.fit = NULL, pred.fit = NULL) -> Pb_ints
glimpse(Pb_ints)

ggplot(Pb_ints, aes(x = year)) + 
  geom_point(data = Pb_all, aes(y = log(Pb)), size = 3) +
  geom_line(aes(y = fit), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  geom_line(aes(y = pred.lwr), color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = pred.upr), color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Year") +
  ylab("Pb") +
  labs(title = "Pb concentration as a function of years since 1975",
       caption = "With Log-transformation fitted line, confidence and prediction intervals") +
  theme(text = element_text(size = 18))


# -- j --
e_log = Pb_log_lm$residuals

ggplot(data = Pb_all, aes(sample = e_log)) +
  geom_qq(size = 3) + geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals(Log-transformation)") +
  theme(text = element_text(size = 18))

ggplot(data = Pb_all, aes(x = e_log)) + geom_histogram(bins = 10)


# Lab1.c

# a: \text{weight} = e^{\beta_0} \cdot \text{year}^{\beta_1} \cdot e^{\epsilon}

# -- b --
cbind(expbeta = exp(Pb_log_lm$coefficients["(Intercept)"]),
      exp(confint(Pb_log_lm, parm = "(Intercept)"))) |>
  round(digits = 4)

cbind(expbeta = exp(Pb_log_lm$coefficients["I(year - 1975)"]),
      exp(confint(Pb_log_lm, parm = "I(year - 1975)"))) |>
  round(digits = 4)


# d (e*β_1 − 1)

#### e & f  ####
Pb_myregion <- filter(Pb_all, region == "Norrbotten")
Pb_log_region_lm<- lm(log(Pb) ~ I(year - 1975), data = Pb_myregion)

new_data <- data.frame(year = 1975) 
predicted_exp_values <- exp( predict(Pb_log_region_lm, newdata = new_data))
print(predicted_exp_values)

predicted_exp_with_confidence <- exp(predict(Pb_log_region_lm, newdata = new_data, interval = "confidence"))
print(predicted_exp_with_confidence)

predicted_exp_with_prediction <- exp(predict(Pb_log_region_lm, newdata = new_data, interval = "prediction"))
print(predicted_exp_with_prediction)
 
####  g  ####
ggplot(Pb_ints, aes(x = year - 1975)) + 
  geom_point(data = Pb_all, aes(y = Pb)) + 
  geom_line(aes(y = exp(fit)), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = exp(conf.lwr), ymax = exp(conf.upr)), alpha = 0.2) +
  geom_line(aes(y = exp(pred.lwr)),
            color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = exp(pred.upr)),
            color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Year") +
  ylab("Pb") +
  labs(title = "Pb concentration as a function of years since 1975",
       caption = "fitted log-log model, 95% conf. and pred. intervals") +
  theme(text = element_text(size = 18))
 
####  h  ####
ggplot(Pb_ints, aes(x = year)) + 
  geom_point(data = Pb_all, aes(y = Pb)) + 
  geom_line(aes(y = exp(fit)), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = exp(conf.lwr), ymax = exp(conf.upr)), alpha = 0.2) +
  geom_line(aes(y = exp(pred.lwr)),
            color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = exp(pred.upr)),
            color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Year") +
  ylab("Pb") +
  labs(title = "Pb concentration as a function of years since 1975",
       caption = "fitted log-log model, 95% conf. and pred. intervals") +
  theme(text = element_text(size = 18))
