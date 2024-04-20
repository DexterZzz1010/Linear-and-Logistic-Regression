### PROJECT 1 ###
## Part 1 ##
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(GGally)

# 导入数据
data <- read_excel("Data/kommuner.xlsx")
summary(data)
# a #####

## For y: PM10 & log(PM10) #####

ggplot(data, aes(x = Vehicles, y = log(PM10))) + geom_point() + 
  xlab("Vehicles (1000/capita)") +
  ylab("PM10 (g)") +
  labs(title = "PM10: by amount of vehicles") +
  theme(text = element_text(size = 18))

ggplot(data, aes(x=Vehicles, y = PM10)) + geom_point() +
  xlab("Vehicles (1000/capita)") +
  ylab("PM10 (g)") +
  labs(title = "log(PM10): by amount of vehicles") +
  theme(text = element_text(size = 18))

lin_model <- lm(log(PM10) ~ Vehicles, data=data)
log_model <- lm(PM10 ~ Vehicles, data=data)
yhatlin = predict(lin_model)
yhatlog = predict(log_model)

data <- mutate(data, elin = lin_model$residuals)
data <- mutate(data, elog = log_model$residuals)

# residual plot without log
ggplot(data, aes(x = yhatlin, y = elin)) +
  geom_point() +
  geom_hline(yintercept = 0)


# plot of residuals with log 
ggplot(data, aes(x = yhatlog, y = elog)) +
  geom_point() +
  geom_hline(yintercept = 0)

# Q-Q plot
ggplot(data = data, aes(sample = elin)) +
  geom_qq(size = 3) + geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals(Linear)") +
  theme(text = element_text(size = 18))

ggplot(data = data, aes(sample = elog)) +
  geom_qq(size = 3) + geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals(Log-transformation)") +
  theme(text = element_text(size = 18))

## For: x = Vehicles & ln(Vehicles) #####

data <- read_excel("Data/kommuner.xlsx")

ggplot(data, aes(x = Vehicles, y = log(PM10))) + geom_point() + 
  xlab("Vehicles (1000/capita)") +
  ylab("PM10 (g)") +
  labs(title = "PM10: by amount of vehicles") +
  theme(text = element_text(size = 18))

ggplot(data, aes(x = log(Vehicles), log(PM10))) + geom_point() +
  xlab("Vehicles (1000/capita)") +
  ylab("PM10 (g)") +
  labs(title = "log(PM10): by amount of vehicles") +
  theme(text = element_text(size = 18))

lin_model <- lm(log(PM10) ~ Vehicles, data=data)
log_model <- lm(log(PM10) ~ log(Vehicles), data=data)
yhatlin = predict(lin_model)
yhatlog = predict(log_model )

data <- mutate(data, elin = lin_model$residuals)
data <- mutate(data, elog = log_model$residuals)

# residual plot without log
ggplot(data, aes(x = yhatlin, y = elin)) +
  geom_point() +
  geom_hline(yintercept = 0)


# plot of residuals with log 
ggplot(data, aes(x = yhatlog, y = elog)) +
  geom_point() +
  geom_hline(yintercept = 0)

# Q-Q plot
ggplot(data = data, aes(sample = elin)) +
  geom_qq(size = 3) + geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals(Linear)") +
  theme(text = element_text(size = 18))

ggplot(data = data, aes(sample = elog)) +
  geom_qq(size = 3) + geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals(Log-transformation)") +
  theme(text = element_text(size = 18))



# b #####

## ln(PM10) against ln(Vehicles) ####
data <- read_excel("Data/kommuner.xlsx")
model_1b <- lm(log(PM10) ~ log(Vehicles), data=data)

Pb_seq <- data.frame(Vehicles = seq(100, 2500))

cbind(Pb_seq, 
      fit = predict(model_1b, newdata = Pb_seq),
      conf = predict(model_1b, newdata = Pb_seq, 
                     interval = "confidence"),
      pred = predict(model_1b, newdata = Pb_seq, 
                     interval = "prediction")) ->
  Pb_ints
glimpse(Pb_ints)

mutate(Pb_ints, conf.fit = NULL, pred.fit = NULL) -> Pb_ints
glimpse(Pb_ints)

# plot
ggplot(Pb_ints, aes(x = log(Vehicles))) + 
  geom_point(data = data, aes(y = log(PM10)), size = 3) +
  geom_line(aes(y = fit), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  geom_line(aes(y = pred.lwr), color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = pred.upr), color = "red", linetype = "dashed", linewidth = 1) +
  xlab("ln(Vehicles)") +
  ylab("ln(PM10)") +
  labs(title = "ln(PM10) against ln(Vehicles)",
       caption = "With fitted line, confidence and prediction intervals") +
  theme(text = element_text(size = 18))

## PM10 against Vehicles #####
data <- read_excel("Data/kommuner.xlsx")
model_1b_ <- lm(PM10 ~ Vehicles, data=data)

Pb_seq <- data.frame(Vehicles = seq(100, 2500))

cbind(Pb_seq, 
      fit = predict(model_1b_, newdata = Pb_seq),
      conf = predict(model_1b_, newdata = Pb_seq, 
                     interval = "confidence"),
      pred = predict(model_1b_, newdata = Pb_seq, 
                     interval = "prediction")) ->
  Pb_ints_
glimpse(Pb_ints_)

mutate(Pb_ints_, conf.fit = NULL, pred.fit = NULL) -> Pb_ints_
glimpse(Pb_ints_)

# plot
ggplot(Pb_ints_, aes(x = Vehicles)) + 
  geom_point(data = data, aes(y = PM10), size = 3) +
  geom_line(aes(y = fit), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  geom_line(aes(y = pred.lwr), color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = pred.upr), color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Year") +
  ylab("Pb") +
  labs(title = "PM10 against Vehicles",
       caption = "With fitted line, confidence and prediction intervals") +
  theme(text = element_text(size = 18))

# c #####
# Beta and s.e.
beta1 <- coef(model_1b)["log(Vehicles)"]
beta1_se <- summary(model_1b)$coefficients["log(Vehicles)", "Std. Error"]

# ln(0.9)
ln_0_9 <- log(0.9)

# Changed PM10
delta_ln_PM10 <- beta1 * ln_0_9

# Calculate percentage of the change
percent_change_PM10 <- (exp(delta_ln_PM10) - 1) * 100

# 95% vi
z_value <- qnorm(0.975)
ci_lower <- percent_change_PM10 - z_value * beta1_se * 10
ci_upper <- percent_change_PM10 + z_value * beta1_se * 10

# To determine the reduction in vehicles needed to halve PM10 emissions
required_reduction <- exp((log(0.5) / beta1) - 1)

cat("Expected change in PM10 for a 10% decrease in vehicles:", percent_change_PM10, "%\n")
cat("95% CI for this change rate: [", ci_lower, ",", ci_upper, "]\n")
cat("Reduction in vehicles needed to halve PM10 emissions:", required_reduction * 100, "%\n")

