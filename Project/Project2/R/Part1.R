### PROJECT 2 ###
## Part 2 ##
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(GGally)
library(tidyr)

# 导入数据
kommuner <- read_excel("Data/kommunerProject2.xlsx")
summary(kommuner)

# a #####
kommuner |> mutate(highcars = as.numeric(Cars > 600)) -> kommuner

kommuner <- mutate(kommuner,
                   highcars_cat = factor(highcars,
                                         levels = c(0, 1),
                                         labels = c("low", "high")))

ggplot(kommuner, aes(Income, highcars, color = highcars_cat)) +
  geom_point() +
  xlab("Income") +
  ylab("High car") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars") +
  theme(text = element_text(size = 14))


# Part of Sweden: 1 = Götaland; 2 = Svealand; 3 = Norrland
kommuner |> count(Part, highcars_cat)

kommuner %>%
  count(Part, highcars_cat) %>%
  spread(key = highcars_cat, value = n, fill = 0) %>%
  mutate(high_percentage = (high / (high + low))*100) %>%
  select(Part, high_percentage) %>%
  print()


# b #####
## model 1b #####
model_1b<- glm(highcars ~ Part, 
                      family = "binomial", 
                      data = kommuner)
summary(model_1b)
model_1b_sum <- summary(model_1b)



## β-estimates #####
# Interval for beta (intercept log odds and log odds ratio)
bhat <- model_1b$coefficients
ci.beta <- confint(model_1b)
cbind(beta = bhat, ci.beta) |> round(digits = 2)

# Interval for Odds and Odds Ratio, exp(beta)
# exp(beta0), exp(beta1)
or = exp(bhat)
ci.or <- exp(ci.beta)
cbind(`exp(beta)` = or, ci.or) |> round(digits = 2)

# Wald-based intervals by hand
se.bhat <- summary(model_1b)$coefficients[, "Std. Error"]
ci.wald <- cbind(lo = bhat - 1.96*se.bhat, hi = bhat + 1.96*se.bhat)
ci.wald |> round(digits = 2)

## Wald test #####
model_1b_sum$coefficients
b1 <- model_1b_sum$coefficients[2, "Estimate"]
se.b1 <- model_1b_sum$coefficients[2, "Std. Error"]
z.b1 <- model_1b_sum$coefficients[2, "z value"]

print(z.b1)

# Pr < 0.05
# z > 1.96

## LR test -  the null hypothesis #####
D_diff <- model_1b$null.deviance - model_1b$deviance
df_diff <- model_1b$df.null - model_1b$df.residual
chi2_alpha <- qchisq(p = 1 - 0.05, df = df_diff)
Pvalue <- pchisq(q = D_diff, df = df_diff, lower.tail = FALSE)

# test output
cbind(D_diff, df_diff, chi2_alpha, Pvalue)

# c #####
## plot Highcars(0/1) against Transit #####
ggplot(kommuner, aes(Transit, highcars)) +
geom_point() +
geom_smooth() +
xlab("Transit") +
ylab("High Cars") +
labs(title = "Highcars(0/1) against Transit") +
theme(text = element_text(size = 14))

## model 1c #####
model_1c<- glm(highcars ~ Transit, 
               family = "binomial", 
               data = kommuner)
summary(model_1c)
model_1c_sum <- summary(model_1c)

## β-estimates #####
# Interval for beta (intercept log odds and log odds ratio)
bhat <- model_1c$coefficients
ci.beta <- confint(model_1c)
cbind(beta = bhat, ci.beta) |> round(digits = 2)

# Interval for Odds and Odds Ratio, exp(beta)
# exp(beta0), exp(beta1)
or = exp(bhat)
ci.or <- exp(ci.beta)
cbind(`exp(beta)` = or, ci.or) |> round(digits = 2)

# Wald-based intervals by hand
se.bhat <- summary(model_1c)$coefficients[, "Std. Error"]
ci.wald <- cbind(lo = bhat - 1.96*se.bhat, hi = bhat + 1.96*se.bhat)
ci.wald |> round(digits = 2)


## Estimated probabilities #####
model_1c_pred <- cbind(
  kommuner,
  phat = predict(model_1c, type = "response"))

ggplot(model_1c_pred, aes(Transit, highcars)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", linewidth = 1) +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars",
       caption = "red = fitted line, blue dashed = moving average") +
  theme(text = element_text(size = 14))

## Plot with confidence interval #####
### Step 1: Conf.int. for the linear predictor, logodds ######
model_1c_pred <- cbind(
  model_1c_pred,
  logit = predict(model_1c, se.fit = TRUE))
glimpse(model_1c_pred)

lambda <- qnorm(1 - 0.05/2)
model_1c_pred |> mutate(
  logit.lwr = logit.fit - lambda*logit.se.fit,
  logit.upr = logit.fit + lambda*logit.se.fit) -> model_1c_pred
glimpse(model_1c_pred)

### Step 2: Confidence interval for the odds ######
model_1c_pred |> mutate(
  odds.lwr = exp(logit.lwr),
  odds.upr = exp(logit.upr)) -> model_1c_pred
glimpse(model_1c_pred)

### Step 3: Confidence interval for the probabilities #####
model_1c_pred |> mutate(
  p.lwr = odds.lwr/(1 + odds.lwr),
  p.upr = odds.upr/(1 + odds.upr)) -> model_1c_pred
glimpse(model_1c_pred)

## Plot estimated probability and  confidence interval #####
ggplot(model_1c_pred, aes(Transit, highcars)) +
  geom_point() +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars",
       caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 14))

## Wald test #####
model_1c_sum$coefficients
b1 <- model_1c_sum$coefficients[2, "Estimate"]
se.b1 <- model_1c_sum$coefficients[2, "Std. Error"]
z.b1 <- model_1c_sum$coefficients[2, "z value"]

print(z.b1)

# Pr < 0.05
## |z| > 1.96





# d ######
# Leverage:
model_1c_infl <- influence(model_1c)
glimpse(model_1c_infl)

model_1c_pred <- cbind(kommuner,
                   xbeta = predict(model_1c),
                   v = model_1c_infl$hat)
glimpse(model_1c_pred)

# Plot
pplus1_1c <- length(model_1c$coefficients)
n <- nobs(model_1c)

ggplot(model_1c_pred, aes(x = xbeta, y = v)) +
  geom_point() +
  geom_hline(yintercept = c(1/n)) +
  geom_hline(yintercept = c(2*pplus1_1c/n)) +
  facet_wrap(~ highcars)


# Find the highest leverages:
model_1c_pred |> slice_max(v, n = 8)

# highlight
ggplot(model_1c_pred, aes(x = xbeta, y = v)) +
  geom_point() +
  geom_point(data = filter(model_1c_pred, v > 0.045), 
             aes(color = "v > 0.045"), size = 3) +
  geom_hline(yintercept = c(1/n)) +
  geom_hline(yintercept = c(2*pplus1_1c/n)) +
  facet_wrap(~ highcars) +
  labs(color = "Highlight",
       title = "Leverage vs linear predictor by Y=0/Y=1") +
  theme(legend.position = "top",
        text = element_text(size = 14))


# e #####

## AIC and BIC #####
aic <- AIC(model_1b,model_1c)
bic <- BIC(model_1b,model_1c)
collect.AICetc <- data.frame(aic, bic)
collect.AICetc |> mutate(df.1 = NULL) -> collect.AICetc
collect.AICetc

## Pseudo R2 ####
model_0_glm <- glm(highcars ~ 1, family = "binomial", data = kommuner)

# log likelihood
lnL0 <- logLik(model_0_glm)[1]
collect.AICetc |> mutate(
  loglik =  c(logLik(model_1b)[1],
              logLik(model_1c)[1])) -> collect.AICetc
collect.AICetc

# McFadden
collect.AICetc |> mutate(
  R2McF = 1 - loglik/lnL0,
  R2McF.adj = 1 - (loglik - (df - 1)/2)/lnL0) -> collect.AICetc
collect.AICetc

