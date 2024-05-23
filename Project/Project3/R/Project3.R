### PROJECT 3 ###
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(GGally)
library(gridExtra)
library(knitr)
library(tidyr)
library(tidyverse)
library(caret)
library(pROC)
# library(MASS)
library(ResourceSelection)
library(ggrepel)

# 导入数据
kommuner <- read_excel("Data/kommunerProject3.xlsx")
summary(kommuner)


# new parts #####
# 分割newparts
coast <- factor(kommuner$Coastal, labels = c("No", "Yes"))
kommuner <- mutate(kommuner, Coastal = coast)
kommuner$Part <- factor(kommuner$Part, labels = c("Gotaland", "Svealand", "Norrland"))

kommuner <-
  mutate(kommuner, NewParts =
           as.numeric(Part == "Gotaland"| Coastal == "Yes") +
           2*as.numeric(Part == "Svealand"  & Coastal == "No") +
           3*as.numeric(Part == "Norrland" & Coastal == "No"))
kommuner$NewParts <- factor(kommuner$NewParts, labels = c("GotalandorYes", "SvealandandNo","NorrlandandNo"))



# For the missing data ####
# 将缺失数据填充
kommuner |> mutate(Fertility = as.numeric(Fertility)) -> kommuner

kommuner |> filter(Part == 3 & Coastal == 0) |>
  summarise(meanfertility = mean(Fertility, na.rm = TRUE))
I <- which(is.na(kommuner$Fertility))
kommuner$Fertility[I] <- 1.57 # meanfertility = 1.57

summary(kommuner)



## plot ######
# 创建并保存 ggplot 图
variables <- c("log(Higheds)", "Children", "Seniors", "log(Income)", "log(GRP)", "Persperhh", 
               "Fertility", "Urban", "Transit", "log(Apartments)", "Builton", "log(Population)", "NewParts")
for (var in variables) {
  p <- ggplot(kommuner, aes_string(x = var, y = "Cars_nbr")) + 
    geom_point() + 
    xlab(var) +
    ylab("log(Cars_nbr)") +
    labs(title = paste("Cars_nbr: by", var)) +
    theme(text = element_text(size = 18))
  
  # 保存图形
  ggsave(filename = paste0("plot_", gsub("[()]", "", var), ".png"), plot = p, width = 8, height = 6)
}





ggplot(kommuner, aes(x = log(Population), y = log(Cars_nbr))) + geom_point() + 
  xlab("log(Population)") +
  ylab("log(Cars_nbr)") +
  labs(title = "log(Cars_nbr): by amount of log(Population)") +
  theme(text = element_text(size = 18))





kommuner_glm <- glm(log(Cars_nbr) ~ log(Population), family = "poisson", data = kommuner)
summary(kommuner_glm)

beta <- kommuner_glm$coefficients
RR <- exp(beta)
ci_beta = confint(kommuner_glm)
ci_RR = exp(ci_beta)

cbind(beta = beta, ci_beta, RR = RR, ci_RR) |> round(digits = 2)

kommuner_pred <- cbind(
  kommuner,
  xb = predict(kommuner_glm, se.fit = TRUE))

kommuner_pred |> mutate(
  xb.residual.scale = NULL,
  xb.lwr = xb.fit - 1.96*xb.se.fit,
  xb.upr = xb.fit + 1.96*xb.se.fit,
  muhat = exp(xb.fit),
  mu.lwr = exp(xb.lwr),
  mu.upr = exp(xb.upr)) -> kommuner_pred

kommuner_pred |> mutate(
  xb.residual.scale = NULL,
  xb.lwr = xb.fit - 1.96*xb.se.fit,
  xb.upr = xb.fit + 1.96*xb.se.fit,
  muhat = exp(xb.fit),
  mu.lwr = exp(xb.lwr),
  mu.upr = exp(xb.upr)) -> kommuner_pred

glimpse(kommuner_pred)

ggplot(kommuner_pred, aes(log(Population), log(Cars_nbr))) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  labs(title = "Expected number of cars",
       caption = "95% confidence interval",
       color = "program") +
  theme(text = element_text(size = 18))


ggpairs(kommuner,columns=c(6,7,8,9,10,11,14,15,16,17,18,20))



model_full <- glm(Cars_nbr ~ log(Higheds) + Children + Seniors + log(Income) + 
                    log(GRP) + Persperhh + Fertility + Urban + Transit + log(Apartments)
                  +Builton +  log(Population) + NewParts, 
                  family = "poisson", 
                  data = kommuner)

summary(model_full)
model_full_sum <- summary(model_full)
vif(model_full)


model_null <- glm(Cars_nbr ~ 1, family = "poisson", data = kommuner)

### AIC stepwise selection ####
step_model_aic <- step(model_null,
                       scope = list(lower = model_null, upper = model_full),
                       direction = "both",
                       trace = TRUE,  # trace=TRUE detail information for every steps
                       k = 2)  # k=2 means using AIC

summary(step_model_aic)
aic_sum <- summary(step_model_aic)
vif(step_model_aic)



### BIC stepwise selection ####
step_model_bic <- step(model_null,
                       scope = list(lower = model_null, upper = model_full),
                       direction = "both",
                       trace = TRUE,
                       k =  log(nobs(model_full)))  # BIC
summary(step_model_bic)
bic_sum <- summary(step_model_bic)

#### AIC and BIC #####
aic <- AIC(step_model_aic,step_model_bic)
bic <- BIC(step_model_aic,step_model_bic)
collect.AICetc <- data.frame(aic, bic)
collect.AICetc |> mutate(df.1 = NULL) -> collect.AICetc
collect.AICetc
# 感觉AIC BIC 没用，根据vif和cor手动筛选


## parts ####
# 探究不同的part之间，分布有没有不同
model_red_part <- update(model_full, . ~ . - NewParts)
summary(model_red_part)

D_diff_red <- model_red_part$deviance - model_full$deviance
D_diff_red

df_diff_red <- model_red_part$df.residual - model_full$df.residual
df_diff_red

anova(model_red_part, model_full)
qchisq(1 - 0.05, df_diff_red)
pchisq(D_diff_red, df_diff_red, lower.tail = FALSE)

# 显著性检验：由于实际的 p 值（1.822603e-109）远小于 0.05，说明 NewParts 变量对模型的改进是显著的。
# 偏差变化：500.76 的偏差变化量也远大于临界值 5.991465，进一步支持了 NewParts 对响应变量 Cars_nbr 有显著影响。

## possion reduce model ######
# 删减后的模型
model_poi_red <- glm(Cars_nbr ~ log(Apartments) + log(Builton) + log(Higheds) + 
                       log(Seniors) + Transit + Urban + log(Vehicles) + NewParts+ 
                       offset( log(Population) ),
                  family = "poisson", 
                  data = kommuner)
summary(model_poi_red)
model_poi_red_sum <- summary(model_poi_red)
vif(model_poi_red)


model_red_pop <- update(model_poi_red, . ~ . - log(Population))
summary(model_red_part)

D_diff_red <- model_red_pop$deviance - model_poi_red$deviance
D_diff_red

df_diff_red <- model_red_pop$df.residual - model_poi_red$df.residual
df_diff_red

anova(model_red_pop, model_poi_red)
qchisq(1 - 0.05, df_diff_red)
pchisq(D_diff_red, df_diff_red, lower.tail = FALSE)



beta <- model_poi_red$coefficients
RR <- exp(beta)
ci_beta = confint(model_poi_red)
ci_RR = exp(ci_beta)

cbind(beta = beta, ci_beta, RR = RR, ci_RR) |> round(digits = 2)


poi_pred <- cbind(
  kommuner,
  xb = predict(model_poi_red, se.fit = TRUE))
poi_pred |> mutate(
  xb.residual.scale = NULL,
  xb.lwr = xb.fit - 1.96*xb.se.fit,
  xb.upr = xb.fit + 1.96*xb.se.fit,
  muhat = exp(xb.fit),
  mu.lwr = exp(xb.lwr),
  mu.upr = exp(xb.upr)) -> poi_pred

glimpse(poi_pred)

ggplot(poi_pred, aes(Fertility, Cars_nbr, color = NewParts)) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  labs(title = "Expected number of awards",
       caption = "95% confidence interval",
       color = "program") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ NewParts)


## leverage #####
poi_pred |> mutate(v = hatvalues(model_poi_red)) -> poi_pred

pplus1 <- length(model_poi_red$coefficients)
n <- nobs(model_poi_red)

top_leverage <- poi_pred %>%
  arrange(desc(v)) %>%
  slice(1:6)


ggplot(poi_pred, aes(log(muhat), v, color = NewParts)) +
  geom_point(size = 2) +
  geom_point(data = top_leverage, aes(x = log(muhat), y = v), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = log(muhat), y = v, label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = 1/n, linetype = "dashed",color = "red") +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Poisson Regression Leverage",
       color = "NewParts", caption = "horizontal line = 2(p+1)/n") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ NewParts)

## Standardised deviance residuals #####
poi_red_infl <- influence(model_poi_red)
glimpse(poi_red_infl)

poi_pred |> mutate(devres = poi_red_infl$dev.res,
                     std.devres = devres/sqrt(1 - v)) ->
  poi_pred
glimpse(poi_pred)

top_dev <- poi_pred %>%
  arrange(desc(abs(std.devres))) %>%
  slice(1:6)

ggplot(poi_pred, aes(log(muhat), std.devres, color = NewParts)) +
  geom_point(size = 2) +
  geom_point(data = top_dev, aes(x = log(muhat), y = std.devres), color = "red", size = 3) +  
  geom_text(data = top_dev, aes(x = log(muhat), y = std.devres, label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", linewidth = 1) +
  labs(title = "Poisson Regression Standardized deviance residuals",
       color = "NewParts") +
  theme(text = element_text(size = 18)) 


## Cook’s D #####
poi_pred |> mutate(D = cooks.distance(model_poi_red)) -> poi_pred

top_cook <- poi_pred %>%
  arrange(desc(abs(D))) %>%
  slice(1:6)

ggplot(poi_pred, aes(log(muhat), D, color = NewParts)) +
  geom_point(size = 2) +
  geom_point(data = top_cook, aes(x = log(muhat), y = D), color = "red", size = 3) +  
  geom_text(data = top_cook, aes(x = log(muhat), y = D, label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = c(4/n, 1), color = "red") +
  labs(title = "Poisson Regression Cook's distance",
       color = "NewParts", caption = "horizontal lines = 4/n and 1") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ NewParts)


# negbin #####
model_nb <- glm.nb(Cars_nbr ~ log(Apartments) + log(Builton) + log(Higheds)  +
                         log(Seniors) + Transit + Urban + log(Vehicles) + NewParts+ 
                       offset(log(Population)),
                       data = kommuner)
summary(model_nb)
vif(model_nb)


model_red_part <- update(model_nb, . ~ . - NewParts)
summary(model_red_part)

D_diff_red <- model_red_part$deviance - model_nb$deviance
D_diff_red

df_diff_red <- model_red_part$df.residual - model_nb$df.residual
df_diff_red

anova(model_red_part, model_nb)
qchisq(1 - 0.05, df_diff_red)
pchisq(D_diff_red, df_diff_red, lower.tail = FALSE)



model_poi <- glm(Cars_nbr ~ log(Apartments) + log(Builton) + log(Higheds) + 
                       log(Seniors) + Transit + Urban + log(Vehicles) + NewParts+ 
                       offset( log(Population) ),
                     family = "poisson", 
                     data = kommuner)

beta <- model_nb$coefficients
ci_beta = confint(model_nb)
cbind(beta = beta, ci_beta) |> round(digits = 2)

cbind(RR = exp(beta), exp(ci_beta)) |> round(digits = 2)

nb_pred <- cbind(
  kommuner,
  xb = predict(model_nb, se.fit = TRUE))
nb_pred |> 
  mutate(
    xb.residual.scale = NULL,
    xb.lwr = xb.fit - 1.96*xb.se.fit,
    xb.upr = xb.fit + 1.96*xb.se.fit,
    muhat = exp(xb.fit),
    mu.lwr = exp(xb.lwr),
    mu.upr = exp(xb.upr)) ->
  nb_pred

ggplot(nb_pred, aes(Fertility, Cars_nbr, color = NewParts)) +
  geom_point() +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  facet_wrap(~ NewParts)


## standardized deviance residuals ####\
# compare with poisson
nb_infl <- influence(model_nb)
nb_pred |> mutate(
  v = hatvalues(model_nb),
  devres = nb_infl$dev.res,
  std.devres = devres/sqrt(1 - v)) ->
  nb_pred

# poi_red_infl <- influence(model_poi_red)
# glimpse(poi_red_infl)

nb_pred |> mutate(
  xb_pois = predict(model_poi_red),
  v_pois = hatvalues(model_poi_red),
  devres_pois = poi_red_infl$dev.res,
  std.devres_pois = devres_pois/sqrt(1 - v_pois)) -> nb_pred

# 对比泊松和伯努利的结果范围
nb_pred |> summarise(
  min_nb = min(std.devres),
  max_nb = max(std.devres),
  min_pois = min(std.devres_pois),
  max_pois = max(std.devres_pois))


high_residuals_excl <- filter(nb_pred, abs(std.devres) > 3)

# plot 伯努利分布的residuals
ggplot(nb_pred, aes(x = xb.fit, color = NewParts)) +
  geom_point(aes(y = std.devres), size = 2) +
  geom_point(data = high_residuals_excl, aes(x = xb.fit, y = std.devres), color = "red", size = 3) +  
  geom_text(data = high_residuals_excl, aes(x = xb.fit, y = std.devres, label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = 2, linewidth = 1) +
  expand_limits(y = c(-4.5, 7.5)) +
  labs(y = "std dev.res", x = "xb", color = "NewParts",
       title = "Negative binomial regression Standardized deviance residuals") +
  theme(text = element_text(size = 18))+
  facet_wrap(~ NewParts)


## leverage #####
nb_pred |> mutate(v = hatvalues(model_nb)) -> nb_pred

pplus1 <- length(model_nb$coefficients)
n <- nobs(model_nb)

top_leverage <- nb_pred %>%
  arrange(desc(v)) %>%
  slice(1:6)


ggplot(nb_pred, aes( xb.fit , v, color = NewParts)) +
  geom_point(size = 2) +
  geom_point(data = top_leverage, aes(x =  xb.fit , y = v), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = xb.fit , y = v, label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = 1/n, linetype = "dashed",color = "red") +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = " Negative binomial regression Leverage",
       color = "program", caption = "horizontal line = 2(p+1)/n") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ NewParts)

## Cook’s D #####
nb_pred |> mutate(D = cooks.distance(model_nb)) -> nb_pred

top_cook <- nb_pred %>%
  arrange(desc(abs(D))) %>%
  slice(1:6)

ggplot(nb_pred, aes(log(muhat), D, color = NewParts)) +
  geom_point(size = 2) +
  geom_point(data = top_cook, aes(x = log(muhat), y = D), color = "red", size = 3) +  
  geom_text(data = top_cook, aes(x = log(muhat), y = D, label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = c(4/n), color = "red") +
  labs(title = "Negative binomial regression Cook's distance",
       color = "program", caption = "horizontal lines = 4/n and 1") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ NewParts)

# model_nb <- glm.nb(Cars_nbr ~  log(Apartments) + log(Builton) + log(Higheds) + 
                #log(Seniors) + Transit + Urban + log(Vehicles) + NewParts, 
                #offset = log(Population),
                #data = kommuner)

model_full <- glm.nb(Cars_nbr ~ llog(Apartments) + log(Builton) + log(Higheds) + log(Population) +
                       log(Seniors) + Transit + Urban + log(Vehicles) + NewParts+offset( log(Population) ),
                  data = kommuner)

summary(model_full)
model_full_sum <- summary(model_full)
vif(model_full)


model_null <- glm(Cars_nbr ~ 1, family = "poisson", data = kommuner)

### AIC stepwise selection ####
step_model_aic <- step(model_null,
                       scope = list(lower = model_null, upper = model_full),
                       direction = "both",
                       trace = TRUE,  # trace=TRUE detail information for every steps
                       k = 2)  # k=2 means using AIC

summary(step_model_aic)
aic_sum <- summary(step_model_aic)
vif(step_model_aic)



### BIC stepwise selection ####
step_model_bic <- step(model_null,
                       scope = list(lower = model_null, upper = model_full),
                       direction = "both",
                       trace = TRUE,
                       k =  log(nobs(model_full)))  # BIC
summary(step_model_bic)
bic_sum <- summary(step_model_bic)

#### AIC and BIC #####
aic <- AIC(step_model_aic,step_model_bic)
bic <- BIC(step_model_aic,step_model_bic)
collect.AICetc <- data.frame(aic, bic)
collect.AICetc |> mutate(df.1 = NULL) -> collect.AICetc
collect.AICetc


## nb_red #####
remove <- c("0183 Sundbyberg","1275 Perstorp","0186 Lidingö","1480 Göteborg")

kommuner_ <- kommuner %>%
  filter(!Kommun %in% remove)
nb_red <- update(model_nb, data = kommuner_)

nb_pred_ <- cbind(
  kommuner_,
  xb = predict(nb_red, se.fit = TRUE))

nb_pred_ |> 
  mutate(
    xb.residual.scale = NULL,
    xb.lwr = xb.fit - 1.96*xb.se.fit,
    xb.upr = xb.fit + 1.96*xb.se.fit,
    muhat = exp(xb.fit),
    mu.lwr = exp(xb.lwr),
    mu.upr = exp(xb.upr)) ->
  nb_pred_

nb_infl <- influence(nb_red)
nb_pred_ |> mutate(
  v = hatvalues(nb_red),
  devres = nb_infl$dev.res,
  std.devres = devres/sqrt(1 - v)) ->
  nb_pred_

# poi_red_infl <- influence(model_poi_red)
# glimpse(poi_red_infl)



# Get municipality with high residuals
high_residuals_excl <- filter(nb_pred_, abs(std.devres) > 3)

# plot 伯努利分布的residuals
ggplot(nb_pred_, aes(x = xb.fit, color = NewParts)) +
  geom_point(aes(y = std.devres), size = 2) +
  geom_point(data = high_residuals_excl, aes(x = xb.fit, y = std.devres), color = "red", size = 3) +  
  geom_text(data = high_residuals_excl, aes(x = xb.fit, y = std.devres, label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = 2, linewidth = 1) +
  expand_limits(y = c(-4.5, 7.5)) +
  labs(y = "std dev.res", x = "xb", color = "program",
       title = "Absence: Negbin model") +
  theme(text = element_text(size = 18))+
  facet_wrap(~ NewParts)

## parts ####
# 探究不同的part之间，分布有没有不同

D_diff_red <- model_poi_red$deviance - model_nb$deviance
D_diff_red

df_diff_red <- model_poi_red$df.residual - model_nb$df.residual
df_diff_red

anova(model_poi_red, model_nb)
qchisq(1 - 0.05, df_diff_red)
pchisq(D_diff_red, df_diff_red, lower.tail = FALSE)

