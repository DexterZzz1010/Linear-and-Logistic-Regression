### PROJECT 2 ###
# Part 1 ######
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(GGally)
library(tidyr)

# 导入数据
kommuner <- read_excel("Data/kommunerProject2.xlsx")
summary(kommuner)

## a #####
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


## b #####
## model 1b #####
model_1b<- glm(highcars ~ Part, 
                      family = "binomial", 
                      data = kommuner)
summary(model_1b)
model_1b_sum <- summary(model_1b)



### β-estimates #####
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

### Wald test #####
model_1b_sum$coefficients
b1 <- model_1b_sum$coefficients[2, "Estimate"]
se.b1 <- model_1b_sum$coefficients[2, "Std. Error"]
z.b1 <- model_1b_sum$coefficients[2, "z value"]

print(z.b1)

# Pr < 0.05
# z > 1.96

### LR test -  the null hypothesis #####
D_diff <- model_1b$null.deviance - model_1b$deviance
df_diff <- model_1b$df.null - model_1b$df.residual
chi2_alpha <- qchisq(p = 1 - 0.05, df = df_diff)
Pvalue <- pchisq(q = D_diff, df = df_diff, lower.tail = FALSE)

# test output
cbind(D_diff, df_diff, chi2_alpha, Pvalue)

## c #####
### plot Highcars(0/1) against Transit #####
ggplot(kommuner, aes(Transit, highcars)) +
geom_point() +
geom_smooth() +
xlab("Transit") +
ylab("High Cars") +
labs(title = "Highcars(0/1) against Transit") +
theme(text = element_text(size = 14))

### model 1c #####
model_1c<- glm(highcars ~ Transit, 
               family = "binomial", 
               data = kommuner)
summary(model_1c)
model_1c_sum <- summary(model_1c)

### β-estimates #####
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


### Estimated probabilities #####
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

### Plot with confidence interval #####
#### Step 1: Conf.int. for the linear predictor, logodds ######
model_1c_pred <- cbind(
  model_1c_pred,
  logit = predict(model_1c, se.fit = TRUE))
glimpse(model_1c_pred)

lambda <- qnorm(1 - 0.05/2)
model_1c_pred |> mutate(
  logit.lwr = logit.fit - lambda*logit.se.fit,
  logit.upr = logit.fit + lambda*logit.se.fit) -> model_1c_pred
glimpse(model_1c_pred)

#### Step 2: Confidence interval for the odds ######
model_1c_pred |> mutate(
  odds.lwr = exp(logit.lwr),
  odds.upr = exp(logit.upr)) -> model_1c_pred
glimpse(model_1c_pred)

#### Step 3: Confidence interval for the probabilities #####
model_1c_pred |> mutate(
  p.lwr = odds.lwr/(1 + odds.lwr),
  p.upr = odds.upr/(1 + odds.upr)) -> model_1c_pred
glimpse(model_1c_pred)

### Plot estimated probability and  confidence interval #####
ggplot(model_1c_pred, aes(Transit, highcars)) +
  geom_point() +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars",
       caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 14))

### Wald test #####
model_1c_sum$coefficients
b1 <- model_1c_sum$coefficients[2, "Estimate"]
se.b1 <- model_1c_sum$coefficients[2, "Std. Error"]
z.b1 <- model_1c_sum$coefficients[2, "z value"]

print(z.b1)

# Pr < 0.05
## |z| > 1.96





## d ######
### Leverage:
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


## e #####

### AIC and BIC #####
aic <- AIC(model_1b,model_1c)
bic <- BIC(model_1b,model_1c)
collect.AICetc <- data.frame(aic, bic)
collect.AICetc |> mutate(df.1 = NULL) -> collect.AICetc
collect.AICetc

### Pseudo R2 ####
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







# Part 2 ######


# 导入数据
kommuner <- read_excel("Data/kommunerProject2.xlsx")
summary(kommuner)

## a #####
kommuner |> mutate(highcars = as.numeric(Cars > 600)) -> kommuner

kommuner <- mutate(kommuner,
                   highcars_cat = factor(highcars,
                                         levels = c(0, 1),
                                         labels = c("low", "high")))


kommuner |> mutate(Fertility = as.numeric(Fertility)) -> kommuner

kommuner |> filter(Part == 3 & Coastal == 0) |>
  summarise(meanfertility = mean(Fertility, na.rm = TRUE))
I <- which(is.na(kommuner$Fertility))
kommuner$Fertility[I] <- 1.57 # 1.57



## b #####
model_full <- glm(highcars ~ log(Higheds)+Children+Seniors+log(Income)+log(GRP)+ Persperhh+Fertility+Urban+Transit+Apartments, 
                  family = "binomial", 
                  data = kommuner)
summary(model_full)
model_full_sum <- summary(model_full)
vif(model_full)


model_null <- glm(highcars ~ 1, family = "binomial", data = kommuner)

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
vif(step_model_bic)

#### AIC and BIC #####
aic <- AIC(step_model_aic,step_model_bic)
bic <- BIC(step_model_aic,step_model_bic)
collect.AICetc <- data.frame(aic, bic)
collect.AICetc |> mutate(df.1 = NULL) -> collect.AICetc
collect.AICetc

#### Pseudo R2 ####
model_0_glm <- glm(highcars ~ 1, family = "binomial", data = kommuner)

# log likelihood
lnL0 <- logLik(model_0_glm)[1]
collect.AICetc |> mutate(
  loglik =  c(logLik(step_model_aic)[1],
              logLik(step_model_bic)[1])) -> collect.AICetc
collect.AICetc

# McFadden
collect.AICetc |> mutate(
  R2McF = 1 - loglik/lnL0,
  R2McF.adj = 1 - (loglik - (df - 1)/2)/lnL0) -> collect.AICetc
collect.AICetc

### model_2b #####
model_2b <- step_model_bic
summary(model_2b)

## c #####

### leverage #####
model_2b_infl <- influence(model_2b)
glimpse(model_2b_infl)

#### kommuner_pred #####
kommuner_pred <- mutate(kommuner,
                        xbeta = predict(model_2b),
                        v = model_2b_infl$hat,
                        D = cooks.distance(model_2b),
                        devresid = model_2b_infl$dev.res,
                        stddevresid = devresid/sqrt(1 - v))

pplus1 <- length(model_2b$coefficients)
n <- nobs(model_2b)

# Get top leverage
#计算模型中参数的数量加1 (pplus1) 和观测值的总数 (n)。
#提取六个杠杆值最高的点 (top_leverage)。
#使用 ggplot 绘制所有数据的杠杆值，并在图中突出显示这六个最高杠杆值点。使用 geom_point 显示点，geom_text 添加标签。
#添加两条水平线，一条表示 1/n（所有点的平均杠杆值），一条表示 2(p+1)/n（高杠杆值的临界线）。
top_leverage <- kommuner_pred %>%
  arrange(desc(v)) %>%
  slice(1:6)


# 杠杆值高的观测点在回归线的确定中具有更大的权重，可能会对回归结果产生显著的影响，特别是如果这些观测点也是异常值的话。

ggplot(kommuner_pred, aes(x = xbeta, y = v)) +
  geom_point(size = 2)  +
  geom_point(data = top_leverage, aes(x = xbeta, y = v), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = xbeta, y = v, label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = 1/n, linetype = "dashed",color = "red") +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Kommuner: leverage vs yhat",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))




### cook #####
f1.kommuner <- pplus1
f2.kommuner <- model_2b$df.residual
cook.limit.kommuner <- qf(0.5, f1.kommuner, f2.kommuner)

top_cooks <- kommuner_pred %>%
  arrange(desc(D)) %>%
  slice(1:6)
# summary(model_2b)
#  Identify the six municipalities with the highest Cook’s distance and highlight them in the plot.
ggplot(kommuner_pred, aes(xbeta, D)) + 
  geom_point(size = 3) +
  geom_point(data = top_cooks, color = "red", size = 3) +  
  geom_text(data = top_cooks, aes(x = xbeta, y = D, label = Kommun), vjust = -1, color = "blue") +
  geom_hline(yintercept = cook.limit.kommuner, color = "red") +
  geom_hline(yintercept = 4/n, linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Kommuner: Cook's D",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18)) 



### DFBETAS #####
head(dfbetas(model_2b))
dfbetas_values <- dfbetas(model_2b)
max_dfbetas_indices <- apply(dfbetas_values, 2, which.max)
top_cooks_indices <- which(rownames(kommuner_pred) %in% rownames(top_cooks))
high_cooks_dfbetas <- dfbetas_values[top_cooks_indices, ]


kommuner_excl_pred <- mutate(
  kommuner,
  df0 = dfbetas(model_2b)[, "(Intercept)"],
  df1 = dfbetas(model_2b)[, "Urban"],
  df2 = dfbetas(model_2b)[, "Apartments"],
  df3 = dfbetas(model_2b)[, "Persperhh"],
  df4 = dfbetas(model_2b)[, "log(Income)"],
  fit = predict(model_2b),
  r = rstudent(model_2b),
  D = cooks.distance(model_2b))

n <- nrow(model_2b$model)  # 总样本大小
significant_threshold <- 2 / sqrt(n)

print(abs(high_cooks_dfbetas) > significant_threshold)

i=0
for (i in 1:nrow(high_cooks_dfbetas)) {
  cat("Observation", top_cooks_indices[i], "affects parameters:\n")
  affected_params <- which(abs(high_cooks_dfbetas[i, ]) > significant_threshold)
  print(names(dfbetas_values)[affected_params])
}


## d #####
### QQ #####
ggplot(oslo_pred, aes(sample = stddevresid)) +
  geom_qq() + geom_qq_line()

### standard deviance
highlightshapes <- c("Cook's D>0.1" = 24)
highlightcolors <- c("|d*|>3" = "green")

ggplot(kommuner_pred, aes(x = xbeta, 
                          y = stddevresid, 
                          color = as.factor(highcars))) +
  geom_point() +
  geom_point(data = filter(kommuner_pred, abs(stddevresid) > 3),
             aes(color = "|r*|>3"), size = 3) +
  geom_point(data = top_cooks, color = "red", size = 3) +  
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), linetype = "dashed")


# Part 3 ######
library(readxl)
library(caret)
library(pROC)
library(ResourceSelection)
library(gridExtra)
## a #####
pred_phat <- cbind(
  kommuner,
  p_null = predict(model_null, type = "response"),
  p_1b = predict(model_1b, type = "response"), 
  p_1c = predict(model_1c, type = "response"), 
  p_bic = predict(step_model_bic, type = "response"), 
  p_aic = predict(step_model_aic, type = "response"), 
  p_full = predict(model_full, type = "response") 
  
)
glimpse(pred_phat)

pred_phat |> mutate(
  yhat_kommuner = factor(p_1c > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high"))) -> pred_phat

cm_bic <- confusionMatrix(
  data = pred_phat$yhat_kommuner, 
  reference = pred_phat$highcars_cat,
  positive = "high")
cm_bic

# 使用管道和 mutate 创建每个模型的预测分类，并计算混淆矩阵
model_predictions <- pred_phat |> mutate(
  yhat_null = factor(p_null > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_1b = factor(p_1b > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_1c = factor(p_1c > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_bic = factor(p_bic > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_aic = factor(p_aic > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_full = factor(p_full > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high"))
)

# 生成每个模型的混淆矩阵
cm_null <- confusionMatrix(data = model_predictions$yhat_null, reference = model_predictions$highcars_cat, positive = "high")
cm_1b <- confusionMatrix(data = model_predictions$yhat_1b, reference = model_predictions$highcars_cat, positive = "high")
cm_1c <- confusionMatrix(data = model_predictions$yhat_1c, reference = model_predictions$highcars_cat, positive = "high")
cm_bic <- confusionMatrix(data = model_predictions$yhat_bic, reference = model_predictions$highcars_cat, positive = "high")
cm_aic <- confusionMatrix(data = model_predictions$yhat_aic, reference = model_predictions$highcars_cat, positive = "high")
cm_full <- confusionMatrix(data = model_predictions$yhat_full, reference = model_predictions$highcars_cat, positive = "high")

### Confusion matrix #####
print(cm_null)
print(cm_bic)
print(cm_aic)

## b #####
### The roc model #####
roc_null <- roc(highcars ~ p_null, data = pred_phat)
roc_1b <- roc(highcars ~ p_1b, data = pred_phat)
roc_1c <- roc(highcars ~ p_1c, data = pred_phat)
roc_aic <- roc(highcars ~ p_aic, data = pred_phat)
roc_2b <- roc(highcars ~ p_bic, data = pred_phat)
roc_full <- roc(highcars ~ p_full, data = pred_phat)

### ROC_2b #####
ggroc(roc_2b) +
  coord_fixed() +
  labs(title = "ROC-curve for model 2b")

### compare #####
ggroc(list(null = roc_aic, 'model 1b' = roc_1b, 'model 1c' = roc_1c,'model aic' =  roc_aic,
           'model 2b' = roc_2b, 'model full' = roc_full )) +
  coord_fixed() +
  labs(title = "ROC-curves for model oslo and the null model")


### ROC pair-wise compare #####

plot_list <- list()
names_list <- c("null", "1b", "1c", "aic", "2b", "full")  # 模型名称列表
roc_list <- list(roc_null, roc_1b, roc_1c, roc_aic, roc_2b, roc_full)
names(roc_list) <- paste("Model", names_list)  # 设置 ROC 对象的名称

# 创建对比的 ROC 曲线图
for (name in names_list) {
  if (name != "2b") {  # 排除与自身的比较
    # 构建一个列表，包含"Model 2b"和当前遍历的模型
    model_name <- paste("Model", name)
    comparison_list <- list(roc_list[["Model 2b"]], roc_list[[model_name]])
    names(comparison_list) <- c("Model 2b", model_name)  # 设置列表元素的名称
    
    plot_list[[name]] <- ggroc(comparison_list) +
      coord_fixed() +
      labs(title = sprintf("ROC: Model 2b vs %s", model_name)) +
      labs(colour = "Model")  # 添加图例标题
  }
}

# 展示所有图形
do.call(gridExtra::grid.arrange, c(plot_list, ncol = 2))

### AUC Calculations #####
aucs <- data.frame(
  model = c("null", "1b", "1c", "aic", "2b", "full"),
  auc = c(auc(roc_null), auc(roc_1b), auc(roc_1c), auc(roc_aic),
          auc(roc_2b), auc(roc_full)),
  lwr = c(ci(roc_null)[1], ci(roc_1b)[1],
          ci(roc_1c)[1], ci(roc_aic)[1],
          ci(roc_2b)[1], ci(roc_full)[1]),
  upr = c(ci(roc_null)[3], ci(roc_1b)[3],
          ci(roc_1c)[3], ci(roc_aic)[3],
          ci(roc_2b)[3], ci(roc_full)[3])
)

# Print the AUCs and confidence intervals
print(aucs)

### pair-wise tests comparing the AUC #####
# Perform ROC tests
test_2b_vs_null = roc.test(roc_2b, roc_null)
test_2b_vs_1b = roc.test(roc_2b, roc_1b)
test_2b_vs_1c = roc.test(roc_2b, roc_1c)
test_2b_vs_aic = roc.test(roc_2b, roc_aic)
test_2b_vs_full = roc.test(roc_2b, roc_full)

# Create a data frame to store the results
roc_comparison_results <- data.frame(
  Comparison = c("Model 2b vs Null", "Model 2b vs 1b", "Model 2b vs 1c", "Model 2b vs AIC", "Model 2b vs Full"),
  p_Value = c(test_2b_vs_null$p.value, test_2b_vs_1b$p.value, test_2b_vs_1c$p.value, test_2b_vs_aic$p.value, test_2b_vs_full$p.value),
  Test_Statistic = c(test_2b_vs_null$statistic, test_2b_vs_1b$statistic, test_2b_vs_1c$statistic, test_2b_vs_aic$statistic, test_2b_vs_full$statistic),
  Alternative_Hypothesis = rep(test_2b_vs_null$alternative, 5)
)

# Print the results table
print(roc_comparison_results)



## c #####
youden <- coords(roc_2b, "best")
topleft <- coords(roc_2b, "best", best.method = "closest.topleft")

youden$threshold
topleft$threshold

youden$name <- "youden"
topleft$name = "topleft"

ggroc(list(null = roc_aic, 'model 1b' = roc_1b, 'model 1c' = roc_1c,'model aic' =  roc_aic,
           'model 2b' = roc_2b, 'model full' = roc_full), linewidth = 1) +
  geom_point(data = topleft, aes(x = specificity, y = sensitivity), size = 3) +
  geom_point(data = youden, aes(x = specificity, y = sensitivity), size = 3) +
  coord_fixed() +
  labs(title = "ROC-curve for model oslo",
       subtitle = "with optimal thresholds") +
  theme(text = element_text(size = 18))


### threshold for each model #####
# Function to extract the best threshold
get_youden_threshold <- function(roc_obj) {
  # coords(roc_obj, "best")$threshold
  coords(roc_obj, "best", best.method = "closest.topleft")$threshold
}

# youden thresholds for each model
threshold_1b <- get_youden_threshold(roc_1b)
threshold_1c <- get_youden_threshold(roc_1c)
threshold_aic <- get_youden_threshold(roc_aic)
threshold_bic <- get_youden_threshold(roc_2b)
threshold_full <- get_youden_threshold(roc_full)



# Mutate to create predicted classifications using the best thresholds
model_best_predictions <- pred_phat |> mutate(
  yhat_1b = factor(p_1b > threshold_1b, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_1c = factor(p_1c > threshold_1c, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_bic = factor(p_bic > threshold_bic, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_aic = factor(p_aic > threshold_aic, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_full = factor(p_full > threshold_full, levels = c(FALSE, TRUE), labels = c("low", "high"))
)

# Compute confusion matrices using the predicted classifications
cm_best_1b <- confusionMatrix(data = model_best_predictions$yhat_1b, reference = model_best_predictions$highcars_cat, positive = "high")
cm_best_1c <- confusionMatrix(data = model_best_predictions$yhat_1c, reference = model_best_predictions$highcars_cat, positive = "high")
cm_best_bic <- confusionMatrix(data = model_best_predictions$yhat_bic, reference = model_best_predictions$highcars_cat, positive = "high")
cm_best_aic <- confusionMatrix(data = model_best_predictions$yhat_aic, reference = model_best_predictions$highcars_cat, positive = "high")
cm_best_full <- confusionMatrix(data = model_best_predictions$yhat_full, reference = model_best_predictions$highcars_cat, positive = "high")

# Print confusion matrices
print(cm_best_1b)
print(cm_best_1c)
print(cm_best_bic)
print(cm_best_aic)
print(cm_best_full)

print(cm_1b)
print(cm_1c)
print(cm_bic)
print(cm_aic)
print(cm_full)

