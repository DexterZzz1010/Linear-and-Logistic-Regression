#### Part 2 ####
## 2(a)
kommuner |> mutate(Fertility = as.numeric(Fertility)) -> kommuner
kommuner |> filter(Part == "Norrland" & Coastal == "No") |>
  summarise(meanfertility = mean(Fertility, na.rm = TRUE))
summary(kommuner)
I <- which(is.na(kommuner$Fertility))
# 把NA替换为平均值
kommuner$Fertility[I] <- 1.556

## 2(b)
# Higheds在Project1中也加了log
model_full <- glm(highcars ~ log(Higheds)+Children+Seniors+log(Income)+log(GRP)+ Persperhh+Fertility+Urban+Transit+log(Apartments), 
                  family = "binomial", 
                  data = kommuner)
summary(model_full)
vif(model_full)

model_null <- glm(highcars ~ 1, family = "binomial", data = kommuner)
summary(model_null)
# AIC
model_aic <- step(model_null,
                  scope = list(lower = model_null, upper = model_full),
                  direction = "both",
                  trace = 0,
                  k = 2)
model_aic_summary <- summary(model_aic)
vif(model_aic)
# BIC
model_bic <- step(model_null,
                  scope = list(lower = model_null, upper = model_full),
                  direction = "both",
                  trace = 0,
                  k = log(nobs(model_full)))
model_bic_summary <- summary(model_bic)
vif(model_bic)
# AIC模型采用的变量覆盖了BIC用的，要进行F检测看是否显著
# 这里的任务包括Report the null hypothesis, the type of test, the test statistic, 
# its distribution when H0 is true, the P-value and the conclusion
D_diff <- model_bic_summary$deviance - model_aic_summary$deviance
df_diff <- model_bic_summary$df.residual - model_aic_summary$df.residual
cbind(D_diff, df_diff)
chi2_alpha <- qchisq(1 - 0.05, df_diff)
Pvalue <- pchisq(D_diff, df_diff, lower.tail = FALSE)
cbind(D_diff, df_diff, chi2_alpha, Pvalue)
# Report R2, AIC and BIC 
aic <- AIC(model_aic,model_bic)
bic <- BIC(model_aic,model_bic)
collect.AICetc <- data.frame(aic, bic)
collect.AICetc |> mutate(df.1 = NULL) -> collect.AICetc
collect.AICetc

#### Pseudo R2 ####
model_0_glm <- glm(highcars ~ 1, family = "binomial", data = kommuner)

# log likelihood
lnL0 <- logLik(model_0_glm)[1]
collect.AICetc |> mutate(
  loglik =  c(logLik(model_aic)[1],
              logLik(model_bic)[1])) -> collect.AICetc
collect.AICetc

# McFadden
collect.AICetc |> mutate(
  R2McF = 1 - loglik/lnL0,
  R2McF.adj = 1 - (loglik - (df - 1)/2)/lnL0) -> collect.AICetc
collect.AICetc

model_2b <- model_bic
summary(model_2b)

## 2(c)
model_2b_infl <- influence(model_2b)
glimpse(model_2b_infl)
kommuner_pred <- cbind(kommuner,
                       xbeta = predict(model_2b),
                       v = model_2b_infl$hat)
glimpse(kommuner_pred)
pplus1_kommuner <- length(model_2b$coefficients)
n <- nobs(model_2b)

ggplot(kommuner_pred, aes(x = xbeta, y = v)) +
  geom_point() +
  geom_hline(yintercept = c(2*pplus1_kommuner/n)) +
  facet_wrap(~ highcars)
kommuner_pred |> slice_max(v, n = 8)

ggplot(kommuner_pred, aes(x = xbeta, y = v)) +
  geom_point() +
  geom_point(data = filter(kommuner_pred, v > 0.0636), 
             aes(color = "v > 0.045"), size = 3) +
  geom_hline(yintercept = c(2*pplus1_kommuner/n)) +
  geom_text_repel(data = filter(kommuner_pred, v > 0.0636), aes(x = xbeta, y = v, label = Kommun), color = "blue") +
  labs(color = "Highlight",
       title = "Leverage vs linear predictor") +
  theme(legend.position = "top",
        text = element_text(size = 14))
filter(kommuner_pred, v > 0.0636)

kommuner_pred <- mutate(kommuner_pred, 
                        Dcook = cooks.distance(model_2b))
kommuner_pred |> slice_max(Dcook, n = 8)
ggplot(kommuner_pred, aes(x = xbeta, y = Dcook, color = as.factor(highcars_cat))) +
  geom_point() +
  geom_point(data = filter(kommuner_pred, Dcook > 0.039), shape = 24,
             color = "black", size = 3) +
  geom_hline(yintercept = 4/n) +
  labs(color = "Highcars_cat = 0/1")+
  labs(title = "Cook's distance vs linear predictor")+
  theme(legend.position = "top",
        text = element_text(size = 14))

### cook #####
f1.kommuner <- pplus1_kommuner
f2.kommuner <- model_2b$df.residual
cook.limit.kommuner <- qf(0.5, f1.kommuner, f2.kommuner)

top_cooks <- kommuner_pred %>%
  arrange(desc(Dcook)) %>%
  slice(1:8)
# summary(model_2b)
#  Identify the six municipalities with the highest Cook’s distance and highlight them in the plot.
ggplot(kommuner_pred, aes(xbeta, Dcook)) + 
  geom_point(size = 3) +
  geom_point(data = top_cooks, color = "red", size = 3) +  
  geom_text(data = top_cooks, aes(x = xbeta, y = Dcook, label = Kommun), vjust = -1, color = "blue") +
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
  df2 = dfbetas(model_2b)[, "log(Apartments)"],
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

## 2(d)
kommuner_pred <- mutate(kommuner,
                        xbeta = predict(model_2b),
                        v = model_2b_infl$hat,
                        D = cooks.distance(model_2b),
                        devresid = model_2b_infl$dev.res,
                        stddevresid = resid(model_2b)/sd(resid(model_2b)))

ggplot(kommuner_pred, aes(sample = stddevresid)) +
  geom_qq() + geom_qq_line()

### standard deviance
highlightshapes <- c("Cook's D>0.1" = 24)
highlightcolors <- c("|d*|>3" = "green")
stddevresid <- kommuner_pred$stddevresid
# 绘制标准化偏差残差与线性预测变量的散点图
# 绘制标准化偏差残差与线性预测变量的散点图，并标注具有高Cook's D值的观测结果
ggplot(kommuner_pred, aes(x = xbeta, y = stddevresid, color = as.factor(highcars))) +
  geom_point() +
  geom_point(data = filter(kommuner_pred, abs(stddevresid) > 3), aes(color = "|di| > 3"), size = 3) +
  geom_point(data = top_cooks, aes(color = "High Cook's D"), size = 3) + 
  geom_point(data = filter(kommuner_pred, abs(stddevresid) > 3),
             aes(color = "Large deviance residual"), size = 3) +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), linetype = "dashed") +
  labs(title = "Standardised Deviance Residuals vs. Linear Predictor",
       x = "Linear Predictor",
       y = "Standardised Deviance Residuals") +
  scale_color_manual(values = c("0" = "blue", "1" = "orange", "High Cook's D" = "red", "Large deviance residual" = "green")) +
  theme_minimal()+
  theme(legend.position = "top",
        text = element_text(size = 14))

# 创建四个散点图
plot1 <- ggplot(kommuner_pred, aes(x = Urban, y = stddevresid)) +
  geom_point() +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), linetype = "dashed") +
  labs(title = "Standardised Deviance Residuals vs. Urban",
       x = "Urban",
       y = "Standardised Deviance Residuals") +
  theme_minimal()

plot2 <- ggplot(kommuner_pred, aes(x = log(Apartments), y = stddevresid)) +
  geom_point() +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), linetype = "dashed") +
  labs(title = "Standardised Deviance Residuals vs. log(Apartments)",
       x = "log(Apartments)",
       y = "Standardised Deviance Residuals") +
  theme_minimal()

plot3 <- ggplot(kommuner_pred, aes(x = Persperhh, y = stddevresid)) +
  geom_point() +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), linetype = "dashed") +
  labs(title = "Standardised Deviance Residuals vs. Persperhh",
       x = "Persperhh",
       y = "Standardised Deviance Residuals") +
  theme_minimal()

plot4 <- ggplot(kommuner_pred, aes(x = log(Income), y = stddevresid)) +
  geom_point() +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), linetype = "dashed") +
  labs(title = "Standardised Deviance Residuals vs. log(Income)",
       x = "log(Income)",
       y = "Standardised Deviance Residuals") +
  theme_minimal()

# 组合四个图
combined_plot <- plot1 + plot2 + plot3 + plot4

# 显示组合图
combined_plot