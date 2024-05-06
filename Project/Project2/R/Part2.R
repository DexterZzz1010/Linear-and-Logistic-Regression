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


kommuner |> mutate(Fertility = as.numeric(Fertility)) -> kommuner

kommuner |> filter(Part == 3 & Coastal == 0) |>
  summarise(meanfertility = mean(Fertility, na.rm = TRUE))
I <- which(is.na(kommuner$Fertility))
kommuner$Fertility[I] <- 1.57 # 1.57



# b #####
model_full <- glm(highcars ~ log(Higheds)+Children+Seniors+log(Income)+log(GRP)+ Persperhh+Fertility+Urban+Transit+Apartments, 
               family = "binomial", 
               data = kommuner)
summary(model_full)
model_full_sum <- summary(model_full)
vif(model_full)


model_null <- glm(highcars ~ 1, family = "binomial", data = kommuner)

## AIC stepwise selection ####
step_model_aic <- step(model_null,
                       scope = list(lower = model_null, upper = model_full),
                       direction = "both",
                       trace = TRUE,  # trace=TRUE detail information for every steps
                       k = 2)  # k=2 means using AIC

summary(step_model_aic)
aic_sum <- summary(step_model_aic)




## BIC stepwise selection ####
step_model_bic <- step(model_null,
                       scope = list(lower = model_null, upper = model_full),
                       direction = "both",
                       trace = TRUE,
                       k =  log(nobs(model_full)))  # BIC
summary(step_model_bic)
bic_sum <- summary(step_model_bic)


### AIC and BIC #####
aic <- AIC(step_model_aic,step_model_bic)
bic <- BIC(step_model_aic,step_model_bic)
collect.AICetc <- data.frame(aic, bic)
collect.AICetc |> mutate(df.1 = NULL) -> collect.AICetc
collect.AICetc

### Pseudo R2 ####
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

## model_2b
model_2b <- step_model_bic
summary(model_2b)

# c #####

## leverage #####
model_2b_infl <- influence(model_2b)
glimpse(model_2b_infl)

### kommuner_pred #####
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




## cook #####
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



## DFBETAS #####
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


# d #####
## QQ #####
ggplot(oslo_pred, aes(sample = stddevresid)) +
  geom_qq() + geom_qq_line()

## standard deviance
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
