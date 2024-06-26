### PROJECT 1 ###
## Part 3 ##
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(GGally)
library(patchwork)


# a #####
kommuner <- read_excel("Data/kommuner.xlsx")
kommuner <- mutate(kommuner, Coastal = coast) # 替代原来的Coastal
kommuner$Part <- factor(kommuner$Part, labels = c("Gotaland", "Svealand", "Norrland"))
kommuner <- mutate(kommuner, Coastal = relevel(Coastal,"Yes"))

kommuner <-
  mutate(kommuner, NewParts =
           as.numeric(Part == "Gotaland"| Coastal == "Yes") +
           3*as.numeric(Part == "Svealand"  & Coastal == "No") +
           4*as.numeric(Part == "Norrland" & Coastal == "No"))
kommuner$NewParts <- factor(kommuner$NewParts, labels = c("GotalandorYes", "SvealandandNo","NorrlandandNo"))
model_2e <- lm(log(PM10)~log(Vehicles)+log(Higheds)+Children+log(Income)+log(GRP)+NewParts, data=kommuner)
# model_2e <- lm(log(PM10)~log(Vehicles)+Children+log(Income)+NewParts, data=kommuner)
summary(model_2e)
## leverage #####
kommuner_pred <- mutate(kommuner,
                    yhat = predict(model_2e),
                    r = rstudent(model_2e),
                    v = hatvalues(model_2e),
                    D = cooks.distance(model_2e))

pplus1 <- length(model_2e$coefficients)
n <- nobs(model_2e)

# Get top leverage
#计算模型中参数的数量加1 (pplus1) 和观测值的总数 (n)。
#提取六个杠杆值最高的点 (top_leverage)。
#使用 ggplot 绘制所有数据的杠杆值，并在图中突出显示这六个最高杠杆值点。使用 geom_point 显示点，geom_text 添加标签。
#添加两条水平线，一条表示 1/n（所有点的平均杠杆值），一条表示 2(p+1)/n（高杠杆值的临界线）。
top_leverage <- kommuner_pred %>%
  arrange(desc(v)) %>%
  slice(1:6)


# 杠杆值高的观测点在回归线的确定中具有更大的权重，可能会对回归结果产生显著的影响，特别是如果这些观测点也是异常值的话。

ggplot(kommuner_pred, aes(x = yhat, y = v)) +
  geom_point(size = 2)  +
  geom_point(data = top_leverage, aes(x = yhat, y = v), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = yhat, y = v, label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Kommuner: leverage vs yhat",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))


ggplot(kommuner_pred, aes(x = log(Vehicles) , y = log(PM10))) +
  geom_point(size = 2)  +
  facet_wrap(~NewParts) +
  geom_point(data = top_leverage, aes(x = log(Vehicles) , y = log(PM10)), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = log(Vehicles) , y = log(PM10), label = Kommun), vjust = -1, color = "blue") + 
  labs(title = "Kommuner: leverage vs yhat",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))

ggplot(kommuner_pred, aes(x = Children , y = log(PM10))) +
  geom_point(size = 2)  +
  facet_wrap(~NewParts) +
  geom_point(data = top_leverage, aes(x = Children , y = log(PM10)), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = Children , y = log(PM10), label = Kommun), vjust = -1, color = "blue") + 
  labs(title = "Kommuner: leverage vs yhat",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))

ggplot(kommuner_pred, aes(x = log(Income) , y = log(PM10))) +
  geom_point(size = 2)  +
  facet_wrap(~NewParts) +
  geom_point(data = top_leverage, aes(x = log(Income) , y = log(PM10)), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = log(Income) , y = log(PM10), label = Kommun), vjust = -1, color = "blue") + 
  labs(title = "Kommuner: leverage vs yhat",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))

ggplot(kommuner_pred, aes(x = log(GRP) , y = log(PM10))) +
  geom_point(size = 2)  +
  facet_wrap(~NewParts) +
  geom_point(data = top_leverage, aes(x = log(GRP) , y = log(PM10)), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = log(GRP) , y = log(PM10), label = Kommun), vjust = -1, color = "blue") + 
  labs(title = "Kommuner: leverage vs yhat",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))

ggplot(kommuner_pred, aes(x = log(Higheds) , y = log(PM10))) +
  geom_point(size = 2)  +
  facet_wrap(~NewParts) +
  geom_point(data = top_leverage, aes(x = log(Higheds) , y = log(PM10)), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = log(Higheds) , y = log(PM10), label = Kommun), vjust = -1, color = "blue") + 
  labs(title = "Kommuner: leverage vs yhat",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))

# b #####
#这个度量考虑了每个数据点对回归系数估计的影响，是通过观察移除一个点后模型参数估计变化的程度来计算的。
#Cook的距离综合了杠杆值和残差大小的影响，因此能够识别那些不仅仅因为其残差或杠杆值大而具有潜在影响力的点。

# Leverage评估观测值对参数估计的影响,Cook's distance评估观测值对模型拟合的影响。
# Cook's distance考虑的范围更广,可以识别对模型拟合影响更大的异常点。
# 但Leverage也是重要指标,可以识别可能是异常点的观测值。

## cook #####
f1.kommuner <- pplus1
f2.kommuner <- model_2e$df.residual
cook.limit.kommuner <- qf(0.5, f1.kommuner, f2.kommuner)

top_cooks <- kommuner_pred %>%
  arrange(desc(D)) %>%
  slice(1:6)
summary(model_2e)
#  Identify the six municipalities with the highest Cook’s distance and highlight them in the plot.
ggplot(kommuner_pred, aes(yhat, D)) + 
  facet_wrap(~NewParts)+
  geom_point(size = 3) +
  geom_point(data = top_cooks, color = "red", size = 3) +  
  geom_text(data = top_cooks, aes(x = yhat, y = D, label = Kommun), vjust = -1, color = "blue") +
  geom_hline(yintercept = cook.limit.kommuner, color = "red") +
  geom_hline(yintercept = 4/n, linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Kommuner: Cook's D",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18)) 

## DFBETAS #####
# DFBETAS 是一个标准化的度量，它显示了一个观测值被移除时系数估计值的变化。
# 首先，我们需要计算模型中所有系数的 DFBETAS，然后找出最大的变化。

head(dfbetas(model_2e))
dfbetas_values <- dfbetas(model_2e)
max_dfbetas_indices <- apply(dfbetas_values, 2, which.max)
top_cooks_indices <- which(rownames(kommuner_pred) %in% rownames(top_cooks))
top_dfbetas <- dfbetas_values[top_cooks_indices, ]

# Get the name of these municipalities
influential_municipalities <- kommuner_pred$Kommun[max_dfbetas_indices]
summary(model_2e)

kommuner_excl_pred <- mutate(
  kommuner,
  df0 = dfbetas(model_2e)[, "(Intercept)"],
  df1 = dfbetas(model_2e)[, "log(Vehicles)"],
  df2 = dfbetas(model_2e)[, "log(Higheds)"],
  df3 = dfbetas(model_2e)[, "Children"],
  df4 = dfbetas(model_2e)[, "log(Income)"],
  df5 = dfbetas(model_2e)[, "log(GRP)"],
  fit = predict(model_2e),
  r = rstudent(model_2e),
  D = cooks.distance(model_2e))

top_cooks_excl <- kommuner_excl_pred %>%
  arrange(desc(D)) %>%
  slice(1:6)

highlightshapes <- c("Cook's D>0.1" = 24)
highlightcolors <- c("|r*|>3" = "red")

# Top influential municipalities in DFBETAS
top_influential <- kommuner_excl_pred %>%
  arrange(desc(abs(df2))) %>%
  arrange(desc(abs(df2))) %>%
  slice(1:6)

###plot log-PM10 against the corresponding variable(s)##########################################################
ggplot(kommuner_pred, aes(x = log(Vehicles) , y = log(PM10))) +
  geom_point() +
  geom_point(data = top_cooks_excl, color = "red", size = 3) +  
  geom_text(data = top_cooks_excl, aes(x = log(Vehicles),  y = log(PM10), label = Kommun), vjust = -1, color = "blue") +
  xlab("log(Vehicles)") +
  ylab("log(PM10)") +
  labs(title = "Influence of Municipalities on PM10",
       subtitle = "Red points indicate municipalities with significant influence on beta parameters") +
  theme(text = element_text(size = 16))

# Plot log(PM10) vs log(Higheds), maybe we need to change the β-parameter
ggplot(kommuner_pred, aes(x = log(Higheds) , y = log(PM10))) +
  geom_point() +
  geom_point(data = top_cooks_excl, color = "red", size = 3) +  
  geom_text(data = top_cooks_excl, aes(x = log(Higheds),  y = log(PM10), label = Kommun), vjust = -1, color = "blue") +
  xlab("log(Higheds)") +
  ylab("log(PM10)") +
  labs(title = "Influence of Municipalities on PM10",
       subtitle = "Red points indicate municipalities with significant influence on beta parameters") +
  theme(text = element_text(size = 16))

# Plot log(PM10) vs log(GRP), maybe we need to change the β-parameter
ggplot(kommuner_pred, aes(x = log(GRP) , y = log(PM10))) +
  geom_point() +
  geom_point(data = top_cooks_excl, color = "red", size = 3) +  
  geom_text(data = top_cooks_excl, aes(x = log(GRP),  y = log(PM10), label = Kommun), vjust = -1, color = "blue") +
  xlab("log(GRP)") +
  ylab("log(PM10)") +
  labs(title = "Influence of Municipalities on PM10",
       subtitle = "Red points indicate municipalities with significant influence on beta parameters") +
  theme(text = element_text(size = 16))

# Plot log(PM10) vs Children, maybe we need to change the β-parameter
ggplot(kommuner_pred, aes(x = Children, y = log(PM10))) +
  geom_point() +
  geom_point(data = top_cooks_excl, color = "red", size = 3) +  
  geom_text(data = top_cooks_excl, aes(x = Children,  y = log(PM10), label = Kommun), vjust = -1, color = "blue") +
  xlab("Children") +
  ylab("log(PM10)") +
  labs(title = "Influence of Municipalities on PM10",
       subtitle = "Red points indicate municipalities with significant influence on beta parameters") +
  theme(text = element_text(size = 16))

# Plot log(PM10) vs log(Income), maybe we need to change the β-parameter
ggplot(kommuner_pred, aes(x = log(Income) , y = log(PM10))) +
  geom_point() +
  geom_point(data = top_cooks_excl, color = "red", size = 3) +  
  geom_text(data = top_cooks_excl, aes(x = log(Income),  y = log(PM10), label = Kommun), vjust = -1, color = "blue") +
  xlab("log(Income)") +
  ylab("log(PM10)") +
  labs(title = "Influence of Municipalities on PM10",
       subtitle = "Red points indicate municipalities with significant influence on beta parameters") +
  theme(text = element_text(size = 16))



#############################################################################################################





###  DFBETAS Plot ########################################################################################
ggplot(kommuner_excl_pred, aes(x = log(PM10), y = df1)) + 
  geom_point(size = 3) +
  geom_point(data = top_cooks_excl, color = "red", size = 3) +  
  geom_text(data = top_cooks_excl, aes(x = log(PM10), y = df1, label = Kommun), vjust = -1, color = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.kommuner)*c(-1, 1),
             color = "red") +
  geom_hline(yintercept = 2/sqrt(n)*c(-1, 1),
             color = "red", linetype = "dashed") +
  xlab("log(PM10)") +
  ylab("df1 log(Vehicles)") +
  labs(title = "Kommuner: top influential",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18))

ggplot(kommuner_excl_pred, aes(x = log(PM10), y = df2)) + 
  geom_point(size = 3) +
  geom_point(data = top_cooks_excl, color = "red", size = 3) +  
  geom_text(data = top_cooks_excl, aes(x = log(PM10), y = df2, label = Kommun), vjust = -1, color = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.kommuner)*c(-1, 1),
             color = "red") +
  geom_hline(yintercept = 2/sqrt(n)*c(-1, 1),
             color = "red", linetype = "dashed") +
  xlab("log(PM10)") +
  ylab("df2 log(Higheds)") +
  labs(title = "Kommuner: top influential",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18))

ggplot(kommuner_excl_pred, aes(x = log(PM10), y = df3)) + 
  geom_point(size = 3) +
  geom_point(data = top_cooks_excl, color = "red", size = 3) +  
  geom_text(data = top_cooks_excl, aes(x = log(PM10), y = df3, label = Kommun), vjust = -1, color = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.kommuner)*c(-1, 1),
             color = "red") +
  geom_hline(yintercept = 2/sqrt(n)*c(-1, 1),
             color = "red", linetype = "dashed") +
  xlab("log(PM10)") +
  ylab("df3 Children") +
  labs(title = "Kommuner: top influential",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18))

ggplot(kommuner_excl_pred, aes(x = log(PM10), y = df4)) + 
  geom_point(size = 3) +
  geom_point(data = top_cooks_excl, color = "red", size = 3) +  
  geom_text(data = top_cooks_excl, aes(x = log(PM10), y = df4, label = Kommun), vjust = -1, color = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.kommuner)*c(-1, 1),
             color = "red") +
  geom_hline(yintercept = 2/sqrt(n)*c(-1, 1),
             color = "red", linetype = "dashed") +
  xlab("log(PM10)") +
  ylab("df4 log(Income)") +
  labs(title = "Kommuner: top influential",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18))

ggplot(kommuner_excl_pred, aes(x = log(PM10), y = df5)) + 
  geom_point(size = 3) +
  geom_point(data = top_cooks_excl, color = "red", size = 3) +  
  geom_text(data = top_cooks_excl, aes(x = log(PM10), y = df5, label = Kommun), vjust = -1, color = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.kommuner)*c(-1, 1),
             color = "red") +
  geom_hline(yintercept = 2/sqrt(n)*c(-1, 1),
             color = "red", linetype = "dashed") +
  xlab("log(PM10)") +
  ylab("df5 log(GRP)") +
  labs(title = "Kommuner: top influential",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18))

##########################################################################################

ggplot(kommuner_excl_pred, aes(x = log(Vehicles), y = log(PM10))) + 
  geom_point(size = 3) +
  geom_point(data = top_influential, color = "red", size = 3) +  
  geom_text(data = top_influential, aes(x = log(Vehicles), y = log(PM10), label = Kommun), vjust = -1, color = "blue") +
  xlab("log(Vehicles)") +
  ylab("log(PM10)") +
  labs(title = "Kommuner: top influential",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18))


# c ####
## Studentized residuals. #####

ggplot(kommuner_excl_pred, aes(x = log(Vehicles), y = log(PM10))) +
  geom_point(size = 2) +
  geom_point(data = filter(kommuner_excl_pred, abs(r) > 3),
             aes(color = "|r*|>3"), size = 3) +
  geom_point(data = filter(kommuner_excl_pred, D > 0.1),
             aes(shape = "Cook's D>0.1"), size = 3) +
  ylab("log(PM10)") +
  xlab("log(Vehicles)") +
  labs(title = "Kommuner: |r*|>3 & Cook's D>0.1") +
  theme(text = element_text(size = 18), 
        legend.position = "bottom") +
  scale_color_manual(values = highlightcolors) +
  scale_shape_manual(values = highlightshapes)

filter(kommuner_excl_pred, abs(r) > 3)

top_cooks <- kommuner_excl_pred %>%
  arrange(desc(D)) %>%
  slice(1:6)

# Calculate the studentized residuals, ri∗, for Model 2(e) and plot them
# against the linear predictor, using suitable horizontal lines as visual guides. Highlight the six
# municipalities with the highest Cook’s distance.

ggplot(kommuner_excl_pred, aes(x = fit, y = r)) +
  geom_point(size = 2) +
  geom_point(data = top_cooks, color = "red", size = 3) +  
  geom_text(data = top_cooks, aes(x = fit, y = r, label = Kommun), vjust = -1, color = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.kommuner)*c(-1, 1),
             color = "red") +
  geom_hline(yintercept = 2/sqrt(n)*c(-1, 1),
             color = "red", linetype = "dashed") +
  ylab("DFBETAS_0(i)") +
  xlab("Fitted values") +
  labs(title = "Kommuner: studentized residuals",
       caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18), 
        legend.position = "bottom") +
  scale_color_manual(values = highlightcolors) +
  scale_shape_manual(values = highlightshapes)

## |ri∗| > 3 but which did not have high Cook’s distance.

high_residuals <- filter(kommuner_excl_pred, abs(r) > 3)

print(high_residuals$Kommun)
print(high_residuals$r)

# Plot sqrt(|r*|) against fitted values and label the points where |r*| > 3.
ggplot(kommuner_excl_pred, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 2) +
  geom_point(data = high_residuals, aes(color = "|r*|>3"), size = 4) +
  geom_text(data = high_residuals, aes(label = Kommun), vjust = 2, color = "blue", size = 3) +  
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = "dashed") +
  labs(title = " sqrt |r| against the linear predictor",
       subtitle = "Analysis of variance stabilization",
       caption = "Reference lines at y = sqrt(0.75 quantile of normal), sqrt(2), sqrt(3)") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  scale_color_manual(values = c("|r*|>3" = "red"))

# d #####
remove <- c("0481 Oxelösund", "1082 Karlshamn", "0861 Mönsterås",
                           "2523 Gällivare", "2514 Kalix", "2584 Kiruna",
                           "1480 Göteborg", "1761 Hammarö", "1484 Lysekil",
                           "1494 Lidköping", "1882 Askersund", "2284 Örnsköldsvik",
                           "0319 Älvkarleby", "1460 Bengtsfors",  "1781 Kristinehamn", 
                           "2262 Timrå",  "0980 Gotland", "1272 Bromölla",  "1885 Lindesberg","1764 Grums")

kommuner_ <- kommuner %>%
  filter(!Kommun %in% remove)
model_3d <- update(model_2e, data = kommuner_)
kommuner_pred_ <- mutate(kommuner_,
                        yhat = predict(model_3d),
                        r = rstudent(model_3d),
                        v = hatvalues(model_3d),
                        D = cooks.distance(model_3d))
kommuner_pred_excl_ <- mutate(
  kommuner_pred_,
  df0 = dfbetas(model_3d)[, "(Intercept)"],
  df1 = dfbetas(model_3d)[, "log(Vehicles)"],
  df2 = dfbetas(model_3d)[, "log(Higheds)"],
  df3 = dfbetas(model_3d)[, "Children"],
  df4 = dfbetas(model_3d)[, "log(Income)"],
  df5 = dfbetas(model_3d)[, "log(GRP)"],
  fit = predict(model_3d),
  r = rstudent(model_3d),
  D = cooks.distance(model_3d))

# Get municipality with high residuals
high_residuals_excl <- filter(kommuner_pred_excl_, abs(r) > 3)

# Plot sqrt(|r*|) against fitted values and label the points where |r*| > 3.
ggplot(kommuner_pred_excl_, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 2) +
  geom_point(data = high_residuals_excl, aes(color = "|r*|>3"), size = 4) +
  geom_text(data = high_residuals_excl, aes(label = Kommun), vjust = 2, color = "blue", size = 3) +  
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = "dashed") +
  labs(title = "sqrt(|r*|) vs refitted values",
       subtitle = "Analysis of variance stabilization",
       caption = "Reference lines at y = sqrt(0.75 quantile of normal), sqrt(2), sqrt(3)") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  scale_color_manual(values = c("|r*|>3" = "red"))

# summary & confint of both models
summary(model_3d)
confint(model_3d)

summary(model_2e)
confint(model_2e)



# 假设mod_a和mod_b是两个线性模型对象
res_a <- residuals(model_3d)
res_b <- residuals(model_2e)

# 检查正态性
# Q-Q图
par(mfrow=c(2, 2)) # 设置图形排列为2行2列
qqnorm(res_a)
qqline(res_a)
qqnorm(res_b)
qqline(res_b)

# 检查恒定方差
# 残差 vs. 拟合值
fitted_a <- fitted(model_3d)
fitted_b <- fitted(model_2e)

plot(fitted_a, res_a, xlab="Fitted Values", ylab="Residuals", main="model_3d")
abline(h=0, col="red")
plot(fitted_b, res_b, xlab="Fitted Values", ylab="Residuals", main="model_2e")
abline(h=0, col="red")

# Shapiro-Wilk 正态性检验
shapiro.test(res_a)
shapiro.test(res_b)

# Breusch-Pagan 恒定方差检验
library(lmtest)
bptest(model_3d)
bptest(model_2e)






# 3(e). Variable selection.

# prepare models for stepwise selection
model_null <- lm(log(PM10) ~ 1, data = kommuner_)
model_null_sum <- summary(model_null)

model_1b <- lm(log(PM10) ~ log(Vehicles), data = kommuner_)
model_1b_sum <- summary(model_1b)

model_2c <- lm(log(PM10)~ log(Vehicles) + NewParts, data=kommuner_)
model_2c_sum <- summary(model_2c)

model_3d <- update(model_2e, data = kommuner_)
model_3d_sum <- summary(model_3d)

# AIC stepwise selection
step_model_aic <- step(model_1b,
                       scope = list(lower = model_null, upper = model_3d),
                       direction = "both",
                       trace = TRUE,  # trace=TRUE detail information for every steps
                       k = 2)  # k=2 means using AIC

summary(step_model_aic)


# BIC stepwise selection
step_model_bic <- step(model_1b,
                       scope = list(lower = model_null, upper = model_3d),
                       direction = "both",
                       trace = TRUE,
                       k =  log(nobs(model_3d)))  # BIC
summary(step_model_bic)

# Gathering statistics for each model
model_stats <- function(model) {
  data.frame(
    Beta_Parameters = length(coef(model)),  # Number of beta parameters
    Residual_SD = sigma(model),  # Residual Standard Deviation
    R_Squared = summary(model)$r.squared,  # R-squared
    Adjusted_R_Squared = summary(model)$adj.r.squared,  # Adjusted R-squared
    AIC = AIC(model),  # AIC
    BIC = BIC(model)  # BIC
  )
}

# Apply function to each model and combine results
model_comparison <- data.frame(
  Model = c("Null", "Model 1(b)", "Model 2(c)", "Model 3(d)", "AIC Model", "BIC Model"),
  rbind(
    model_stats(model_null),
    model_stats(model_1b),
    model_stats(model_2c),
    model_stats(model_3d),
    model_stats(step_model_aic),
    model_stats(step_model_bic)
  )
)

# Print the comparison table
print(model_comparison)

