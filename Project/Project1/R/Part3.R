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
           as.numeric(Part == "Gotaland" & Coastal == "No") +
           2*as.numeric(Part == "Svealand" & Coastal == "No") +
           3*as.numeric(Part == "Norrland" | Coastal == "Yes"))
kommuner$NewParts <- factor(kommuner$NewParts, labels = c("GotalandNo", "SvealandNo", "NorrlandYes"))
model_2e <- lm(log(PM10)~log(Vehicles)+log(Higheds)+Children+log(Income)+log(GRP)+NewParts, data=kommuner)
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

ggplot(kommuner_pred, aes(x = log(Vehicles), y = v)) +
  facet_wrap(~NewParts) +
  geom_point(size = 2)  +
  geom_point(data = top_leverage, aes(x = log(Vehicles), y = v), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = log(Vehicles), y = v, label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Kommuner: leverage vs log Vehicles",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))

# b #####
#这个度量考虑了每个数据点对回归系数估计的影响，是通过观察移除一个点后模型参数估计变化的程度来计算的。
#Cook的距离综合了杠杆值和残差大小的影响，因此能够识别那些不仅仅因为其残差或杠杆值大而具有潜在影响力的点。

## cook #####
f1.kommuner <- pplus1
f2.kommuner <- model_2e$df.residual
cook.limit.kommuner <- qf(0.5, f1.kommuner, f2.kommuner)

top_cooks <- kommuner_pred %>%
  arrange(desc(D)) %>%
  slice(1:6)

#  Identify the six municipalities with the highest Cook’s distance and highlight them in the plot.
ggplot(kommuner_pred, aes(yhat, D)) + 
  facet_wrap(~NewParts)+
  geom_point(size = 3) +
  geom_point(data = top_cooks, color = "red", size = 3) +  
  geom_text(data = top_cooks, aes(x = yhat, y = D, label = Kommun), vjust = -1, color = "blue") +
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

# Plot log(PM10) vs Vehicles, maybe we need to change the β-parameter
ggplot(kommuner_pred, aes(x = Vehicles, y = log(PM10))) +
  geom_point() +
  geom_point(data = kommuner_pred[kommuner_pred$Kommun %in% influential_municipalities, ],
             aes(x = Vehicles, y = log(PM10)), color = "red", size = 4) + 
  geom_text(data = kommuner_pred[kommuner_pred$Kommun %in% influential_municipalities, ],
            aes(x = Vehicles, y = log(PM10), label = Kommun), vjust = -1, color = "blue") +
  xlab("Vehicles (1000/capita)") +
  ylab("log(PM10) (g)") +
  labs(title = "Influence of Municipalities on PM10",
       subtitle = "Red points indicate municipalities with significant influence on beta parameters") +
  theme(text = element_text(size = 16))


# c ####
## Studentized residuals. #####
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

ggplot(kommuner_pred, aes(x = log(Vehicles), y = log(PM10))) + 
  geom_point(size = 3) +
  geom_point(data = top_influential, color = "red", size = 3) +  
  geom_text(data = top_influential, aes(x = log(Vehicles), y = log(PM10), label = Kommun), vjust = -1, color = "blue") +
  xlab("log(Vehicles)") +
  ylab("log(PM10)") +
  labs(title = "Kommuner: Cook's D",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18))


ggplot(kommuner_excl_pred, aes(x = log(Vehicles), y = log(PM10))) +
  geom_point(size = 2) +
  geom_point(data = filter(kommuner_excl_pred, abs(r) > 3),
             aes(color = "|r*|>3"), size = 3) +
  geom_point(data = filter(kommuner_excl_pred, D > 0.1),
             aes(shape = "Cook's D>0.1"), size = 3) +
  ylab("DFBETAS_0(i)") +
  xlab("Fitted values") +
  labs(title = "Pike: DFBETAS_0: impact on the intercept",
       subtitle = "without the strange fish",
       caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
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
  geom_point(data = top_cooks, aes(color = "Top Cook's D"), size = 4,color = "red") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.kommuner)*c(-1, 1),
             color = "red") +
  geom_hline(yintercept = 2/sqrt(n)*c(-1, 1),
             color = "red", linetype = "dashed") +
  ylab("DFBETAS_0(i)") +
  xlab("Fitted values") +
  labs(title = "Pike: DFBETAS_0: impact on the intercept",
       subtitle = "without the strange fish",
       caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18), 
        legend.position = "bottom") +
  scale_color_manual(values = highlightcolors) +
  scale_shape_manual(values = highlightshapes)

## |ri∗| > 3 but which did not have high Cook’s distance.

high_residuals <- filter(kommuner_excl_pred, abs(r) > 3)

# Plot sqrt(|r*|) against fitted values and label the points where |r*| > 3.
ggplot(kommuner_excl_pred, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 2) +
  geom_point(data = high_residuals, aes(color = "|r*|>3"), size = 4) +
  geom_text(data = high_residuals, aes(label = Kommun), vjust = 2, color = "blue", size = 3) +  
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = "dashed") +
  labs(title = " sqrt |ri∗| against the linear predictor",
       subtitle = "Analysis of variance stabilization",
       caption = "Reference lines at y = sqrt(0.75 quantile of normal), sqrt(2), sqrt(3)") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  scale_color_manual(values = c("|r*|>3" = "red"))

# d #####
remove <- c("0481 Oxelösund", "1082 Karlshamn", "0861 Mönsterås",
                           "2523 Gällivare", "1480 Göteborg", "2584 Kiruna",
                           "1484 Lysekil", "1761 Hammarö", "2514 Kalix",
                           "1882 Askersund", "2284 Örnsköldsvik","1471 Götene",
            "2262 Timrå","1460 Bengtsfors","0980 Gotland",
                           "1494 Lidköping","1781 Kristinehamn")

kommuner_ <- kommuner %>%
  filter(!Kommun %in% remove)
kommuner_excl_lm <- update(model_2e, data = kommuner_)
kommuner_pred_ <- mutate(kommuner_,
                        yhat = predict(kommuner_excl_lm),
                        r = rstudent(kommuner_excl_lm),
                        v = hatvalues(kommuner_excl_lm),
                        D = cooks.distance(kommuner_excl_lm))
kommuner_pred_excl_ <- mutate(
  kommuner_pred_,
  df0 = dfbetas(kommuner_excl_lm)[, "(Intercept)"],
  df1 = dfbetas(kommuner_excl_lm)[, "log(Vehicles)"],
  df2 = dfbetas(kommuner_excl_lm)[, "log(Higheds)"],
  df3 = dfbetas(kommuner_excl_lm)[, "Children"],
  df4 = dfbetas(kommuner_excl_lm)[, "log(Income)"],
  df5 = dfbetas(kommuner_excl_lm)[, "log(GRP)"],
  fit = predict(kommuner_excl_lm),
  r = rstudent(kommuner_excl_lm),
  D = cooks.distance(kommuner_excl_lm))

# Get municipality with high residuals
high_residuals_excl <- filter(kommuner_pred_excl_, abs(r) > 3)

# Plot sqrt(|r*|) against fitted values and label the points where |r*| > 3.
ggplot(kommuner_pred_excl_, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 2) +
  geom_point(data = high_residuals_excl, aes(color = "|r*|>3"), size = 4) +
  geom_text(data = high_residuals_excl, aes(label = Kommun), vjust = 2, color = "blue", size = 3) +  
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = "dashed") +
  labs(title = "sqrt(|r*|) vs fitted values",
       subtitle = "Analysis of variance stabilization",
       caption = "Reference lines at y = sqrt(0.75 quantile of normal), sqrt(2), sqrt(3)") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  scale_color_manual(values = c("|r*|>3" = "red"))

# summary & confint of both models
summary(kommuner_excl_lm)
confint(kommuner_excl_lm)

summary(model_2e)
confint(model_2e)
