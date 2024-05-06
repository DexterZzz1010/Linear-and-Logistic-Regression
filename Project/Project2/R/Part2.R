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

model_full <- glm(highcars ~ log(Higheds)+Children+Seniors+log(Income)+log(GRP)+ Persperhh+Fertility+Urban+Transit+Apartments, 
               family = "binomial", 
               data = kommuner)
summary(model_full)
model_full_sum <- summary(model_full)
vif(model_full)


model_null <- glm(highcars ~ 1, family = "binomial", data = kommuner)

# AIC stepwise selection
step_model_aic <- step(model_null,
                       scope = list(lower = model_null, upper = model_full),
                       direction = "both",
                       trace = TRUE,  # trace=TRUE detail information for every steps
                       k = 2)  # k=2 means using AIC

summary(step_model_aic)


# BIC stepwise selection
step_model_bic <- step(model_null,
                       scope = list(lower = model_null, upper = model_full),
                       direction = "both",
                       trace = TRUE,
                       k =  log(nobs(model_full)))  # BIC
summary(step_model_bic)



