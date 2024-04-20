#Lab3####
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
load("Data/Pb_all.rda")
summary(Pb_all)
# Pb_all <- mutate(Pb_all, region = relevel(region, "[mzq]"))
##### a #####
model_log_all <- lm(log(Pb) ~ I(year - 1975)*region , data = Pb_all)

model_log_all_sum <- summary(model_log_all)
model_log_all_pred <- mutate(Pb_all,
                             yhat = predict(model_log_all),
                             r = rstudent(model_log_all),
                             v = hatvalues(model_log_all),
                             D = cooks.distance(model_log_all))
summary(model_log_all)

Pb_log_sum_all <- summary(model_log_all)

Pb_log_sum_all$coefficients
Pb_log_sum_all$coefficients["I(year - 1975)",]

# Extract and report the F-statistic from the model summary
f_statistic <- Pb_log_sum_all$fstatistic
cat("F-statistic: ", f_statistic[1], "\n")
cat("df1: ", f_statistic[2], ", df2: ", f_statistic[3], "\n")
cat("p-value: ", pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE), "\n")

# P value
Pb_log_sum_all$coefficients["I(year - 1975)", "Pr(>|t|)"]

# t-value and t-quantile
tvalue <- Pb_log_sum_all$coefficients["I(year - 1975)", "Pr(>|t|)"]

qt(p = 0.05/2, 
   df = model_log_all$df.residual, 
   lower.tail = FALSE)

df <- model_log_all$df.residual
print(df)
abs(tvalue)

cod_pred <- mutate(Pb_all, 
                   yhat_linear = predict(model_log_all),
                   r_linear = rstudent(model_log_all),
                   yhat_loglog = predict(model_log_all),
                   r_loglog = rstudent(model_log_all))

ggplot(cod_pred, aes(x = yhat_linear, y = r_linear))+ 
  facet_wrap(~region)+
  geom_point() +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  geom_point(data = filter(cod_pred, abs(r_linear) > 3), 
             aes(color = "|r*|>3"), size = 3) +
  labs(title = "Studentized residuals vs linear predictor",
       subtitle = "Lin-lin model",
       color = "Highlight") +
  scale_color_manual(values = highlightcolors) +
  theme(legend.position = "bottom",
        text = element_text(size = 18))

##### b #####
Pb_lm <- lm(log(Pb) ~ I(year - 1975), data = Pb_all)
Pb_sum <- summary(Pb_lm)

Pb_r_lm <- lm(log(Pb) ~ I(year - 1975) + region, data = Pb_all)
Pb_r_sum <- summary(Pb_r_lm)

Pb_i_lm <- lm(log(Pb) ~ I(year - 1975) * region, data = Pb_all)
Pb_i_sum <- summary(Pb_i_lm)

collect.R2AIC <- data.frame(
  nr = seq(1, 3),
  model = c("01.time", "02.time + region", "02.time * region"),
  sigma = c(Pb_sum$sigma,
            Pb_r_sum$sigma,
            Pb_i_sum$sigma),
  R2 = c(Pb_sum$r.squared,
         Pb_r_sum$r.squared,
         Pb_i_sum$r.squared),
  R2.adj = c(Pb_sum$adj.r.squared,
             Pb_r_sum$adj.r.squared,
             Pb_i_sum$adj.r.squared))
collect.R2AIC |> arrange(desc(R2.adj))

ggplot(collect.R2AIC) +
  geom_point(aes(x = model, y = R2, color = "R2"), size = 3) + 
  geom_point(aes(x = model, y = R2.adj, color = "R2-adjusted"), size = 3) + 
  geom_line(aes(x = nr, y = R2, color = "R2"), linewidth = 1) +
  geom_line(aes(x = nr, y = R2.adj, color = "R2-adjusted"), 
            linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = 1) +
  labs(title = "Cabbage: R2 and R2-adjusted",
       color = "") +
  ylab("R2") +
  theme(text = element_text(size = 18), legend.position = "bottom")

cbind(collect.R2AIC,
      AIC = AIC(Pb_lm, Pb_r_lm, Pb_i_lm),
      BIC = BIC(Pb_lm, Pb_r_lm, Pb_i_lm)) -> collect.R2AIC
glimpse(collect.R2AIC)

collect.R2AIC |>
  rename(df = AIC.df, AIC = AIC.AIC, BIC = BIC.BIC) |>
  mutate(BIC.df = NULL) -> collect.R2AIC

collect.R2AIC |> arrange(AIC)
collect.R2AIC |> arrange(BIC)

r_squared <- Pb_i_sum$r.squared

