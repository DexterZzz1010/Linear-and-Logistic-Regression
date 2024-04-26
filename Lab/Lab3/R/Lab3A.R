#Lab3####
library(ggplot2)
library(dplyr)
load("Data/Pb_all.rda")
# Pb_all <- mutate(Pb_all, region = relevel(region, "[mzq]"))
summary(Pb_all)

#### a #####
Pb_log_lm<- lm(log(Pb) ~ I(year - 1975), data = Pb_all)
Pb_lm<- lm(Pb ~ I(year - 1975), data = Pb_all)

# for Pb_lm

Pb_pred <- mutate(Pb_all,
                    yhat = predict(Pb_lm),
                    r = rstudent(Pb_lm),
                    v = hatvalues(Pb_lm),
                    D = cooks.distance(Pb_lm))
highlightcolors <- c("|r*|>3" = "red",
                     "length>200" = "magenta", 
                     "all data" = "orange",
                     "excl.length>200" = "blue")
# with 1/n and 2(p+1)/n horizontal lines:
# p+1 = 
pplus1 <- length(Pb_lm$coefficients)
n <- nobs(Pb_lm)

ggplot(Pb_pred, aes(x = year-1975  , y = v)) +
  geom_point(size = 2) +
  geom_point(data = filter(Pb_pred, Pb > 70), 
             aes(color = "length>200"), size = 3) +
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Pike: leverage vs log length",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16)) +
  scale_color_manual(values = highlightcolors)

# for Pb_log_lm

Pb_log_pred <- mutate(Pb_all,
                  yhat = predict(Pb_log_lm),
                  r = rstudent(Pb_log_lm),
                  v = hatvalues(Pb_log_lm),
                  D = cooks.distance(Pb_log_lm))

# with 1/n and 2(p+1)/n horizontal lines:
# p+1 = 
pplus1_log <- length(Pb_log_lm$coefficients)
n_log <- nobs(Pb_log_lm)



ggplot(Pb_log_pred, aes(x = year-1975  , y = v)) +
  geom_point(size = 2) +
  geom_point(data = filter(Pb_log_pred, Pb > 70), 
             aes(color = "length>200"), size = 3) +
  geom_hline(yintercept = 1/n_log) +
  geom_hline(yintercept = 2*pplus1_log/n_log, color = "red") +
  labs(title = "Pike: leverage vs log length",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16)) +
  scale_color_manual(values = highlightcolors)
year_all = Pb_all$year
mean(year_all)

##### b ######
Pb_all <- mutate(Pb_all, region = relevel(region, "Norrbotten"))
Pb_log_selected<- lm(log(Pb) ~ I(year - 1975)+ region, data = Pb_all)
summary(Pb_log_selected)
Pb_selected_pred <- mutate(Pb_all,
                      yhat = predict(Pb_log_selected),
                      r = rstudent(Pb_log_selected),
                      v = hatvalues(Pb_log_selected),
                      D = cooks.distance(Pb_log_selected))

pplus1_selected <- length(Pb_log_selected$coefficients)
n_selected <- nobs(Pb_log_selected)

region <- c("Orebro" = "red",
                     "Jamtland" = "magenta", 
                     "Vasternorrland" = "orange",
                     "VastraGotaland" = "blue")

ggplot(Pb_selected_pred, aes(x = year - 1975, y = v, color = region)) +
  geom_jitter(width = 1) +
  geom_point(data = filter(Pb_selected_pred), 
             aes(color = region), size = 3) +
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Pike: leverage vs Pb",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16)) +
  scale_color_manual(values = region)

#### c ####
highlightcolors <- c("|r*|>3" = "red")

cod_pred <- mutate(Pb_all, 
                   yhat_linear = predict(Pb_log_selected),
                   r_linear = rstudent(Pb_log_selected),
                   yhat_loglog = predict(Pb_log_selected),
                   r_loglog = rstudent(Pb_log_selected))

ggplot(cod_pred, aes(x = yhat_linear, y = r_linear)) +
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

which(abs(rstudent(Pb_log_selected)) > 3)

#### d ####
ggplot(cod_pred, aes(x = yhat_linear, y = r_linear)) +
  geom_point()+ 
  facet_wrap(~region) +
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

#### e ####
ggplot(cod_pred, aes(x = yhat_linear, y  = sqrt(abs(r_linear)))) +
  geom_point()+ 
  facet_wrap(~region) +
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = 2) +
  geom_point(data = filter(cod_pred, sqrt(abs(r_linear)) > sqrt(3)), 
             aes(color = "|r*|>3"), size = 3) +
  labs(title = "Studentized residuals vs linear predictor",
       subtitle = "Lin-lin model",
       color = "Highlight") +
  scale_color_manual(values = highlightcolors) +
  theme(legend.position = "bottom",
        text = element_text(size = 18))


f1.Pb <- pplus1_selected
f2.Pb <- Pb_log_selected$df.residual
cook.limit.Pb <- qf(0.5, f1.Pb, f2.Pb)

ggplot(Pb_selected_pred, aes(yhat, D)) + 
  facet_wrap(~region)+
  geom_point(size = 3) +
  geom_point(data = filter(Pb_selected_pred, (year - 1975) > 200),
             aes(color = "length>200"), size = 3) +
  geom_hline(yintercept = 4/n, linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Pike: Cook's D",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18)) +
  scale_color_manual(values = highlightcolors)

#### g ####
head(dfbetas(Pb_log_selected))
highlightshapes <- c("Cook's D>0.1" = 24)

Pb_excl_pred <- mutate(
  Pb_selected_pred,
  df0 = dfbetas(Pb_log_selected)[, "(Intercept)"],
  df1 = dfbetas(Pb_log_selected)[, "I(year - 1975)"],
  fit = predict(Pb_log_selected),
  r = rstudent(Pb_log_selected),
  D = cooks.distance(Pb_log_selected))

ggplot(Pb_excl_pred, aes(x = year, y = df1)) +
  facet_wrap(~region)+
  geom_point(size = 2) +
  geom_point(data = filter(Pb_excl_pred, abs(r) > 3),
             aes(color = "|r*|>3"), size = 3) +
  geom_point(data = filter(Pb_excl_pred, D > 0.1),
             aes(shape = "Cook's D>0.1"),
             size = 3) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.pike)*c(-1, 1),
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
