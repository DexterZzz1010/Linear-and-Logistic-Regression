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

kommuner |> mutate(highcars = as.numeric(Cars > 600)) -> kommuner

kommuner <- mutate(kommuner,
                   highcars_cat = factor(highcars,
                                         levels = c(0, 1),
                                         labels = c("low", "high")))

# Part of Sweden: 1 = Götaland; 2 = Svealand; 3 = Norrland
kommuner |> count(Part, highcars_cat)

kommuner %>%
  count(Part, highcars_cat) %>%
  spread(key = highcars_cat, value = n, fill = 0) %>%
  mutate(high_percentage = (high / (high + low))*100) %>%
  select(Part, high_percentage) %>%
  print()
