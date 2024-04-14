#Lab2####
library(ggplot2)
library(dplyr)
load("Data/Pb_all.rda")
summary(Pb_all)

####  a  ####
# Plot Pb
ggplot(Pb_all, aes(x = year, y = Pb))+ facet_wrap(~ region) + geom_point()

####  b  ####
ggplot(Pb_all, aes(x = year, y = log(Pb)))+ facet_wrap(~ region) + geom_point()

####  c  ####
Pb_log_region<- lm(log(Pb) ~ I(year - 1975)+ region, data = Pb_all)
summary(Pb_log_region)
confint(Pb_log_region)


####  d  ####
Pb_all <- mutate(Pb_all, region = relevel(region, "VastraGotaland"))
Pb_log_selected<- lm(log(Pb) ~ I(year - 1975)+ region, data = Pb_all)
summary(Pb_log_selected)
confint(Pb_log_selected)

new_data <- data.frame(year = 1975)
predicted_values <- predict(Pb_log_selected, newdata = new_data)
print(predicted_values)

####  e  ####
# Pb_selected_Orebro <- mutate(Pb_all, region = relevel(region, "Orebro"))
# Pb_log_selected_Orebro<- lm(log(Pb) ~ I(year - 1975), data = Pb_selected_Orebro)
Pb_all <- mutate(Pb_all, region = relevel(region, "Orebro"))
Pb_log_selected_Orebro<- lm(log(Pb) ~ I(year - 1975)+ region, data = Pb_all)
summary(Pb_log_selected_Orebro)
confint(Pb_log_selected_Orebro)

new_data <- data.frame(year = 1975,region= "Orebro")
predicted_values_Orebro <- predict(Pb_log_selected_Orebro, newdata = new_data)
print(predicted_values_Orebro) # 3.642157 



