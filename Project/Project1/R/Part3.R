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

## leverage #####
kommuner_pred <- mutate(kommuner,
                    yhat = predict(model_2e),
                    r = rstudent(model_2e),
                    v = hatvalues(model_2e),
                    D = cooks.distance(model_2e))

pplus1 <- length(model_2e$coefficients)
n <- nobs(model_2e)

# Get top leverage
top_leverage <- kommuner_pred %>%
  arrange(desc(v)) %>%
  slice(1:6)

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
