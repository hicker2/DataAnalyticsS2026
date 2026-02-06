library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("~/GitHub/DataAnalyticsS2026/Labs/Lab 2/NY-House-Dataset.csv")

dataset <- NY_House_Dataset
dataset <- dataset[dataset$PRICE<195000000,]
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]
dataset <- dataset[dataset$BEDS<40,]
dataset <- dataset[dataset$BATH!=0,]

## Propertysqft model
lmod_psqft <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
summary(lmod_psqft)
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")
plot(fitted(lmod_psqft), resid(lmod_psqft))
abline(h=0, col="red")


## Beds model
lmod_beds <- lm(log10(PRICE)~log10(BEDS), data = dataset)
summary(lmod_beds)
ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")
plot(fitted(lmod_beds), resid(lmod_beds))
abline(h=0, col="red")


## Bath model
lmod_bath <- lm(log10(PRICE)~log10(BATH), data = dataset)
summary(lmod_bath)
ggplot(dataset, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")
plot(fitted(lmod_bath), resid(lmod_bath))
abline(h=0, col="red")

