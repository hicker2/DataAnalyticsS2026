library("ggplot2")
library("readr")



## read dataset
NY_House_Dataset <- read_csv("~/Courses/Data Analytics/Spring26/datasets/NY-House-Dataset.csv")

dataset <- NY_House_Dataset


ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()


## filter data
dataset <- dataset[dataset$PRICE<195000000,]

## column names
names(dataset)

## fit linear model
lmod0 <- lm(PRICE~PROPERTYSQFT, data = dataset)

## print model output
summary(lmod0)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod0)

## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

lmod1 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod1)

## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod1)

## better scatter plot of 2 variables with best fit line

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


## filter data

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]


lmod2 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod2)

## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod2)

## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")



### THE END ###

