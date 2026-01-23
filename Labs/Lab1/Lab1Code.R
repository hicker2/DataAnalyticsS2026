library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("C:\\Users\\Ryan\\Documents\\GitHub\\DataAnalyticsS2026\\Labs\\Lab1")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

TCG.new <- epi.data$TCG.new
TCG.new <- na.omit(TCG.new)
BER.new <- epi.data$BER.new
BER.new <- na.omit(BER.new)

summary(TCG.new)
summary(BER.new)

# Boxplots -------------------------------------------------
boxplot(TCG.new, names="TCG.new")
boxplot(BER.new, names = "BER.new")
# ----------------------------------------------------------


# Histograms -----------------------------------------------
x <- seq(0, 90, 5)
hist(TCG.new, x, prob=TRUE)
lines(density(TCG.new, bw="SJ"))
x1 <- seq(0, 90, 1)
d1 <- dnorm(x1,mean=40.46, sd=sd(TCG.new),log=FALSE)
lines(x1,d1)
x <- seq(0, 100, 5)
hist(BER.new, x, prob=TRUE)
lines(density(BER.new, bw="SJ"))
x1 <- seq(0, 100, 1)
d1 <- dnorm(x1,mean=49, sd=sd(BER.new),log=FALSE)
lines(x1,d1)
# ----------------------------------------------------------

# ECDP -----------------------------------------------------
plot(ecdf(TCG.new), do.points=FALSE, verticals=TRUE)
plot(ecdf(BER.new), do.points=FALSE, verticals=TRUE) 
# ----------------------------------------------------------

# QQ -------------------------------------------------------
qqnorm(TCG.new); qqline(TCG.new)
qqnorm(BER.new); qqline(BER.new)
qqplot(TCG.new, BER.new, xlab = "Q-Q plot for TCG & BER")
# ----------------------------------------------------------