library(readr)
library(EnvStats)
library(nortest)
library(class)

# set working directory
setwd("~\\GitHub\\DataAnalyticsS2026\\Assignments")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# -- Choose Variable --------------------
var_name <- "TCG.new"
x_all <- epi.data[[var_name]]
x_all <- na.omit(x_all)
# ---------------------------------------

# -- Question 1 -------------------------
# -- Part 1.1 ---------------------------
hist(x_all, prob = TRUE, xlab = var_name, col = "lightgray", border = "white")
lines(density(x_all, bw = "SJ"), col = "blue", lwd=2)
x1 <- seq(0, 100, 1)
d1 <- dnorm(x1, mean=mean(x_all), sd=sd(x_all), log=FALSE)
lines(x1, d1, col="red", lwd=2)
# ---------------------------------------
# -- Part 1.2 ---------------------------
boxplot(epi.data[[var_name]] ~ epi.data$region,
        xlab = "Region",
        ylab = var_name,
        col = "lightblue",
        las = 2,
        cex.axis=0.7)
# ---------------------------------------
# ---------------------------------------

# -- Question 2 -------------------------
region1 <- "Global West"
region2 <- "Asia-Pacific"

sub1 <- subset(epi.data, region == region1)[[var_name]]
sub2 <- subset(epi.data, region == region2)[[var_name]]

sub1 <- na.omit(sub1)
sub2 <- na.omit(sub2)

# -- Part 2.1 ---------------------------
par(mfrow = c(1, 2))

hist(sub1,
     prob = TRUE,
     main = paste("Histogram -", region1),
     xlab = var_name,
     col = "lightgreen",
     border = "white")
lines(density(sub1, bw = "SJ"), col = "darkgreen", lwd = 2)

hist(sub2,
     prob = TRUE,
     main = paste("Histogram -", region2),
     xlab = var_name,
     col = "lightpink",
     border = "white")
lines(density(sub2, bw = "SJ"), col = "red", lwd = 2)

par(mfrow = c(1, 1))
# ---------------------------------------

# -- Part 2.2 ---------------------------
qqplot(sub1, sub2,
       main = paste("QQ Plot:", region1, "vs", region2),
       xlab = region1,
       ylab = region2,
       col = "purple"
       )

abline(0, 1, col = "black", lwd = 2)
# ---------------------------------------
# ---------------------------------------

# -- Question 3 -------------------------
pop_name <- "population"
gdp_name <- "gdp"

q3_data <- na.omit(epi.data[, c(var_name, pop_name, gdp_name)])

q3_data$pop_data <- log10(q3_data$population)
q3_data$gdp_data <- log10(q3_data$gdp)
q3_data$tcg_data <- q3_data$TCG.new

plot_with_fit <- function(x, y, xlab, ylab, col, title_suffix = "") {
  plot(x, y, main = paste(ylab, "vs", xlab, title_suffix), xlab = xlab, ylab = ylab,
       col = col, pch = 16, cex = 0.7)
  abline(lm(y ~ x), col = "red", lwd = 2)
}

fit_model <- function(formula, data, label) {
  m <- lm(formula, data = data)
  cat("\n========== ", label, " ==========\n", sep = "")
  print(summary(m))
  par(mfrow = c(2, 2))
  plot(m, main = label)
  par(mfrow = c(1, 1))
  return(m)
}

analyse_region <- function(data, region_label) {
  cat("\n\n########## Region:", region_label, "##########\n")
  
  # Prepare data
  d <- na.omit(data[, c(var_name, pop_name, gdp_name)])
  colnames(d) <- c("TCG", "population", "gdp")
  d$log_pop <- log10(d$population)
  d$log_gdp <- log10(d$gdp)
  
  # Plots
  par(mfrow = c(1, 2))
  plot_with_fit(d$log_pop, d$TCG, "log10(Population)", var_name, "steelblue", title_suffix = paste("-", region_label))
  plot_with_fit(d$log_gdp, d$TCG, "log10(GDP)", var_name, "darkorange", title_suffix = paste("-", region_label))
  par(mfrow = c(1, 1))
  
  # Models + diagnostics
  m1 <- fit_model(TCG ~ log_pop, d, paste("Model 1: TCG ~ log10(Pop) |", region_label))
  m2 <- fit_model(TCG ~ log_gdp, d, paste("Model 2: TCG ~ log10(GDP) |", region_label))
  
  # Comparison
  cat("\n---------- Model Comparison:", region_label, "----------\n")
  cat(sprintf("Model 1 (log_pop only) - Adj. R²: %.4f  |  AIC: %.2f\n",
              summary(m1)$adj.r.squared, AIC(m1)))
  cat(sprintf("Model 2 (log_gdp only) - Adj. R²: %.4f  |  AIC: %.2f\n",
              summary(m2)$adj.r.squared, AIC(m2)))
  
  invisible(list(model1 = m1, model2 = m2))
}

# -- Part 3.1 and Part 3.2 ---------------
results <- list()
for (r in c(region1, region2)) {
  results[[r]] <- analyse_region(subset(epi.data, region == r), r)
}
# ---------------------------------------

# -- Part 3.3 ---------------------------

# In both regions I believe the linear model using gdp only is the best fit. 
# This is because it has the higher adjusted R² value, although still a horribly performing model.

# ---------------------------------------
# ---------------------------------------

# -- Question 4 -------------------------
region3 <- "Eastern Europe"

q4_vars  <- c("region", pop_name, gdp_name, var_name, "FCL.new")
q4_data  <- subset(epi.data, region %in% c(region1, region2, region3))
q4_data  <- na.omit(q4_data[, q4_vars])

q4_data$log_pop <- log10(q4_data$population)
q4_data$log_gdp <- log10(q4_data$gdp)

normalise <- function(x) (x - min(x)) / (max(x) - min(x))

run_knn <- function(train_x, test_x, train_y, test_y, k, label) {
  pred <- knn(train_x, test_x, train_y, k = k)
  cm   <- table(Predicted = pred, Actual = test_y)
  acc  <- sum(diag(cm)) / sum(cm)
  cat(sprintf("\n--- %s  (k = %d) ---\n", label, k))
  print(cm)
  cat(sprintf("Accuracy: %.4f\n", acc))
  return(acc)
}

set.seed(69420)
train_idx <- sample(nrow(q4_data), size = 0.7 * nrow(q4_data))
train     <- q4_data[ train_idx, ]
test      <- q4_data[-train_idx, ]

# -- Part 4.1 ---------------------------
featuresA <- c("log_pop", "log_gdp", "TCG.new")

train_xA <- as.data.frame(lapply(train[, featuresA], normalise))
test_xA  <- as.data.frame(mapply(function(col, tr_col)
  (col - min(tr_col)) / (max(tr_col) - min(tr_col)),
  test[, featuresA], train[, featuresA]))

cat("\n========== kNN — log(Pop) + log(GDP) + TCG.new ==========\n")
accA <- sapply(c(3, 5, 7, 9, 11, 20), function(k)
  run_knn(train_xA, test_xA, train$region, test$region, k,
          "log(Pop) + log(GDP) + TCG.new"))

best_k <- c(3, 5, 7, 9, 11, 20)[which.max(accA)]
cat(sprintf("\nBest k for Model A: %d  (Accuracy: %.4f)\n", best_k, max(accA)))
# ---------------------------------------

# -- Part 4.2 ---------------------------
featuresB <- c("log_pop", "log_gdp", "FCL.new")

train_xB <- as.data.frame(lapply(train[, featuresB], normalise))
test_xB  <- as.data.frame(mapply(function(col, tr_col)
  (col - min(tr_col)) / (max(tr_col) - min(tr_col)),
  test[, featuresB], train[, featuresB]))

cat(sprintf("\n========== kNN — log(Pop) + log(GDP) + FCL.new  (k = %d) ==========\n", best_k))
run_knn(train_xB, test_xB, train$region, test$region, best_k,
        "log(Pop) + log(GDP) + FCL.new")
# ---------------------------------------

# -- Part 4.3 ---------------------------
# The better performing model is population, gdp, and TCG.
# This is most likely because FCL has a lower accuracy rate, although not by much.
# ---------------------------------------

