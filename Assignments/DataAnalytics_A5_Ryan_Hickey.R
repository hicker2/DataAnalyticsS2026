library(readr)
library(EnvStats)
library(nortest)
library(class)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(corrplot)
library(car)
library(caret)
library(GGally)

# set working directory
setwd("~\\GitHub\\DataAnalyticsS2026\\Assignments")

# read data
data <- read_csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

# Borough 3 chosen
borough <- data[data$BOROUGH == "3",]

##################################################
####            Part 1 A (EDA)                ####
##################################################

# Stupid data says some of these are strings not numbers
borough$`SALE PRICE`        <- as.numeric(borough$`SALE PRICE`)
borough$`GROSS SQUARE FEET` <- as.numeric(borough$`GROSS SQUARE FEET`)
borough$`LAND SQUARE FEET`  <- as.numeric(borough$`LAND SQUARE FEET`)
borough$`TOTAL UNITS`       <- as.numeric(borough$`TOTAL UNITS`)
borough$`YEAR BUILT`        <- as.numeric(borough$`YEAR BUILT`)

# Get rid of those stupid random values that don't matter
borough_clean <- borough[
  !is.na(borough$`SALE PRICE`)        & borough$`SALE PRICE`        > 1000 &
  !is.na(borough$`GROSS SQUARE FEET`) & borough$`GROSS SQUARE FEET` > 0    &
  !is.na(borough$`LAND SQUARE FEET`)  & borough$`LAND SQUARE FEET`  > 0    &
  !is.na(borough$`TOTAL UNITS`)       & borough$`TOTAL UNITS`       > 0    &
  !is.na(borough$`YEAR BUILT`)        & borough$`YEAR BUILT`        > 1800,
]

df <- borough_clean
names(df)[names(df) == "SALE PRICE"]        <- "SALE_PRICE"
names(df)[names(df) == "GROSS SQUARE FEET"] <- "GROSS_SQF"
names(df)[names(df) == "LAND SQUARE FEET"]  <- "LAND_SQF"
names(df)[names(df) == "TOTAL UNITS"]       <- "TOTAL_UNITS"
names(df)[names(df) == "YEAR BUILT"]        <- "YEAR_BUILT"

ggplot(df, aes(x = SALE_PRICE)) +
  geom_histogram(bins = 60, fill = "#2c7bb6", colour = "white") +
  scale_x_log10(labels = scales::dollar_format()) +
  labs(title = "Distribution of Sale Price",
       x = "Sale Price", y = "Count")

ggplot(df, aes(x = GROSS_SQF)) +
  geom_histogram(bins = 60, fill = "#d7191c", colour = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Distribution of Gross Square Feet",
       x = "Gross Sq Ft", y = "Count")

ggplot(df, aes(x = LAND_SQF)) +
  geom_histogram(bins = 60, fill = "#1a9641", colour = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Distribution of Land Square Feet",
       x = "Land Sq Ft", y = "Count")

ggplot(df[df$TOTAL_UNITS <= 50, ], aes(x = TOTAL_UNITS)) +
  geom_histogram(binwidth = 1, fill = "#756bb1", colour = "white") +
  labs(title = "Distribution of Total Units",
       x = "Total Units", y = "Count")

ggplot(df, aes(x = YEAR_BUILT)) +
  geom_histogram(bins = 50, fill = "#fd8d3c", colour = "white") +
  labs(title = "Year Built Distribution",
       x = "Year Built", y = "Count")


# Use IQR method to identify outliers
Q1 <- quantile(df$SALE_PRICE, 0.25)
Q3 <- quantile(df$SALE_PRICE, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 3 * IQR_val
upper_bound <- Q3 + 3 * IQR_val
df$price_outlier <- df$SALE_PRICE < lower_bound | df$SALE_PRICE > upper_bound

ggplot(df, aes(x = "", y = SALE_PRICE, colour = price_outlier)) +
  geom_jitter(width = 0.25, alpha = 0.3, size = 0.8) +
  scale_y_log10(labels = scales::dollar_format()) +
  scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"),
                      labels = c("Normal", "Outlier (3xIQR)"),
                      name   = "") +
  labs(title = "Sale Price Outliers",
       x = NULL, y = "Sale Price")

ggplot(df, aes(x = GROSS_SQF, y = SALE_PRICE, colour = price_outlier)) +
  geom_point(alpha = 0.35, size = 0.9) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::dollar_format()) +
  scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"),
                      labels = c("Normal", "Price Outlier"), name = "") +
  labs(title = "Gross Sq Ft vs Sale Price",
       x = "Gross Square Feet", y = "Sale Price")

ggplot(df, aes(x = LAND_SQF, y = SALE_PRICE, colour = price_outlier)) +
  geom_point(alpha = 0.35, size = 0.9) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::dollar_format()) +
  scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"),
                      labels = c("Normal", "Price Outlier"), name = "") +
  labs(title = "Land Sq Ft vs Sale Price",
       x = "Land Square Feet", y = "Sale Price")

ggplot(df, aes(x = TOTAL_UNITS, y = SALE_PRICE, colour = price_outlier)) +
  geom_point(alpha = 0.35, size = 0.9) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::dollar_format()) +
  scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"),
                      labels = c("Normal", "Price Outlier"), name = "") +
  labs(title = "Land Sq Ft vs Sale Price",
       x = "Total Units", y = "Sale Price")

ggplot(df, aes(x = YEAR_BUILT, y = SALE_PRICE, colour = price_outlier)) +
  geom_point(alpha = 0.35, size = 0.9) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::dollar_format()) +
  scale_colour_manual(values = c("FALSE" = "#2c7bb6", "TRUE" = "#d7191c"),
                      labels = c("Normal", "Price Outlier"), name = "") +
  labs(title = "Land Sq Ft vs Sale Price",
       x = "Year Built", y = "Sale Price")

##################################################
####           Part 1 B (Regression)          ####
##################################################

df_model <- df[!df$price_outlier, ]
df_model$log_price <- log(df_model$SALE_PRICE)
df_model$log_gsf   <- log(df_model$GROSS_SQF)
df_model$log_lsf   <- log(df_model$LAND_SQF)
df_model$age       <- 2024 - df_model$YEAR_BUILT

set.seed(69420)
train_idx <- createDataPartition(df_model$log_price, p = 0.70, list = FALSE)
train <- df_model[ train_idx, ]
test  <- df_model[-train_idx, ]

eval_model <- function(model, test_df, label) {
  preds   <- predict(model, newdata = test_df)
  actuals <- test_df$log_price
  rmse    <- sqrt(mean((actuals - preds)^2))
  mae     <- mean(abs(actuals - preds))
  r2      <- 1 - sum((actuals - preds)^2) / sum((actuals - mean(actuals))^2)
  sprintf("%-50s  RMSE=%.4f  MAE=%.4f  R2=%.4f\n", label, rmse, mae, r2)
  data.frame(Model = label, RMSE = rmse, MAE = mae, R2 = r2)
}

m1 <- lm(log_price ~ log_gsf, data = train)
m2 <- lm(log_price ~ log_gsf + log_lsf, data = train)
m3 <- lm(log_price ~ log_gsf + log_lsf + TOTAL_UNITS + age, data = train)
results <- rbind(
  eval_model(m1, test, "Model 1: log_gsf"),
  eval_model(m2, test, "Model 2: +log_lsf"),
  eval_model(m3, test, "Model 3: Full")
)
results

# The best model seems to be the first model, just using the gross square feet.
# It has the highest R2 value, and the second best Mean Absolute Error.
# As for data cleaning, after getting the borough the next step was removing all the na values.
# I also removed the erroneous values such as sq feet than 0 etc.
# Then I converted all the values to be numeric numbers for easier modeling later.
# Then I used the IQR method to identify outliers and plotted graphs of the outliers
# vs the data attributes to be used in the modeling.
# After removing the outliers the modeling occurred.
