library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

set.seed(69420)

setwd("~/GitHub/DataAnalyticsS2026/Labs/Lab 5")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash",
                 "Magnesium","Total phenols","Flavanoids",
                 "Nonflavanoid Phenols","Proanthocyanins",
                 "Color Intensity","Hue",
                 "Od280/od315 of diluted wines","Proline")

## FIX: remove spaces from column names
names(wine) <- make.names(names(wine))

## convert Type to factor
wine$Type <- as.factor(wine$Type)

## split train/test
N <- nrow(wine)
train.indexes <- sample(N, 0.7 * N)

train <- wine[train.indexes, ]
test  <- wine[-train.indexes, ]

##################################################
#### LINEAR SVM ####
##################################################

svm.mod0 <- tune.svm(
  Type ~ Hue + Flavanoids + Ash,
  data = train,
  kernel = "linear",
  cost = c(0.1, 1, 10, 100)
)

print(svm.mod0)

## predict
test.pred <- predict(svm.mod0$best.model, test)

## confusion matrix
cm <- as.matrix(table(Actual = test$Type, Predicted = test.pred))
print(cm)

## metrics
n <- sum(cm)
diagv <- diag(cm)
rowsums <- apply(cm, 1, sum)
colsums <- apply(cm, 2, sum)

accuracy <- sum(diagv) / n
recall <- diagv / rowsums
precision <- diagv / colsums
f1 <- 2 * precision * recall / (precision + recall)

svm.mod0.res <- data.frame(model="linear", precision, recall, f1)
print(accuracy)
print(svm.mod0.res)

results <- svm.mod0.res

##################################################
#### RADIAL SVM ####
##################################################

gamma.range <- seq(0.1, 10, 0.1)

svm.mod1 <- tune.svm(
  Type ~ Hue + Flavanoids + Ash,
  data = train,
  kernel = "radial",
  gamma = gamma.range,
  cost = c(0.1, 1, 10)
)

print(svm.mod1)

## predict
test.pred <- predict(svm.mod1$best.model, test)

## confusion matrix
cm <- as.matrix(table(Actual = test$Type, Predicted = test.pred))
print(cm)

## metrics
n <- sum(cm)
diagv <- diag(cm)
rowsums <- apply(cm, 1, sum)
colsums <- apply(cm, 2, sum)

accuracy <- sum(diagv) / n
recall <- diagv / rowsums
precision <- diagv / colsums
f1 <- 2 * precision * recall / (precision + recall)

svm.mod1.res <- data.frame(model="radial", precision, recall, f1)
print(accuracy)
print(svm.mod1.res)

results <- rbind(results, svm.mod1.res)

##################################################
#### KNN ####
##################################################

train.x <- train[, c("Hue", "Flavanoids", "Ash")]
test.x  <- test[, c("Hue", "Flavanoids", "Ash")]

train.y <- train$Type
test.y  <- test$Type

train.x <- scale(train.x)
test.x  <- scale(test.x,
                 center = attr(train.x, "scaled:center"),
                 scale  = attr(train.x, "scaled:scale"))
k <- 5

## train + predict
test.pred <- knn(train = train.x,
                 test = test.x,
                 cl = train.y,
                 k = k)

## confusion matrix
cm <- as.matrix(table(Actual = test.y, Predicted = test.pred))
print(cm)

## metrics
n <- sum(cm)
diagv <- diag(cm)
rowsums <- apply(cm, 1, sum)
colsums <- apply(cm, 2, sum)

accuracy <- sum(diagv) / n
recall <- diagv / rowsums
precision <- diagv / colsums
f1 <- 2 * precision * recall / (precision + recall)

knn.res <- data.frame(model = "knn", precision, recall, f1)
print(accuracy)
print(knn.res)

results <- rbind(results, knn.res)

##################################################
#### FINAL RESULTS ####
##################################################

print(results)
