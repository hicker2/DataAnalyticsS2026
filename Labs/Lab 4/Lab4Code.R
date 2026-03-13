library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)


setwd("~/GitHub/DataAnalyticsS2026/Labs/Lab 4")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###

X <- wine[,-1]
Y <- wine$Type

###


### PC plots ###
Xmat <- as.matrix(X)

Xc <- scale(Xmat, center = T, scale = T)

principal_components <- princomp(Xc)
summary(principal_components)
principal_components$loadings

autoplot(principal_components, data = wine, colour = "Type",
         loadings = TRUE, loadings.colour = "blue",
         loadings.label = TRUE, loadings.label.size = 3, scale = 0)

Z <- principal_components$scores
ggplot(as.data.frame(Z), aes(x = Z[,1], y = Z[,2], colour = Y)) + geom_point() +
  labs(title = "Wine Dataset Projected onto PC1 and PC2", x = "PC1", y = "PC2")

### Variables contributing to 1st PC ###

loadings_pc1 <- principal_components$loadings[, 1]
loadings_df <- data.frame(
  Variable   = names(loadings_pc1),
  Loading    = loadings_pc1,
  AbsLoading = abs(loadings_pc1)
)
loadings_df <- loadings_df[order(loadings_df$AbsLoading, decreasing = TRUE), ]
print(loadings_df)

### KNN using subset ###

selected_vars <- c("Color Intensity", "Hue", "Od280/od315 of diluted wines", "Proline")
X_sel <- scale(wine[, selected_vars])
set.seed(69420)
n <- nrow(X_sel)
train_idx <- sample(1:n, size = floor(0.8 * n))
X_train <- X_sel[train_idx, ]
X_test  <- X_sel[-train_idx, ]
Y_train <- Y[train_idx]
Y_test  <- Y[-train_idx]
k_val <- round(sqrt(length(train_idx)))
knn_orig_pred <- knn(train = X_train, test = X_test, cl = Y_train, k = k_val)

cm_orig <- table(Predicted = knn_orig_pred, Actual = Y_test)
print(cm_orig)

### KNN on PC ###

X_pca_train <- Z[train_idx, 1:2]
X_pca_test  <- Z[-train_idx, 1:2]
knn_pc_pred <- knn(train = X_pca_train, test = X_pca_test, cl = Y_train, k = k_val)

cm_pc <- table(Predicted = knn_pc_pred, Actual = Y_test)
print(cm_pc)

### Comparing Models ###

cm_orig_caret <- confusionMatrix(knn_orig_pred, Y_test)
cm_orig_caret$byClass[, "F1"]

cm_pc_caret <- confusionMatrix(knn_pc_pred, Y_test)
cm_pc_caret$byClass[, "F1"]

mean(cm_orig_caret$byClass[, "F1"])
mean(cm_pc_caret$byClass[, "F1"])

# Based on the F1 scores, and the contingency tables, the PC model does better 
# Both models perform well, but the PC model has a perfect score

