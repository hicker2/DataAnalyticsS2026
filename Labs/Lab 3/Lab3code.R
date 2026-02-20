################################################
####                Lab 3                   ####
################################################

library("caret")
library(GGally)
library(psych)
library(class)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)


# ---- SETUP ---- #
## read data
abalone <- read.csv("~/GitHub/DataAnalyticsS2026/Labs/Lab 3/abalone/abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)

## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

## split train/test
train.indexes <- sample(4177,0.7*4177)

train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]
# -------------------------------------

# ----- Model 1 ------
features_A <- c('whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight')

knn_A <- knn(train = train[, features_A],
             test  = test[, features_A],
             cl    = train$age.group,
             k     = 3)

# ----- Model 2 ------
features_B <- c("length", 'diameter', 'height')

knn_B <- knn(train = train[, features_B],
             test  = test[, features_B],
             cl    = train$age.group,
             k     = 3)

# ----- Contingency Tables --------
ct_A <- table(Predicted = knn_A, Actual = test$age.group)
print(ct_A)
acc_A <- sum(diag(ct_A)) / sum(ct_A)
sprintf("Model A Accuracy: %.4f", acc_A)

ct_B <- table(Predicted = knn_B, Actual = test$age.group)
print(ct_B)
acc_B <- sum(diag(ct_B)) / sum(ct_B)
sprintf("Model B Accuracy: %.4f", acc_B)

# ----- Best features -------------
if (acc_A >= acc_B) {
  best_features <- features_A
} else {
  best_features <- features_B
}

# ----- Finding optimal K neighbors ------
k_values  <- 1:30
k_accuracy <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  preds <- knn(train = train[, best_features],
               test  = test[, best_features],
               cl    = train$age.group,
               k     = k_values[i])
  k_accuracy[i] <- sum(preds == test$age.group) / length(test$age.group)
}

optimal_k   <- k_values[which.max(k_accuracy)]
optimal_acc <- max(k_accuracy)

sprintf("Optimal k = %d  |  Accuracy = %.4f", optimal_k, optimal_acc)


# ----- K means setup -----

data_A <- scale(abalone.sub[, features_A])
data_B <- scale(abalone.sub[, features_B])
set.seed(69420)


compute_sil <- function(data, k, method = "kmeans") {
  if (method == "kmeans") {
    model  <- kmeans(data, centers = k, nstart = 25)
    labels <- model$cluster
  } else {
    model  <- pam(data, k = k)
    labels <- model$clustering
  }
  sil <- silhouette(labels, dist(data))
  mean(sil[, 3])
}

k_range <- 2:10

# ----- Feature Set A ------
sil_kmeans_A <- sapply(k_range, compute_sil, data = data_A, method = "kmeans")
sil_pam_A    <- sapply(k_range, compute_sil, data = data_A, method = "pam")

optimal_k_kmeans_A <- k_range[which.max(sil_kmeans_A)]
optimal_k_pam_A    <- k_range[which.max(sil_pam_A)]

sprintf("Optimal k (k-means): %d  | Avg Silhouette: %.4f\n", optimal_k_kmeans_A, max(sil_kmeans_A))
sprintf("Optimal k (PAM):     %d  | Avg Silhouette: %.4f\n", optimal_k_pam_A,    max(sil_pam_A))

# ----- Feature Set B ------
sil_kmeans_B <- sapply(k_range, compute_sil, data = data_B, method = "kmeans")
sil_pam_B    <- sapply(k_range, compute_sil, data = data_B, method = "pam")

optimal_k_kmeans_B <- k_range[which.max(sil_kmeans_B)]
optimal_k_pam_B    <- k_range[which.max(sil_pam_B)]

sprintf("Optimal k (k-means): %d  | Avg Silhouette: %.4f\n", optimal_k_kmeans_B, max(sil_kmeans_B))
sprintf("Optimal k (PAM):     %d  | Avg Silhouette: %.4f\n", optimal_k_pam_B,    max(sil_pam_B))


# ----- Final Models -----
final_kmeans_A <- kmeans(data_A, centers = optimal_k_kmeans_A, nstart = 25)
final_pam_A    <- pam(data_A,   k = optimal_k_pam_A)

final_kmeans_B <- kmeans(data_B, centers = optimal_k_kmeans_B, nstart = 25)
final_pam_B    <- pam(data_B,   k = optimal_k_pam_B)


# ----- Plots -----
sil_plot_kmeans_A <- silhouette(final_kmeans_A$cluster, dist(data_A))
fviz_silhouette(sil_plot_kmeans_A)
sil_plot_pam_A <- silhouette(final_pam_A$clustering, dist(data_A))
fviz_silhouette(final_pam_A)
sil_plot_kmeans_B <- silhouette(final_kmeans_B$cluster, dist(data_B))
fviz_silhouette(sil_plot_kmeans_B)
sil_plot_pam_B <- silhouette(final_pam_B$clustering, dist(data_B))
fviz_silhouette(final_pam_B)


