library(readr)
library(EnvStats)
library(nortest)
library(class)
library(ggplot2)
library(randomForest)
library(e1071)

# Below is the extra information detailing how the numbers of the dataset map
# to real outputs never used but useful to see for eda
label_mappings <- list(
  Age                              = c("1" = "18-21",      "2" = "22-25",               "3" = "26+"),
  Sex                              = c("1" = "Female",     "2" = "Male"),
  high_school_type                 = c("1" = "Private",    "2" = "State",               "3" = "Other"),
  Scholarship_type                 = c("1" = "None",       "2" = "25%",                 "3" = "50%",            "4" = "75%",         "5" = "Full"),
  Additional_work                  = c("1" = "Yes",        "2" = "No"),
  Regular_artistic_or_sports_activity = c("1" = "Yes",    "2" = "No"),
  Has_partner                      = c("1" = "Yes",        "2" = "No"),
  Total_salary                     = c("1" = "$135-200",   "2" = "$201-270",            "3" = "$271-340",       "4" = "$341-410",    "5" = "$410+"),
  Transportation_to_university     = c("1" = "Bus",        "2" = "Car/Taxi",            "3" = "Bicycle",        "4" = "Other"),
  Campus_Accommodation_type        = c("1" = "Rental",     "2" = "Dormitory",           "3" = "With Family",    "4" = "Other"),
  Mother_education                 = c("1" = "Primary",    "2" = "Secondary",           "3" = "High School",    "4" = "University",  "5" = "MSc",  "6" = "PhD"),
  Father_education                 = c("1" = "Primary",    "2" = "Secondary",           "3" = "High School",    "4" = "University",  "5" = "MSc",  "6" = "PhD"),
  Sibling_count                    = c("1" = "1",          "2" = "2",                   "3" = "3",              "4" = "4",           "5" = "5+"),
  Parental_status                  = c("1" = "Married",    "2" = "Divorced",            "3" = "Deceased"),
  Mother_occupation                = c("1" = "Retired",    "2" = "Housewife",           "3" = "Gov. Officer",   "4" = "Private",     "5" = "Self-Employed", "6" = "Other"),
  Father_occupation                = c("1" = "Retired",    "2" = "Gov. Officer",        "3" = "Private",        "4" = "Self-Employed", "5" = "Other"),
  Weekly_study_hours               = c("1" = "None",       "2" = "<5 hrs",              "3" = "6-10 hrs",       "4" = "11-20 hrs",   "5" = "20+ hrs"),
  Non_academic_Reading_frequency   = c("1" = "None",       "2" = "Sometimes",           "3" = "Often"),
  Academic_Reading_frequency       = c("1" = "None",       "2" = "Sometimes",           "3" = "Often"),
  Seminars_attendance              = c("1" = "Yes",        "2" = "No"),
  Projects_on_success              = c("1" = "Positive",   "2" = "Negative",            "3" = "Neutral"),
  Class_attendance                 = c("1" = "Always",     "2" = "Sometimes",           "3" = "Never"),
  Who_you_study_with               = c("1" = "Alone",      "2" = "With Friends",        "3" = "N/A"),
  When_you_study                   = c("1" = "Closest to Exam", "2" = "Regularly",      "3" = "Never"),
  Taking_notes                     = c("1" = "Never",      "2" = "Sometimes",           "3" = "Always"),
  Listening_during_class           = c("1" = "Never",      "2" = "Sometimes",           "3" = "Always"),
  Discussion_helps_learning        = c("1" = "Never",      "2" = "Sometimes",           "3" = "Always"),
  Flip_classroom                   = c("1" = "Not Useful", "2" = "Useful",              "3" = "N/A"),
  GPA_last_sem                     = c("1" = "<2.00",      "2" = "2.00-2.49",           "3" = "2.50-2.99",      "4" = "3.00-3.49",   "5" = "3.49+"),
  Expected_GPA                     = c("1" = "<2.00",      "2" = "2.00-2.49",           "3" = "2.50-2.99",      "4" = "3.00-3.49",   "5" = "3.49+"),
  Grade                            = c("0" = "Fail",       "1" = "DD",                  "2" = "DC",             "3" = "CC",          "4" = "CB",   "5" = "BB", "6" = "BA", "7" = "AA")
)

# set working directory
setwd("~\\GitHub\\DataAnalyticsS2026\\Assignments")

# read data
data <- read_csv("DATA (1).csv")

col_names <- c("Student_ID", "Age", "Sex", "high_school_type", "Scholarship_type",
               "Additional_work", "Regular_artistic_or_sports_activity", "Has_partner",
               "Total_salary", "Transportation_to_university", "Campus_Accommodation_type",
               "Mother_education", "Father_education", "Sibling_count", "Parental_status",
               "Mother_occupation", "Father_occupation", "Weekly_study_hours",
               "Non_academic_Reading_frequency", "Academic_Reading_frequency",
               "Seminars_attendance", "Projects_on_success", "Class_attendance",
               "Who_you_study_with", "When_you_study", "Taking_notes",
               "Listening_during_class", "Discussion_helps_learning", "Flip_classroom",
               "GPA_last_sem", "Expected_GPA", "Course_ID", "Grade")

colnames(data) <- col_names

###################################################
# EDA 
###################################################

plot_var <- function(name) {
    ggplot(data, aes(x = .data[[name]])) +
      geom_histogram(bins = 8, fill = "skyblue", color = "black") +
      labs(title = paste("Histogram of", name),
           x =name,
           y = "Count") +
      theme_minimal()
}

analyze_distribution <- function(name) {
  var <- data[[name]]
  
  summary(var)
  test <- shapiro.test(var)
  # print(test)
  
  if (test$p.value < 0.05) {
    cat(sprintf("Conclusion %s: Not normally distributed\n", name))
  } else {
    cat("Conclusion %s: Approximately normal\n", name)
  }
}

for (col in colnames(data)) {
  if (col == "Student_ID") {
    next
  }
  analyze_distribution(col)
}

plot_var("Grade")
plot_var("Total_salary")

###################################################
# Preprocessing
###################################################
set.seed(42069)
prepare_data <- function(data) {
  data_clean <- data[, !names(data) %in% c("Student_ID", "Course_ID")]
  data_clean[] <- lapply(data_clean, function(x) as.numeric(as.factor(x)))
  data_clean <- na.omit(data_clean)
  return(data_clean)
}
###################################################
# Clustering
###################################################

plot_clusters <- function(data_clust, km_model) {
  
  pca <- prcomp(data_clust, scale. = TRUE)
  
  plot_data <- data.frame(
    PC1 = pca$x[,1],
    PC2 = pca$x[,2],
    Cluster = factor(km_model$cluster)
  )
  
  ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size = 2) +
    labs(title = "Clusters",
         x = "PC1", y = "PC2") +
    theme_minimal()
}

plot_clusters_with_grade <- function(data_clust, km_model, original_data) {
  
  pca <- prcomp(data_clust, scale. = TRUE)
  
  plot_data <- data.frame(
    PC1 = pca$x[,1],
    PC2 = pca$x[,2],
    Cluster = factor(km_model$cluster),
    Grade = factor(original_data$Grade)
  )
  
  ggplot(plot_data, aes(x = PC1, y = PC2, color = Grade)) +
    geom_point(size = 2) +
    labs(title = "Clusters Colored by Grade",
         x = "PC1", y = "PC2") +
    theme_minimal()
}

km_clust <- prepare_data(data)
km <- kmeans(km_clust, centers = 3, nstart = 10)
plot_clusters(km_clust, km)
plot_clusters_with_grade(km_clust, km, data)

###################################################
# Linear Regression
###################################################

lm_data <- prepare_data(data)
lm_model <- lm(Grade ~ Listening_during_class + Weekly_study_hours + 
                 Taking_notes + Academic_Reading_frequency, lm_data)

# Evaluation
predictions <- predict(lm_model, lm_data)
actual <- lm_data$Grade

summary(lm_model)

predictions <- predict(lm_model, lm_data)

plot_data <- data.frame(
  Actual = actual,
  Predicted = predictions
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Model: Actual vs Predicted Grades",
       x = "Actual Grade",
       y = "Predicted Grade") +
  theme_minimal()

###################################################
# Random Forest
###################################################

rf_data <- prepare_data(data)
rf_model <- randomForest(Grade ~ Listening_during_class + Weekly_study_hours + 
                         Taking_notes + Academic_Reading_frequency, rf_data,
                         ntree = 100)

# Evaluation
predictions <- predict(rf_model, rf_data)
actual <- rf_data$Grade

summary(rf_model)
rmse <- sqrt(mean((actual - predictions)^2))
r2 <- 1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
cat("\nRandom Forest Evaluation:\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", r2, "\n")
predictions <- predict(lm_model, lm_data)

plot_data <- data.frame(
  Actual = actual,
  Predicted = predictions
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Random Forest: Actual vs Predicted",
       x = "Actual Grade",
       y = "Predicted Grade") +
  theme_minimal()

###################################################
# SVM and PCA
###################################################

variance_threshold <- 0.90
svm_clean <- prepare_data(data)
pca <- prcomp(svm_clean[, colnames(svm_clean) != "Grade"], scale. = TRUE)
var_explained <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
num_comp <- which(var_explained >= variance_threshold)[1]
cat("Number of components selected:", num_comp, "\n")

pca_data <- as.data.frame(pca$x[, 1:num_comp])
pca_data$Grade <- svm_clean$Grade

svm_model <- svm(
  Grade ~ .,
  data = pca_data,
  type = "eps-regression"
)
summary(svm_model)

predictions <- predict(svm_model, pca_data)
actual <- pca_data$Grade

rmse <- sqrt(mean((actual - predictions)^2))
r2   <- 1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
cat("\nSVM (PCA) Evaluation:\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", r2, "\n")

plot_data <- data.frame(
  Actual = actual, 
  Predicted = predictions
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "SVM with PCA: Actual vs Predicted",
       x = "Actual Grade", y = "Predicted Grade") +
  theme_minimal()
