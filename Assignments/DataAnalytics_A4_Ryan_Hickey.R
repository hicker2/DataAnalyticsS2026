# ── Load libraries ─────────────────────────────────────────────
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)
library(broom)
library(pROC)


# set working directory
setwd("~\\GitHub\\DataAnalyticsS2026\\Assignments")

# read data
df <- read_csv("users_prs.csv")

# ── Cleaning ──────────────────────────────────────────────────

clean_df <- df %>%
  
  # Remove bots
  filter(!str_detect(tolower(login), "bot"),
         !str_detect(tolower(name), "bot")) %>%
  
  # Valid PR rows only
  filter(!is.na(pr_number)) %>%
  
  # Keep only resolved PRs
  filter(pr_state %in% c("MERGED", "CLOSED")) %>%
  
  mutate(
    # Binary target variable
    merged_flag = ifelse(pr_state == "MERGED", 1, 0),
    
    # PR size
    pr_size = pr_additions + pr_deletions,
    
    # Title length (proxy for clarity/detail)
    title_length = nchar(pr_title),
    
    # Time to resolution (in days)
    time_to_close = as.numeric(
      difftime(as.POSIXct(pr_closed_at),
               as.POSIXct(pr_created_at),
               units = "days")
    )
  )


# ── Quick sanity checks ────────────────────────────────────────
cat("Rows:", nrow(clean_df), "\n")
cat("Users:", n_distinct(clean_df$login), "\n")
cat("Merge rate:", mean(clean_df$merged_flag, na.rm = TRUE), "\n")
cat("Unique users:", n_distinct(clean_df$login), "\n")
count(clean_df, pr_state)

# ── 1. PR SIZE vs MERGE ────────────────────────────────────────
ggplot(clean_df, aes(x = factor(merged_flag), y = pr_size)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "PR Size vs Merge Outcome",
    x = "Merged (1 = Yes, 0 = Closed)",
    y = "PR Size (log scale)"
  ) +
  theme_minimal()

# ── 2. TIME TO CLOSE vs MERGE ─────────────────────────────────
ggplot(clean_df, aes(x = factor(merged_flag), y = time_to_close)) +
  geom_boxplot() +
  labs(
    title = "Time to Close vs Merge Outcome",
    x = "Merged (1 = Yes, 0 = Closed)",
    y = "Days to Close"
  ) +
  theme_minimal()

# ── 3. TITLE LENGTH vs MERGE ──────────────────────────────────
ggplot(clean_df, aes(x = factor(merged_flag), y = title_length)) +
  geom_boxplot() +
  labs(
    title = "Title Length vs Merge Outcome",
    x = "Merged (1 = Yes, 0 = Closed)",
    y = "Title Length (characters)"
  ) +
  theme_minimal()

# ── 4. USER-LEVEL EFFECTS ─────────────────────────────────────
user_summary <- clean_df %>%
  group_by(login) %>%
  summarise(
    total_prs = n(),
    merge_rate = mean(merged_flag),
    avg_pr_size = mean(pr_size, na.rm = TRUE),
    avg_time_to_close = mean(time_to_close, na.rm = TRUE),
    
    # Add follower/following info (same per user, so take first)
    followers = first(followers),
    following = first(following),
    
    .groups = "drop"
  )

# ── Merge rate distribution ───────────────────────────────────
ggplot(user_summary, aes(x = merge_rate)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of User Merge Rates",
    x = "Merge Rate",
    y = "Count"
  ) +
  theme_minimal()

# ── Followers vs Merge Rate ───────────────────────────────────
ggplot(user_summary, aes(x = followers, y = merge_rate)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  labs(
    title = "Followers vs Merge Rate",
    x = "Followers (log scale)",
    y = "Merge Rate"
  ) +
  theme_minimal()

# ── Following vs Merge Rate ───────────────────────────────────
ggplot(user_summary, aes(x = following, y = merge_rate)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  labs(
    title = "Following vs Merge Rate",
    x = "Following (log scale)",
    y = "Merge Rate"
  ) +
  theme_minimal()

shapiro.test(user_summary$merge_rate)

# ── Followers grouped (boxplot) ───────────────────────────────
user_summary <- user_summary %>%
  mutate(
    follower_group = case_when(
      followers == 0 ~ "0",
      followers <= 10 ~ "1-10",
      followers <= 100 ~ "11-100",
      followers <= 1000 ~ "101-1000",
      TRUE ~ "1000+"
    )
  )

ggplot(user_summary, aes(x = follower_group, y = merge_rate)) +
  geom_boxplot() +
  labs(
    title = "Merge Rate by Follower Group",
    x = "Follower Group",
    y = "Merge Rate"
  ) +
  theme_minimal()

# ── 5. CORRELATION CHECK ──────────────────────────────────────
numeric_df <- clean_df %>%
  select(merged_flag, pr_size, title_length, time_to_close) %>%
  na.omit()

cor_matrix <- cor(numeric_df)

print("Correlation Matrix:")
print(cor_matrix)

# ── Modeling Time! ──────────────────────────────────────
set.seed(42)

# ── Prepare modeling dataset ───────────────────────────────────
model_df <- clean_df %>%
  mutate(
    # Convert to datetime
    created_at = as.POSIXct(created_at),
    pr_created_at = as.POSIXct(pr_created_at),
    
    # Feature engineering
    account_age_days = as.numeric(difftime(Sys.time(), created_at, units = "days")),
    pr_year = as.numeric(format(pr_created_at, "%Y")),
    pr_month = as.numeric(format(pr_created_at, "%m"))
  ) %>%
  select(
    followers,
    following,
    public_repos,
    merged_flag,
    pr_size,
    title_length,
    time_to_close,
    account_age_days,
    pr_year,
    pr_month
  ) %>%
  na.omit()

# Convert target to factor for classification models
model_df$merged_flag <- as.factor(model_df$merged_flag)

# ── Train/Test Split ───────────────────────────────────────────
train_index <- createDataPartition(model_df$merged_flag, p = 0.7, list = FALSE)
train_data <- model_df[train_index, ]
test_data  <- model_df[-train_index, ]

train_data <- train_data %>%
  mutate(
    log_time_to_close = log1p(time_to_close),
    log_account_age = log1p(account_age_days)
  )

test_data <- test_data %>%
  mutate(
    log_time_to_close = log1p(time_to_close),
    log_account_age = log1p(account_age_days)
  )

train_balanced <- downSample(
  x = train_data[, -which(names(train_data) == "merged_flag")],
  y = train_data$merged_flag
)

# ── Evaluation Function ────────────────────────────────────────
evaluate_model <- function(true, pred_class, pred_prob = NULL, name = "Model") {
  cat("\n============================\n")
  cat(name, "\n")
  cat("============================\n")
  
  # Confusion Matrix
  cm <- confusionMatrix(as.factor(pred_class), as.factor(true))
  print(cm)
  
  # Extract metrics
  precision <- cm$byClass["Precision"]
  recall    <- cm$byClass["Recall"]
  f1        <- cm$byClass["F1"]
  accuracy  <- cm$overall["Accuracy"]
  
  cat("\n--- Metrics ---\n")
  cat("Accuracy :", round(accuracy, 4), "\n")
  cat("Precision:", round(precision, 4), "\n")
  cat("Recall   :", round(recall, 4), "\n")
  cat("F1 Score :", round(f1, 4), "\n")
}

# ── Model 1: Logistic Regression (All features) ────────────
model1 <- glm(
  merged_flag ~ log1p(pr_size) + title_length + time_to_close +
    account_age_days + pr_year + pr_month + followers + following + public_repos,
  data = train_data,
  family = "binomial"
)

pred1 <- predict(model1, test_data, type = "response")
pred1_class <- ifelse(pred1 > 0.5, 1, 0)
summary(model1)

coef_df <- tidy(model1) %>%
  filter(term != "(Intercept)")

ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Feature Effects (Logistic Regression)",
    x = "Feature",
    y = "Coefficient"
  ) +
  theme_minimal()

evaluate_model(test_data$merged_flag, pred1_class,, "Model 1: Logistic")

# ── Model 2: Logistic Regression (+ user features) ─────────────
model2 <- glm(
  merged_flag ~ title_length + time_to_close + 
    account_age_days + pr_year + following,
  data = train_data,
  family = "binomial"
)

pred2 <- predict(model2, test_data, type = "response")
pred2_class <- ifelse(pred2 > 0.5, 1, 0)
summary(model2)

test_data$pred_prob2 <- pred2

ggplot(test_data, aes(x = pred_prob2, fill = merged_flag)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(
    title = "Model 2: Predicted Probability Distribution",
    x = "Predicted Probability of Merge",
    fill = "Actual Outcome"
  ) +
  theme_minimal()

evaluate_model(test_data$merged_flag, pred2_class,, "Model 2: Logistic (Only important features)")

# ── Model 3: Random Forest ─────────────────────────────────────

model3 <- randomForest(
  merged_flag ~ pr_size + title_length + time_to_close + followers + following,
  data = train_data,
  ntree = 100,
  # classwt = c("0" = 2, "1" = 1)
)

pred3_class <- predict(model3, test_data)

imp <- importance(model3)
imp_df <- data.frame(
  Feature = rownames(imp),
  Importance = imp[,1]
)

ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Random Forest Feature Importance",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal()

pred3_prob <- predict(model3, test_data, type = "prob")[,2]
test_data$pred3_prob <- pred3_prob

ggplot(test_data, aes(x = pred3_prob, fill = merged_flag)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(
    title = "Random Forest: Predicted Probabilities",
    x = "Probability of Merge",
    fill = "Actual Outcome"
  ) +
  theme_minimal()

evaluate_model(test_data$merged_flag, pred3_class,, "Model 3: Random Forest")

# ── Model 4: Support Vector Machine (SVM) ──────────────────────
train_data <- train_data %>%
  mutate(across(where(is.numeric), scale))

test_data <- test_data %>%
  mutate(across(where(is.numeric), scale))

train_data <- train_data %>%
  mutate(
    log_pr_size = log1p(pr_size),
    log_time_to_close = log1p(time_to_close),
    log_followers = log1p(followers),
    log_following = log1p(following)
  )

test_data <- test_data %>%
  mutate(
    log_pr_size = log1p(pr_size),
    log_time_to_close = log1p(time_to_close),
    log_followers = log1p(followers),
    log_following = log1p(following)
  )
train_data <- train_data %>%
  mutate(
    size_time_interaction = log_pr_size * log_time_to_close
  )
train_data$merged_flag <- factor(
  train_data$merged_flag,
  levels = c(0, 1),
  labels = c("Closed", "Merged")
)

test_data$merged_flag <- factor(
  test_data$merged_flag,
  levels = c(0, 1),
  labels = c("Closed", "Merged")
)
control <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE
)

model4 <- train(
  merged_flag ~ log_time_to_close + log_following,
  data = train_data,
  method = "svmRadial",
  trControl = control,
  tuneLength = 5
)

pred4_class <- predict(model4, test_data)
pred_prob4 <- predict(model4, test_data, type = "prob")[, "Merged"]

test_data$pred_prob4 <- pred_prob4

ggplot(test_data, aes(x = pred_prob4, fill = merged_flag)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(
    title = "SVM: Predicted Probability Distribution",
    x = "Predicted Probability of Merge",
    fill = "Actual Outcome"
  ) +
  theme_minimal()


evaluate_model(test_data$merged_flag, pred4_class,, "Model 4: SVM")


