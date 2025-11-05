set.seed(123)

data("Boston")
df <- Boston
skim(df)

initial_predictors <- c("lstat", "rm", "age", "dis", "rad", "tax", "ptratio", "indus", "nox", "crim")

train_indices <- createDataPartition(df$medv, p=0.8, list = FALSE)

train_df <- df[train_indices, ]
test_df <- df[-train_indices, ]

result <- lm(train_df$medv ~ ., data = train_df[, initial_predictors])
summary(result)



source("C:\\Users\\ji040\\OneDrive\\문서\\Gradient Descent.R")

train_indices <- create_test_label(df$medv, p = 0.8, list = FALSE)

train_df <- df[train_indices, ]
test_df <- df[-train_indices, ]

gd_model <- train_bb_lm(train_df, target <- "medv", preditors <- initial_predictors, l2 <- 1e-4)

train_metrics.bb_lm(gd_model, train_df)

pred <- predict(gd_model, newdata = test_df)
rmse <- sqrt(mean((test_df$medv - pred)^2))
r2 <- 1 - sum((test_df$medv - pred)^2) / sum((test_df$medv - mean(test_df$medv))^2)

print(round(pred, 3))
print(paste("RMSE:" ,round(rmse, 3)))
print(paste("R-Squared:", round(r2, 3)))

coef(gd_model)
