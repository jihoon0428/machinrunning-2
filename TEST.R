train_data <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/자료/3주차_데이터/simple_train_data.csv")

ggplot(train_data, aes(x = thigh, y = upper_arm)) +
  geom_point(alpha = 0.7) +
  labs(title = "대퇴부 vs 상박부 산점도",
       x  = "대퇴부 둘레 (thigh, cm)",
       y = "상박부 둘레 (upper_arm, cm)") +
  theme_minimal()

model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

residuals(model)

train_data <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/자료/3주차_데이터/simple_train_2.csv")

ggplot(train_data, aes(x = thigh, y = upper_arm)) +
  geom_point(alpha = 0.7) +
  labs(title = "대퇴부 vs 상박부 산점도",
       x  = "대퇴부 둘레 (thigh, cm)",
       y = "상박부 둘레 (upper_arm, cm)") +
  theme_minimal()

model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

residuals(model)

train_data <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/자료/3주차_데이터/simple_train_data.csv")
test_data <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/자료/3주차_데이터/simple_train_2.csv")

model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

saveRDS(model, "regression_model.rds")

loaded_model <- readRDS("regression_model.rds")

predicted <- predict(loaded_model, newdata = test_data)
head(predicted)

ggplot(train_data, aes(x = thigh, y = upper_arm)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  labs(title = "대퇴부 vs 상박부 산점도",
       x  = "대퇴부 둘레 (thigh, cm)",
       y = "상박부 둘레 (upper_arm, cm)")

result <- data.frame(Actual = test_data$upper_arm, Predicted = predicted)

rmse <- sqrt(mean((result$Actual - result$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))

model <- lm(upper_arm ~ 0 + thigh, data = train_data)
summary(model)

df <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/자료/diabetes/diabetes.csv", header = TRUE, na =".", stringsAsFactors = TRUE)
skim(df)

df <- select(df, -c())

result <- lm(df$Diabetes ~ ., data = df)
result1 <- lm(df$Diabetes ~ 1,  data = df)

anova_result <- anova(result, result1)
print(anova_result)

summary(result)

saveRDS(model, "regression_model.rds")

loaded_model <- readRDS("regression_model.rds")

new_data <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/자료/diabetes/diabetes_test.csv")

predicted <- predict(loaded_model, newdata = new_data)
head(predicted)

results <- data.frame(new_data, Predicted = predicted)

write.csv(results, "predicted_values.csv", row.names = FALSE)

rmse <- sqrt(mean((results$Diabetes - results$Predicted)^2))

print(paste(, round(rmse, 3)))

df$Fitted <- fitted(result)
df$Residuals <- resid(result)

ggplot(df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted",
       x = "(Fitted Values)",
       y = "(Residuals)") +
  theme_minimal()
       
df$StdResid <- sqrt(abs(df$Residuals))

ggplot(df, aes(x = Fitted, y = StdResid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Scale-Location Plot",
       x = "(Fitted Values)",
       y = "Standardized Residuals)") +
  theme_minimal()

bptest(result)

durbinWatsonTest(result)

set.seed(123)

data("Boston")
df <- Boston
skim(df)

initial_predictors <- c("lstat", "rm", "age", "dis", "rad", "tax", "ptratio", "indus", "nox", "crim")
initial_predictors <- names(select(df,))

train_indices <- createDataPartition(df$medv, p=0.8, list = FALSE)

train_df <- df[train_indices, ]
test_df <- df[-train_indices, ]

result <- lm(train_df$medv ~ ., data = train_df[, initial_predictors])
summary(result)

source("C:\\Users\\ji040\\OneDrive\\문서\\Gradient Descent.R")

train_indices <- createDataPartition(df$medv, p = 0.8, list = FALSE)

train_df <- df[train_indices, ]
test_df <- df[-train_indices, ]

gd_model <- train_bb_lm(train_df, target <- "medv", preditors <- initial_predictors, l2 <- 1e-4)

train_metrics.bb_lm(gd_model, train_df)

pred <- predict(gd_model, newdata = test_df)
rmse <- sqrt(mean((test_df$medv - pred)^2))
r2 <- 1 - sum((test_df$medv - pred)^2) / sum((test_df$medv - mean(test_df$medv))^2)

print(round(pred, 3))
print(paste( ,round(rmse, 3)))
print(paste(, round(r2, 3)))

coef(gd_model)

opar <- par(no.readonly =  TRUE)
par(mfrow = c(2, 2))
plot(result)
par(opar)

shapiro_test(result$residuals)

vif_values <- vif(result)

print(vif_values)

selected_predictors <- names(vif_values[vif_values < 7])
print(selected_predictors)

influencePlot(result, id.method="identify")

df=df[-c(381, 419, 366, 369, 373), ]

df <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\diabetes\\diabetes.csv", header = TRUE, na = ".")
result <- lm(df$Diabetes ~ ., data = df)

result_bk = lm(df$Diabetes ~., data=df)
Df_fit_bk = stepAIC(result_bk, direction = , trace = T)

result_fw = lm(df$Diabetes ~ 1, data=df)
Df_fit_fw = stepAIC(result_fw, direction = , scope = df$Diabetes ~ Pregnancies + SkinThickness + Insulin + Age + Outcome, trace =  T)
