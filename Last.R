train_data <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\3주차_데이터\\simple_train_data.csv")

ggplot(train_data, aes(x = thigh, y = upper_arm)) +
  geom_point(alpha = 0.7) +
  labs(title = "대퇴부 vs 상박부 산점도",
       x = "대퇴부 둘레 (thigh, cm)",
       y = "상박부 둘레 (upper_arm, cm)") +
  theme_minimal()

model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

residuals(model)

train_dt <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\3주차_데이터\\simple_train_2.csv")

ggplot(train_dt, aes(x = thigh, y = upper_arm)) +
  geom_point(alpha = 0.7) +
  labs(title = "대퇴부 vs 상박부 산점도",
       x = "대퇴부 둘레 (thigh, cm)",
       y = "상박부 둘레 (upper_arm, cm)") +
  theme_minimal()

model_t <- lm(upper_arm ~ thigh, data = train_dt)
summary(model_t)

residuals(model_t)

train_data <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\3주차_데이터\\simple_train_data.csv")
test_data <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\3주차_데이터\\simple_test_data.csv")

model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

saveRDS(model, "regression_model.rds")

load_model <- readRDS("regression_model.rds")

predicted <- predict(load_model, newdata = test_data)
head(predicted)

ggplot(train_data, aes(x = thigh, y = upper_arm)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "대퇴부 vs 상박부 산점도",
       x = "대퇴부 둘레 (thigh, cm)",
       y = "상박부 둘레 (upper_arm, cm)")

result <- data.frame(Actual = test_data$upper_arm, Predicted = predicted)

rmse <- sqrt(mean((result$Actual - result$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))

model <- lm(upper_arm ~ 0 + thigh, data = train_data)
summary(model)


data <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/자료/diabetes/diabetes.csv", header = TRUE, na = ".", stringsAsFactors =  TRUE)
skim(data)

data <- select(data, -c(Outcome))

result <- lm(data$Diabetes ~ ., data = data)
result1 <- lm(data$Diabetes ~ 1,  data = data)

anova_result <- anova(result, result1)
print(anova_result)

summary(result)

saveRDS(result, "regression_model.rds")

loaded_model <- readRDS("regression_model.rds")
rds <- readRDS("C:\\Users\\ji040\\OneDrive\\바탕 화면\\데이터\\regression_model.rds")

new_data <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/자료/diabetes/diabetes_test.csv")

predicted <- predict(loaded_model, newdata = new_data)
head(predicted)

results <- data.frame(new_data, Predicted = predicted)
summary(results)

write.csv(results, "predicted_values.csv", row.names = FALSE)

rmse <- sqrt(mean((results$Diabetes - results$Predicted)^2))

print(paste("RMSE: ", round(rmse, 3)))

data$Fitted <- fitted(result)
data$Residuals <- resid(result)

ggplot(data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted",
       x = "(Fitted Values)",
       y = "(Residuals)") +
  theme_minimal()

data$StdResid <- sqrt(abs(data$Residuals))

ggplot(data, aes(x = Fitted, y = StdResid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Scale-Location Plot",
       x = "(Fitted Values)",
       y = "Standardized Residuals)") +
  theme_minimal()

bptest(result)


set.seed(123)
durbinWatsonTest(result)


df <- Boston
skim(df)

initial_predictors <- c("lstat", "rm", "age", "dis", "rad", "tax", "ptratio", "indus", "nox", "crim")

train_indices <- createDataPartition(df$medv, p = 0.8, list = FALSE)

train_df <- df[train_indices, ]
test_df <- df[-train_indices, ]

results <- lm(train_df$medv ~ ., data = train_df[, initial_predictors])
summary(results)

source("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\Gradient Descent.R")

train_indices <- createDataPartition(df$medv, p = 0.8, list=FALSE)

df_train <- df[train_indices, ]
df_test <- df[-train_indices, ]

gd_model <- train_bb_lm(train_df, target <- "medv", preditors <- initial_predictors, l2 <- 1e-4)

train_metrics.bb_lm(gd_model, df_train)

pred <- predict(gd_model, newdata = df_test)
rmse <- sqrt(mean((df_test$medv - pred)^2))
r2 <- 1 - sum((df_test$medv - pred)^2) / sum((df_test$medv - mean(df_test$medv))^2)

print(round(pred, 3))
print(paste("RMSE:", round(rmse, 3)))
print(paste("R-Squared:", round(r2, 3)))

coef(gd_model)


opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(result)
par(opar)

shapiro_test(result$residuals)

vif_values <- vif(result)

print(vif_values)

selected_predictors <- names(vif_values[vif_values < 7])
print(selected_predictors)

influencePlot(result, id.method="identify")

df=df[-c(5, 446, 229, 580, 14), ]

df <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\diabetes\\diabetes.csv", header = TRUE, na = ".")
result <- lm(df$Diabetes ~ ., data = df)

result_bk = lm(df$Diabetes ~., data=df)
Df_fit_bk = stepAIC(result_bk, direction = , trace = T)

result_fw = lm(df$Diabetes ~ 1, data=df)
Df_fit_fw = stepAIC(result_fw, direction = , scope = df$Diabetes ~ Pregnancies + SkinThickness + Insulin + Age + Outcome, trace =  T)


d <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\UniversalBank\\UniversalBank.csv", header = TRUE, na = ".")

d$Personal.Loan <- ifelse(d$Personal.Loan == 1, "yes", "no")
d$Personal.Loan <- as.factor(d$Personal.Loan)

trainIndex <- createDataPartition(d$Personal.Loan, p = 0.8, list = FALSE)
train_data <- d[trainIndex, ]
test_data <- d[-trainIndex, ]

resultd = glm(train_data$Personal.Loan ~., data= train_data, family = "binomial")

tidy_result <- tidy(resultd)
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)

print(tidy_result)

vif(resultd)

train_data <-select(train_data, -c(Age, Experience, Income, ID, ZIP.Code, Mortgage, CD.Account))
test_data <- select(test_data, -c(Age, Experience, Income, ID, ZIP.Code, Mortgage, CD.Account))

result=glm(train_data$Personal.Loan ~., data= train_data, family = "binomial")

influenceIndexPlot(resultd, id.method="identify")

cook_d <- cooks.distance(resultd)
n <- nrow(train_data)
cutoff <- 4/n
influential_idx <- which(cook_d > cutoff)
influential_idx

test_predictions <- predict(resultd, newdata = test_data, type = "response")
predicted_class <- ifelse(test_predictions > 0.5, "yes", "no")
predicted_class <- as.factor(predicted_class)
print(predicted_class)

saveRDS(resultd, file = "logistic_model.rds")
Load <- readRDS("logistic_model.rds")

tidy_result <- tidy(Load)
tidy_result
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)


confusion_matrix <- confusionMatrix(predicted_class, test_data$Personal.Loan)
confusion_matrix

roc_curve <- roc(test_data$Personal.Loan, test_predictions)
plot(roc_curve)
auc(roc_curve)

d_test <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\UniversalBank\\UniversalBank_test.csv", header = TRUE, na = ".")

d_test <- select(d_test, -c(Age, Experience, Income))

test_predictions_new <- predict(result, newdata = d_test, type = "response")

predicted_class_new <- ifelse(test_predictions_new > 0.5, "yes", "no")
predicted_class_new <- as.factor(predicted_class_new)
print(test_predictions_new)

confusion_matrix <- confusionMatrix(predicted_class_new, d_test$Personal.Loan)
confusion_matrix

roc_curve <- roc(d_test$Personal.Loan, test_predictions)
plot(roc_curve)
auc(roc_curve)

data(mtcars)
skim(mtcars)

mtcars[sapply(mtcars, is.character)] <- lapply(mtcars[sapply(mtcars, is.character)], as.factor)
mtcars$am <- as.factor(mtcars$am)

train_indices <- createDataPartition(mtcars$am, p = 0.8, list= FALSE)
train_data <- mtcars[train_indices, ]
test_data <- mtcars[-train_indices, ]

control_params <- rpart.control(cp = 0.01, maxdepth = 5, minsplit = 10)

tree_model <- rpart(am ~ ., data = train_data, method = "class", control = control_params)

summary(tree_model)
rpart.plot(tree_model)

predicted_probs <- predict(tree_model, test_data, type= "prob")[,2]
predicted_probs

predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
predicted_classes <- as.factor(predicted_classes)
predicted_classes

confusion_matrix <- confusionMatrix(predicted_classes, test_data$am)
print(confusion_matrix)

roc_curve <- roc(test_data$am, predicted_probs)
plot(roc_curve)
auc(roc_curve)


library(dplyr)
library(broom)
library(rpart.plot)
library(rpart)
library(e1071)
library(GGally)
library(tidyverse)
library(tidymodels)
library(MASS)
library(caret)
library(car)
library(lmtest)
library(ggplot2)
library(skimr)
library(rstatix)
library(C50)
library(caret)
library(pROC)

df=read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\실습데어터코드\\customer.csv", stringsAsFactors = FALSE)

char_cols <- sapply(df, is.character)
df[char_cols] <- lapply(df[char_cols], function(x) {
  x <- trimws(x)
  x[x == ""] <- NA
  factor(x)
})

for (v in names(df)) {
  if (is.factor(df[[v]])) {
    lv <- levels(df[[v]])
    lv[lv == ""] <- "missing"
    lv <- make.names(lv)
    levels(df[[v]]) <- lv
  }
} 

names(df) <- make.names(names(df), unique = TRUE)

df <- df[!is.na(df$Segmentation), ]
df$Segmentation <- droplevels(df$Segmentation)

skim(df)
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
df$am <- as.factor(df$am)

train_indices <- createDataPartition(df$Segmentation, p = 0.8, list= FALSE)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

ctrl_c50 <- C5.0Control(CF = 0.25, minCases = 2, winnow = FALSE)

c50_model <- C5.0(Segmentation ~ ., data = train_data, trials = 1)

summary(c50_model)
rpart.plot(c50_model)

pred_probs <- predict(c50_model, test_data, type= "prob")
pred_probs

pred_classes <- predict(c50_model, test_data, type= "class")
pred_classes <- as.factor(pred_classes)
pred_classes

conf_mat <- confusionMatrix(pred_classes, test_data$Segmentation)
print(conf_mat)

roc_curve <- multiclass.roc(response = test_data$Segmentation, predictor = pred_probs)
roc_curve$auc

classes <- levels(test_data$Segmentation)
roc_list <- list()

for (cl in classes) {
  response_bin <- factor(test_data$Segmentation == cl,
                         levels = c(FALSE, TRUE),
                         labels = c("other", cl))
  prob_cl <- pred_probs[, cl]
  roc_list[[cl]] <- roc(response_bin, prob_cl, levels = c("other", cl))
}

plot(roc_list[[1]], col = 1, lwd = 2,
     main = "One-vs-Rest ROC Curves (C5.0, 4-class)")
if (length(classes) > 1) {
  for (i in 2:length(classes)) {
    plot(roc_list[[i]], col = i, lwd = 2, add = TRUE)
  }
}
legend("bottomright",
       legend = paste0(classes, " (AUC=", round(sapply(roc_list, auc), 3), ")"),
       col    = seq_along(classes),
       lwd    = 2, cex = 0.8)