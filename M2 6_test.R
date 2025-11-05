data <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\synthetic.csv")
skim(data)

model <- lm(data$y ~ ., data = data)
summary(model)

result <- lm(data$y ~ ., data = data)
vif_values <- vif(result)

print(vif_values)

selected_predictors <- names(vif_values[vif_values > 7])
print(selected_predictors)# vif 값이 7 초과인 변수: x1, x2

influencePlot(result, id.method="identify")

data = data[-c(950, 282, 330, 1011), ]

set.seed(123)
initial_predictors <- c("x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")

train_indices <- createDataPartition(data$y, p =0.8, list= FALSE)

train_data <- data[train_indices, ]
test <- data[-train_indices, ]

result <- lm(train_data$y ~ ., data = train_data[, initial_predictors])
summary(result)

coef(result)

source("C:\\Users\\ji040\\OneDrive\\문서\\Gradient Descent.R")

gd_model <- train_bb_lm(train_data, target = "y", predictors = initial_predictors, l2 = 1e-4)
train_metrics.bb_lm(gd_model, train_data)

pred <- predict(gd_model, newdata = test)
rmse <- sqrt(mean((test$y - pred)^2))
r2 <- 1 - sum((test$y - pred)^2) / sum((test$y - mean(test$y))^2)

print(round(pred, 3))
print(paste("RMSE:" ,round(rmse, 3)))
print(paste("R-Squared:", round(r2, 3)))

coef(gd_model)
