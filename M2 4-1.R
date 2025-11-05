train_data <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\3주차_데이터\\simple_train_data.csv")
test_data <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\3주차_데이터\\simple_test_data.csv")

model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

saveRDS(model, "regression_model.rds")

loaded_model <- readRDS("regression_model.rds")

predicated <- predict(loaded_model, newdata = test_data)
head(predicated)

ggplot(train_data, aes(x = thigh, y = upper_arm)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "대퇴부 vs 상박부 회귀분석",
       x = "대퇴부 둘레(cm)",
       y = "상박부 둘레(cm)")

results <- data.frame(Actual = test_data$upper_arm, Predicted = predicated)

rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))

model <- lm(upper_arm ~ 0 + thigh, data = train_data)
summary(model)

model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)