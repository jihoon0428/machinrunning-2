data <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\실습데어터코드\\StudentsPerformance.csv")

test_df <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\실습데어터코드\\StudentsPerformance.csv")

model <- lm(reading.score ~ writing.score, data = data)

summary(model)

predicted <- predict(model, newdata = test_df)

result <- data.frame(Actual <- test_df$reading.score, Predicted = predicted)

rmse <- sqrt(mean((result$Actual - result$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))

model <- lm(reading.score ~ 0 + writing.score, data=data)
summary(model)
