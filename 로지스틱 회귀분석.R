df <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\실습데어터코드\\StudentsPerformance.csv")

df <- df[, c("gender", "math.score", "reading.score", "writing.score")]

df$gender <- as.factor(df$gender)

trainIndex <- createDataPartition(df$gender, p = 0.8, list = FALSE)

train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]
result = glm(train_data$gender ~ math.score + reading.score + writing.score, data=train_data, family = "binomial")

tidy_result <- tidy(result)
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)

print(tidy_result)

vif(result)

test_data <- select("gender", "math.score", "reading.score", "writing.score")

test_predictions <- predict(result, newdata = test_data, type = "response")
predicted_class <- ifelse(test_predictions > 0.5, "female", "male")
predicted_class <- as.factor(predicted_class)

saveRDS("logistic_model.rds")

Load <- readRDS("logistic_model.rds")

tidy_result <- tidy(Load)
tidy_result
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)

confusion_Matrix <- confusionMatrix(predicted_class, test_data$gender)
confusion_Matrix
