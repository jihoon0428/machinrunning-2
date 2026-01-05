data <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\실습데어터코드\\StudentsPerformance.csv")

data$gender <- as.factor(data$gender)
trainIndex <- createDataPartition(data$gender, p = 0.8, list = FALSE)

train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

constrol_params <- rpart.control(cp = 0.01, maxdepth = 5, minsplit = 10)

tree_model <- rpart(gender ~ math.score + reading.score + writing.score, data = data, method = "class", control = constrol_params)

summary(tree_model)
rpart.plot(tree_model)

predict_probs <- predict(tree_model, test_data, type = "prob")[,2]
predict_probs

predicted_classes <- ifelse(predict_probs > 0.5, 1, 0)
predicted_classes <- as.factor(predicted_classes)


confusion_matrix <- confusionMatrix(predicted_classes, test_data$gender)
print(confusion_matrix)

probs <- predict(tree_model, newdata = test_data, type = "prob")

predicted_classes <- ifelse(probs[, "female"] > 0.5, "female", "male")

predicted_classes <- factor(predicted_classes, levels = levels(test_data$gender))

confusion_matrix <- confusionMatrix(predicted_classes, test_data$gender)
print(confusion_matrix)
