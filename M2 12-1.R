data("mtcars")
skim(mtcars)

mtcars[sapply(mtcars, is.character)] <- lapply(mtcars[sapply(mtcars, is.character)], as.factor)
mtcars$am <- as.factor(mtcars$am)

train_indices <- createDataPartition(mtcars$am, p = 0.8, list = FALSE)
train_data <- mtcars[train_indices, ]
test_data <- mtcars[-train_indices, ]

control_params <- rpart.control(cp=0.01, maxdepth = 5, minsplit = 10)

tree_model <- rpart(am ~ ., data = train_data, method ="class", control = control_params)

summary(tree_model)
rpart.plot(tree_model)

predicted_probs <- predict(tree_model, test_data, type = "prob")[,2]
predicted_probs

predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
predicted_classes <- as.factor(predicted_classes)
predicted_classes

confusion_Matrix <- confusionMatrix(predicted_classes, test_data$am)
print(confusion_Matrix)

roc_curve <- roc(test_data$am, predicted_probs)
plot(roc_curve)
auc(roc_curve)
