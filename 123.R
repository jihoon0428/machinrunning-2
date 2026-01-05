df_test <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\UniversalBank\\UniversalBank_test.csv", header = TRUE, na = ".")

df_test <- select(df_test, -c(Age, Experience, Income))

test_predictions_new <- predict(result, newdata = df_test, type = "response")

predicted_class_new <- ifelse(test_predictions_new > 0.5, "yes", "no")
predicted_class_new <- as.factor(predicted_class_new)
print(test_predictions_new)

confusion_matrix <- confusionMatrix(predicted_class_new, df_test$Personal.Loan)
confusion_matrix

roc_curve <- roc(df_test$Personal.Loan, test_predictions)
plot(roc_curve)
auc(roc_curve)
