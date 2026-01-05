confusion_matrix <- confusionMatrix(predicted_class, test_data$Personal.Loan)
confusion_matrix

roc_curve <- roc(test_data$Personal.Loan, test_predictions)
plot(roc_curve)
auc(roc_curve)
    