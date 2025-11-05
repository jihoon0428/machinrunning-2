install.packages("rstatix")
install.packages("skimr")

library(rstatix)
library(skimr)

df <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\diabetes\\diabetes.csv", header = TRUE, na = ".", stringsAsFactors = TRUE)
skim(df)

df <- select(df, -c(var1))
df <- select(df, -c(SkinThickness, Insulin, Outcome))
skim(df)

result <- lm(df$Diabetes ~ ., data = df)
result1 <- lm(df$Diabetes ~ 1, data = df)

anova_result <- anova(result, result1)
print(anova_result)

summary(result)

saveRDS(result, "regression_model.rds")

loaded_model <- readRDS("regression_model.rds")

new_data <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\diabetes\\diabetes_test.csv")

predicated <- predict(loaded_model , newdata = new_data)
head(predicated)

results <- data.frame(new_data, Predicated = predicated)

write.csv(results, "C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\diabetes\\predicted_values.csv", row.names = FALSE)

rmse <- sqrt(mean((results$Diabetes - results$Predicated)^2))

print(paste("RMSE:", round(rmse, 3)))