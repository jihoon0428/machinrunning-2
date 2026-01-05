df <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/자료/UniversalBank/UniversalBank.csv", header = TRUE, na =".")

df$Personal.Loan <- ifelse(df$Personal.Loan == 1, "yes", "no")
df$Personal.Loan <- as.factor(df$Personal.Loan)

trainIndex <- createDataPartition(df$Personal.Loan, p = 0.8, list = FALSE)
train_data <- df[trainIndex,]
test_data <- df[-trainIndex,]

result = glm(train_data$Personal.Loan ~., data = train_data, family = "binomial")

tidy_result <- tidy(result)
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)

print(tidy_result)

df <- select(df, -c(Education, CD.Account))

vif(result)

df <-select(df, -c(Age, Experience))

result = glm(train_data$Personal.Loan ~., data = train_data, family = "binomial")

influencePlot(result, id.method="identify")

df = df[-c(783, 385)]

result = glm(train_data$Personal.Loan ~., data = train_data, family = "binomial")

summary(result)

cook_d <- cooks.distance(result)
n <- nrow(train_data)

cutoff <- 4 / n
influence_idx <-which(cook_d > cutoff)

test_predictions <- predict(result, newdata = test_data, type = "response")
predicted_class <- ifelse(test_predictions > 0.5, "yes", "no")
predicted_class <- as.factor(predicted_class)

saveRDS(result, file = "logistic_model.rds")

Load <- readRDS("logistic_model.rds")

tidy_result <- tidy(Load)
tidy_result
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)

