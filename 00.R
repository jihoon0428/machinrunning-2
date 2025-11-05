loaded_model <- readRDS("exam_1.rds")

data <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/중간고사_데이터/exam_1.csv")

predicted <- predict(loaded_model, newdata = data)

data2 <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/중간고사_데이터/exam_2.csv")

model <- lm(data2$y ~ ., data = data2) #x2, x4
summary(model)

initial_predictors <- c("X1", "X3", "X5")

train_indices <- createDataPartition(data2$y, p=0.8, list = FALSE)

train_df <- data2[train_indices, ]

data2 <- select(data2, -c(X2, X4))

result <- lm(data2$y ~ ., data = data2)

summary(result)
coef(result)
data2$StdResid <- sqrt(abs(data2$Residuals))
ggplot(data2, aes(x = Fitted, y = StdResid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Scale-Location Plot",
       x = "(Fitted Values)",
       y = "Standardized Residuals)") +
  theme_minimal()

bptest(result) 

durbinWatsonTest(result)

data3 <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/중간고사_데이터/exam_3.csv")

model <- lm(data3$y ~ ., data = data3) #x2, x4
summary(model)

data3 <- select(data3, "X1", "X2", "X4")

result <- lm(data3$y ~ ., data = data3)

summary(result)
opar <- par(no.readonly =  TRUE)
par(mfrow = c(2, 2))
plot(result)
par(opar)

shapiro_test(result$residuals)

data4 <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/중간고사_데이터/exam_4.csv")

model <- lm(data4$y ~ ., data = data4) # "X4, X5, X6 X8, X9, X11, X12"
summary(model)

data4 <- select(data4, -c(X4, X5, X6, X8, X9, X11, X12))

result <- lm(data4$y ~ ., data = data4)

summary(result)

vif_values <- vif(result)

print(vif_values)

selected_predictors <- names(vif_values[vif_values > 8])
print(selected_predictors)

influencePlot(result, id.method="identify")

data4= data4[-c(70, 172, 252, 159, 211), ]

data5 <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/중간고사_데이터/exam_5.csv")
summary(data5)

data5_a <- select(data5, -c(y_B))

result <- lm(data5_a$y_A ~ ., data = data5_a)#x3, x5, x6

summary(result)

data5_a <- select(data5, -c(X3, X5, X6, y_B))


data5_b <- select(data5, -c(X1, X2, y_A))

result2 <- lm(data5_b$y_B ~ ., data = data5_b) #x1, x2

summary(result2)

result_bk = lm(data5_a$y_A ~., data=data5)
Df_fit_bk = stepAIC(result_bk, direction = , trace = T)

result_fw = lm(data5_a$y_A ~ 1, data=data5)
Df_fit_fw = stepAIC(result_fw, direction = , scope = data5_a$y_A ~ X1 + X2 + X4, trace =  T)

result_bk = lm(data5_b$y_B ~., data=data5_b)
Df_fit_bk = stepAIC(result_bk, direction = , trace = T)

result_fw = lm(data_5_b$y_B ~ 1, data=data_5_b)
Df_fit_fw = stepAIC(result_fw, direction = , scope = df$Diabetes ~ Pregnancies + SkinThickness + Insulin + Age + Outcome, trace =  T)
