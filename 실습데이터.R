dt <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/실습데이터/heart.csv")

dt$sex <- ifelse(dt$sex ==1, "female"
dt$sex <- as.factor(dt$sex)

trainIndex <- createDataPartition(dt$sex, p = 0.8, list = FALSE)
train_data <- dt[trainIndex, ]
test_data <-dt[-trainIndex, ]

result = glm(train_data$sex ~ trestbps + Study_Hours + Screen_Time + 
               Caffeine_Intake + Physical_Activity + Weekday_Sleep_Start + Weekday_Sleep_End, data = train_data, family = "binomial")

tidy_result <- tidy(result)
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)

print(tidy_result)

vif