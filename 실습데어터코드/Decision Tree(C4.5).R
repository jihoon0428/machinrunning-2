library(C50)     # C5.0 (C4.5 후속)
library(caret)
library(pROC)

df=read.csv("C:/Users/ji040/OneDrive/바탕 화면/실습데이터/student_sleep_patterns.csv")

# 1) 문자형 컬럼 공백 제거 후 factor로 변환
char_cols <- sapply(df, is.character)
df[char_cols] <- lapply(df[char_cols], function(x) {
  x <- trimws(x)        # 앞뒤 공백 제거
  x[x == ""] <- NA      # 완전 빈 문자열은 NA로 처리
  factor(x)
})

# 2) factor 레벨을 make.names 로 안전하게 변환
for (v in names(df)) {
  if (is.factor(df[[v]])) {
    lv <- levels(df[[v]])
    lv[lv == ""] <- "missing"
    lv <- make.names(lv)
    levels(df[[v]]) <- lv
  }
} 

# 3) 컬럼 이름도 안전하게
names(df) <- make.names(names(df), unique = TRUE)


# 4) 타겟(종속변수)에 NA 있으면 제거
df$Gender <- factor(df$Gender)
df <- df[!is.na(df$Gender), ]
df$Gender <- droplevels(df$Gender)

# 5) 모델 및 예측
skim(df)

df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)

train_indices <- createDataPartition(df$Gender, p =0.8, list = FALSE)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

ctrl_C50 <- C5.0Control(CF = 0.25, minCases = 2, winnow = FALSE)
C50_model <- C5.0(train_data$Gender ~ Age + Sleep_Duration + Study_Hours + Screen_Time + 
                    Caffeine_Intake + Physical_Activity + Weekday_Sleep_Start + Weekday_Sleep_End, data = train_data, control = ctrl_C50, trials = 1)

summary(C50_model)
plot(C50_model)

pred_prob <- predict(C50_model, newdata = test_data, type = "prob")
pred_prob
pred_class <- predict(C50_model, newdata = test_data, type = "class")
pred_class <- as.factor(pred_class)
pred_class

conf_mat <- confusionMatrix(pred_class, test_data$Gender)
print(conf_mat)

mc_roc <-multiclass.roc(response = test_data$Gender, predictor = pred_prob)
mc_roc$auc

# 6) 클래스별 ROC 커브
classes <- levels(test_data$Gender)
roc_list <- list()

for (cl in classes) {
  response_bin <- factor(test_data$Gender == cl,
                         levels = c(FALSE, TRUE),
                         labels = c("other", cl))
  prob_cl <- pred_prob[, cl]
  roc_list[[cl]] <- roc(response_bin, prob_cl, levels = c("other", cl))
}

plot(roc_list[[1]], col = 1, lwd = 2,
     main = "Gender vs Rest ROC Curves (C5.0, 4-class)")
if (length(classes) > 1) {
  for (i in 2:length(classes)) {
    plot(roc_list[[i]], col = i, lwd = 2, add = TRUE)
  }
}
legend("bottomright",
       legend = paste0(classes, " (AUC=", round(sapply(roc_list, auc), 3), ")"),
       col    = seq_along(classes),
       lwd    = 2, cex = 0.8)
