df <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\diabetes\\diabetes.csv", header = TRUE, na = ".")
result <- lm(df$Diabetes ~ ., data = df)

result_bk = lm(df$Diabetes ~., data=df)
Df_fit_bk = stepAIC(result_bk, direction = "backward", trace = T)

result_fw = lm(df$Diabetes ~ 1, data=df)
Df_fit_fk = stepAIC(result_fw, direction = "forward", scope = df$Diabetes ~ Pregnancies + SkinThickness + Insulin + Age + Outcome, trace =  T)