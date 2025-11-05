data_w$Fitted <- fitted(data_w)
data_w$Residuals <- resid(data_w)

#선형성#
ggplot(data_w, aes(x = data_w$Fitted, y = data_w$Residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual vs Fitted (선형성 확인)", 
       x = "적합값 (Fitted Values)",
       y = "잔차 (Residuals)") +
  theme_minimal()


#등분산성#
data_w$StdRedis <- sqrt(abs(data_w$Residuals))

ggplot(data_w, aes(x = data_w$Fitted, y = data_w$Residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Scale-Location (등분산성 확인)", 
       x = "적합값 (Fitted Values)",
       y = "Standardized Residuals") +
  theme_minimal()

library(lmtest)
bptest(df)

#독립성#
durbinWatsonTest(result)

