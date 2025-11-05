df$Fitted <- fitted(result)
df$Residuals <- resid(result)

#선형성#
ggplot(df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual vs Fitted (선형성 확인)", 
       x = "적합값 (Fitted Values)",
       y = "잔차 (Residuals)") +
  theme_minimal()


#등분산성#
df$StdRedis <- sqrt(abs(df$Residuals))

ggplot(df, aes(x = Fitted, y = StdRedis)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Scale-Location (등분산성 확인)", 
       x = "적합값 (Fitted Values)",
       y = "Standardized Residuals") +
  theme_minimal()

library(lmtest)
bptest(result)

#독립성#
durbinWatsonTest(result)

