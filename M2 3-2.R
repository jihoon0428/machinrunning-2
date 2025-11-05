train_data <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\자료\\3주차_데이터\\simple_train_data.csv")

library(ggplot2)
ggplot(train_data, aes(x = thigh, y = upper_arm)) +
  geom_point(alpha = 0.7) +
  labs(title = "대퇴부 vs 상박부 산점도", 
       x = "대퇴부 둘레 (thigh, cm)",
       y = "상박부 둘레 (upper_arm, cm)") +
  theme_minimal()

model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

residuals(model)