data_wine <- read.csv("C:\\Users\\ji040\\OneDrive\\바탕 화면\\wine_quality_merged.csv", header = TRUE, na = ".", stringsAsFactors =  TRUE)

ggplot(data_wine, aes(x = data_wine$alcohol, y = data_wine$quality)) +
  geom_point(alpha = 0.8) +
  labs(title = "품질, 총 알코올 비교",
       x = "품질 정도", y = "총 알코올 농도")+
  theme_minimal()

model <- lm(data_wine$quality ~ data_wine$total.sulfur.dioxide, data = data_wine)
summary(model)

data_w <- select(data_wine, -c(type))
skim(df)

result <- lm(df$quality ~ ., data = df)
summary(result)