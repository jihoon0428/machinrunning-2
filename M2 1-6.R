grow <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/cafe.csv", stringsAsFactors = TRUE)

anova_result <- aov(Satisfaction ~ CoffeeType, data = grow)

summary(anova_result)

tukey_result <- glht(anova_result, linfct = mcp(CoffeeType = "Tukey"))

summary(tukey_result)