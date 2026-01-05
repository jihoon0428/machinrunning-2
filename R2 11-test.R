df_test <- read.csv("C:/Users/ji040/OneDrive/바탕 화면/자료/UniversalBank/UniversalBank_test.csv", header=TRUE)

df_test <- select(df_test, -c(Education, Age, Income))

