df <- data.frame(values = c(5, 7, 10, 12, 14, 18, 20, 22, 25, 27, 30))

ggplot(df, aes(x = values)) +
  geom_boxplot(binwidth = 5, fill = "steelblue", color = "black")+
  labs(title = "Boxplot of Values")+
  xlab("Values")+
  ylab("Frequency")
                 
            