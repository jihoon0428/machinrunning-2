df <-data.frame(x=c(1, 2, 3, 4, 5), y=c(6, 8, 5, 7, 9))

ggplot(df, aes(x=x, y=y)) +
  geom_point(color = "blue", size=3)+
  geom_line(aes(color = "Connected Points"), size = 0.5)+
  xlab("X")+
  ylab("Y")

