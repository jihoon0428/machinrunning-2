df <-data.frame(x=c(1, 2, 3, 4, 5), y=c(6, 8, 5, 7, 9))
df2 <-data.frame(x=c(5, 6, 7, 8, 9), y=c(18, 12, 16, 77, 63))

ggplot()+
  geom_point(data = df, aes(x=x, y=y), color = "blue", size=3)+
  geom_line(data = df, aes(x=x, y=y, color = "Connected Points"), size = 0.5)+
  geom_point(data = df2, aes(x=x, y=y), color = "blue", size=3)+
  geom_line(data =df2, aes(x=x, y=y, color = "Connected Points"), size = 0.5)+
  labs(title = "Scatter Plot")+
  xlab("X")+
  ylab("Y")

