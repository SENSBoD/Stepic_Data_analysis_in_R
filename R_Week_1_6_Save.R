getwd() # показує шлях до робочої директорії
# setwd(dir = "C:/Users/Администратор/Documents") # новий шлях до робочої директорії

library(psych)

df  <- mtcars

mean_mpg  <- mean(df$mpg)

descr_df  <- describe(df[,-c(8,9)])

my_boxplot  <- ggplot(df, aes(x = factor(am), y = disp))+
  geom_boxplot()+
  xlab("Transmission")+
  ylab("Displacement")+
  ggtitle("My boxplot")

write.csv(df, "df.csv")
write.csv(descr_df, "descr_df.csv")

my_mean  <- mean(10^6 : 10^7) # середнє для дуже великого значення
save(my_mean, file = "my_mean.RData")
