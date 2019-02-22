df  <- mtcars
df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))

hist(df$mpg)
hist(df$mpg, breaks = 20)
hist(df$mpg, breaks = 20, xlab = "MPG")
hist(df$mpg, breaks = 20, xlab = "MPG", main ="Histogram of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

boxplot(mpg ~ am, df)
boxplot(hp ~ am, df)
boxplot(mpg ~ am, df, ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)

boxplot(df$mpg[df$am == "Auto"], df$mpg[df$am == "Manual"], ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)

plot(df$mpg, df$hp)
plot(df$mpg, df$hp, xlab = "MPG", ylab ="HP" , main ="MPG and HP", pch = 22)

plot(density(df$mpg), xlab = "MPG", main ="Density of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

# Library ggplot2
library(ggplot2)

ggplot(df, aes(x = mpg))+
  geom_histogram(fill = "white", col = "black", binwidth = 2)

ggplot(df, aes(x = mpg))+
  geom_histogram(fill = "green", col = "black", binwidth = 2)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  ggtitle("MPG histogram")

ggplot(df, aes(x = mpg))+
  geom_dotplot(binwidth = 1.0)

ggplot(df, aes(x = mpg, fill = am))+
  geom_dotplot(binwidth = 1.0)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG dotplot")

ggplot(df, aes(x = mpg))+
  geom_density(fill = "red")

ggplot(df, aes(x = mpg, fill = am))+
  geom_density(alpha = 0.5)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG density plot")

ggplot(df, aes(x = am, y = hp))+
  geom_boxplot()

ggplot(df, aes(x = am, y = hp, col = vs))+
  geom_boxplot()

ggplot(df, aes(x = am, y = hp, fill = vs))+
  geom_boxplot()+
  xlab("Transmission type")+
  ylab("Gross horsepower")+
  scale_fill_discrete(name="Engine type")+
  ggtitle("Gross horsepower and engine type")

ggplot(df, aes(x = mpg, y = hp))+
  geom_point(size = 3)

ggplot(df, aes(x = mpg, y = hp, col = vs))+
  geom_point(size = 3)

ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
  geom_point()

my_plot  <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
  geom_point()
my_plot

my_plot2  <- ggplot(df, aes(x = am, y = hp, fill = vs))
my_plot2 + geom_boxplot()

# Exercises

df <- airquality
ggplot(df, aes(x = as.factor(Month), y = Ozone, group = Month))+
  geom_boxplot()
boxplot(Ozone ~ Month, df)

# Используем знакомые нам данные mtcars. 
# Нужно построить scatterplot с помощью ggplot из ggplot2, по оси x которого будет mpg, по оси y - disp, а цветом отобразить переменную (hp).
# Полученный график нужно сохранить в переменную plot1.
df <- mtcars
plot1 <- ggplot(df, aes(x = mpg, y = disp, col = hp))+
  geom_point()
plot1

df <- iris
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))
