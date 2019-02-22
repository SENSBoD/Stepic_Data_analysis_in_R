# regression diagnostics
library(ggplot2)

swiss <- data.frame(swiss)
str(swiss)

# relationships between all variables
pairs(swiss)

library(PerformanceAnalytics)
chart.Correlation(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point()

# Outliers - викиди

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

# Normality of variables distributions - нормальність розподілу
ggplot(swiss, aes(x = Examination)) + 
  geom_histogram()

ggplot(swiss, aes(x = Education)) + # графік зсунутий вліво
  geom_histogram()
# Щоб вибравити, вільмем натуральний логарифм від Education
ggplot(swiss, aes(x = log(Education))) + 
  geom_histogram()

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)

hist(my_vector)

ggplot()+
  aes(my_vector)+
  geom_histogram()

ggplot()+ # розподіл стане нормальним
  aes(sqrt(my_vector))+
  geom_histogram()

# Функция scale() позволяет совершить стандартизацию вектора, то есть делает его среднее значение равным нулю, 
# а стандартное отклонение - единице (Z-преобразование). 
# Стандартизованный коэффициент регрессии (??) можно получить, если предикторы и зависимая переменная стандартизованы.
# Напишите функцию, которая на вход получает dataframe с двумя количественными переменными, 
# а возвращает стандартизованные коэффициенты для регрессионной модели, 
# в которой первая переменная датафрейма выступает в качестве зависимой, а вторая в качестве независимой.
beta.coef <- function(x){
  x_1 <- scale(x[[1]], center = TRUE, scale = TRUE)
  x_2 <- scale(x[[2]], center = TRUE, scale = TRUE)
  fit <- lm(x_1 ~ x_2, data = x)
  #fit$coefficients
  #return(c(x_1, x_2))
  return(c(fit$coefficients[[1]], fit$coefficients[[2]]))
}
beta.coef(mtcars[,c(1,3)])

beta.coef <- function(x){    
  x <-scale(x)    
  return(lm(x[,1] ~ x[,2])$coefficients)}

library(QuantPsyc)
lm.beta(lm(mtcars[,1] ~ mtcars[,2]))

# Напишите функцию normality.test, которая получает на вход dataframe с количественными переменными, 
# проверяет распределения каждой переменной на нормальность с помощью функции shapiro.test. 
# Функция должна возвращать вектор с значениями p - value, полученного в результате проверки на нормальность каждой переменной. 
# Названия элементов вектора должны совпадать с названиями переменных. 
normality.test  <- function(x){
  #sapply(x, shapiro.test(x))
  return(sapply(x, FUN =  shapiro.test)['p.value',])
}
#vector <- sapply(list, as.numeric) 
normality.test(mtcars[,1:6])


# linearity - перевірка на лінійний звязок
swiss <- data.frame(swiss)

library(ggplot2)
ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)

# побудуємо поліноміальну модель
swiss$Examination_squared <- (swiss$Examination)^2

lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)

anova(lm2, lm1)

swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- 1:nrow(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point(size = 3) + 
  geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd=1) +
  geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd=1)

# залишки для lm1
ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3) + geom_hline(yintercept = 0, col = 'red', lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) + geom_hline(yintercept = 0, col = 'red', lwd = 1)

# independence of errors - незалежність залишків
ggplot(swiss, aes(x = obs_number, y = lm1_resid)) + 
  geom_point(size = 3) + geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid)) + 
  geom_point(size = 3) + geom_smooth()

# Homoscedasticity - гомоскедантичність
ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3) + geom_smooth(method = "lm")

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) + geom_smooth(method = "lm")


# Функция gvlma() из библиотеки gvlma позволяет получить оценку 
# выполнения основных допущений линейной регрессии
library(gvlma)
my_df <- read.csv("https://stepic.org/media/attachments/lesson/12088/homosc.csv")
x <- gvlma(DV ~ IV, data = my_df)
summary(x)



# Errors Normally distributed - нормальність розподілу залишків
ggplot(swiss, aes(x = lm1_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)

#par(mfrow=c(2,2))
#plot(lm1)

ggplot(swiss, aes(x = lm2_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)

# Напишите функцию resid.norm, которая тестирует распределение остатков от модели на нормальность при помощи функции shapiro.test 
# и создает гистограмму при помощи функции ggplot() с красной заливкой "red", 
# если распределение остатков значимо отличается от нормального (p < 0.05), 
# и с зелёной заливкой "green" - если распределение остатков значимо не отличается от нормального. 
# На вход функция получает регрессионную модель. Функция возвращает переменную, в которой сохранен график ggplot.
resid.norm  <- function(fit){
  res <- shapiro.test(fit$residuals)
  if(res$p.value < 0.05){
    my_plot <- ggplot(fit$model, aes(x = fit$residuals)) + 
      geom_histogram(fill = 'red')
  }
  else{
    my_plot <- ggplot(fit$model, aes(x = fit$residuals)) + 
      geom_histogram(fill = 'green')
  }
  return(my_plot)
}
fit <- lm(mpg ~ wt, mtcars)
my_plot <- resid.norm(fit)
my_plot

fit <- lm(mpg ~ disp, mtcars)
my_plot <- resid.norm(fit)
my_plot

resid.norm <- function(fit) {    
  resid.norm.pv <- shapiro.test(fit$residuals)$p.value    
  plt <- ggplot(data.frame(fit$model), aes(x = fit$residuals)) +    
    geom_histogram(fill = ifelse(resid.norm.pv < 0.05, 'red', 'green'))    
  return(plt)}

# Ещё одной проблемой регрессионных моделей может стать мультиколлинеарность - ситуация, 
# когда предикторы очень сильно коррелируют между собой. Иногда корреляция между двумя предикторами может достигать 1, 
# например, когда два предиктора - это одна и та же переменная, измеренная в разных шкалах (x1 - рост в метрах, x2 - рост в сантиметрах)  
# Проверить данные на мультиколлинеарность можно по графику pairs() и посчитав корреляцию между всеми предикторами c помощью функции cor.
# Напишите функцию high.corr, которая принимает на вход датасет с произвольным числом количественных переменных и 
# возвращает вектор с именами двух переменных с максимальным абсолютным значением коэффициента корреляции . 
high.corr <- function(x){
  #pairs(x)
  corr_matrix <- cor(x)
  diag(corr_matrix) <- 0
  return(rownames(which(abs(corr_matrix) == (max(abs(corr_matrix))), arr.ind = TRUE)))
}
high.corr(swiss)
a <- cor(swiss)
diag(a) <- 0
a
a[which.max(abs(a))]
rownames(which(a == max(a), arr.ind = TRUE))

colnames(a)[1]
rownames(a)[2]

dimnames(a)

test_data <- as.data.frame(list(V1 = c(0.9, 1.2, -0.3, 0.2, 1.2, 0.1, 0.8, -1.9, 0.2, 0.4), V2 = c(0.6, 0, -1.2, 0, 1, -2, 0.3, 0.8, -1.8, 0.7), V3 = c(0.3, -0.8, 0.1, 0.7, -1, 0.2, 0.8, 0.2, -1, -1.1), V4 = c(1.3, -0.7, -0.3, -0.2, -1.5, 0.7, 0.1, -2.9, -2.6, 0.9), V5 = c(0.9, 0.8, 2, 0.6, -1.1, 0.6, -1.2, 1.3, -0.4, 0.6), V6 = c(-1.3, 0.7, 0.3, 0.2, 1.5, -0.7, -0.1, 2.9, 2.6, -0.9)))
a <- cor(test_data)
diag(a) <- 0
a
high.corr(test_data)

high.corr <- function(x){    
  cr <- cor(x)    
  diag(cr) <- 0    
  return(rownames(which(abs(cr)==max(abs(cr)),arr.ind=T)))}
