df  <- mtcars

cor.test(x = df$mpg, y = df$hp) # по замовчуванню коеф.кореляції Пірсона
fit <- cor.test(x = df$mpg, y = df$hp)

cor.test(~ mpg + hp, df)

str(fit)

fit$p.value

plot(x = df$mpg, y = df$hp)

library(ggplot2)

ggplot(df, aes(hp, mpg, col = factor(cyl)))+
  geom_point(size = 3)

# попарний аналіз
df  <- mtcars
df_numeric  <- df[,c(1,3:7)] # тільки числові значення

pairs(df_numeric)

cor(df_numeric)
typeof(cor(df_numeric))

library(psych)
fit <- corr.test(df_numeric)
fit$r
fit$p

# Напишите функцию corr.calc, которая на вход получает data.frame с двумя количественными переменными, 
# рассчитывает коэффициент корреляции Пирсона и возвращает вектор из двух значений: коэффициент корреляции и p - уровень значимости.
corr.calc <- function(x){
  data <- as.data.frame(x)
  fit <- cor.test(x = data[,1], y = data[,2])
  return(c(fit$estimate[[1]], fit$p.value))
}
test_data <- as.data.frame(list(x1 = c(1, -1.6, 0.5, -1.3, -0.5, -0.3, 1.9, -1.2, 1.6, 0.8, 0.9, 0.6, 1.1, -0.2, -1.4, 0.3, -0.1, 0, 0.5, 2), y1 = c(0.1, -0.8, -0.7, -0.7, 0.3, 0.2, 1.2, -1.5, 0, 0.9, -0.4, 0.3, 1.5, 0.7, 1.4, 1, 0.7, 0.5, 0.9, 0.4)))
corr.calc(test_data)

corr.calc <- function(test_data){    
  fit  <- cor.test(test_data[[1]], test_data[[2]])    
  r <- fit$estimate    
  p <- fit$p.value    
  return(c(r, p))}

fit <- cor.test(x = test_data$x1, y = test_data$y1)
fit$statistic
fit$p.value
fit$estimate
vector_1 <-c(fit$statistic[[1]], fit$p.value) 

# Напишите функцию filtered.cor которая на вход получает data.frame с  произвольным количеством переменных (как количественными, 
# так и любых других типов), рассчитывает коэффициенты корреляции Пирсона между всеми парами количественных переменных и 
# возвращает наибольшее по модулю значение коэффициента корреляции. 
# (То есть функция может вернуть -0.9, если это наибольшая по модулю  корреляция).
# Гарантируется наличие в data.frame хотя бы двух количественных переменных.
filtered.cor <- function(x){
  numeric_data <- Filter(is.numeric, x)
  a <- cor(numeric_data)
  for(i in 1:nrow(a)){
    for(j in 1:ncol(a)){
      if(i == j){
        a[i, j] <- 0
      }
    }
  }
  max_cur <- which.max(a)
  min_cur <- which.min(a)
  if(abs(a[min_cur]) > abs(a[max_cur])){
    max <- a[min_cur]
    return(max)
  }
  else{
    max <- a[max_cur]
    return(max)
  }
}

filtered.cor <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))    
  cor_mat <- cor(x[, num_var])    
  diag(cor_mat) <- 0    
  return(cor_mat[which.max(abs(cor_mat))])}

# Напишите функцию smart_cor, которая получает на вход dataframe с двумя количественными переменными. 
# Проверьте с помощью теста Шапиро-Уилка, что данные в обеих переменных принадлежат нормальному распределению.
# Если хотя бы в одном векторе распределение переменной отличается от нормального (p - value меньше 0.05), 
# то функция должна возвращать коэффициент корреляции Спирмена. (Числовой вектор из одного элемента).
# Если в обоих векторах распределение переменных от нормального значимо не отличается, 
# то функция должна возвращать коэффициент корреляции Пирсона.
smart_cor <- function(x){
  norm_test_1 <- shapiro.test(x[[1]])
  norm_test_2 <- shapiro.test(x[[2]])
  if(norm_test_1$p.value <= 0.05 | norm_test_2$p.value <= 0.05){
    fit  <- cor.test(x[[1]], x[[2]], method = "spearman")
    return(fit$estimate)
  }
  else{
    fit  <- cor.test(x[[1]], x[[2]]) 
    return(fit$estimate)
  }
}

# regression
df  <- mtcars
df_numeric  <- df[,c(1,3:7)]

fit <- lm(mpg ~ hp, df)
fit

summary(fit)

ggplot(df, aes(cyl, mpg))+
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  theme(axis.text=element_text(size = 25),
       axis.title=element_text(size = 25, face = "bold"))

ggplot(df, aes(hp, mpg))+
  geom_point(size = 3)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

# predict
fitted_values_mpg  <- data.frame(mpg = df$mpg, fitted = fit$fitted.values )
new_hp <- data.frame(hp = c(100, 150, 129, 300))

predict(fit, new_hp)
new_hp$mpg  <- predict(fit, new_hp)

# лінійна регресія, з номінативний предиктором
my_df <- mtcars

str(my_df)

fit  <- lm(mpg ~ cyl, my_df)
summary(fit)

my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))

fit  <- lm(mpg ~ cyl, my_df)
summary(fit)

aggregate(mpg ~ cyl, my_df, mean)

#Exercises

# Скачайте набор данных - dataframe с двумя количественными переменными (вспомните при необходимости, 
# как задавать разделитель и другие параметры функции read.table), постройте линейную регрессию, 
# где - первая переменная - зависимая, вторая - независимая. 
# В ответ укажите значения регрессионных коэффициентов сначала intercept затем  slope.
df_2 <- read.csv('C:/Users/Администратор/Downloads/dataset_11508_12.txt', sep = ' ')
df_2
fit  <- lm(X0.188 ~ X4.467, df_2)
summary(fit)

# Воспользуемся уже знакомыми данными diamonds из библиотеки ggplot2. 
# Только для бриллиантов класса Ideal (переменная cut) c числом карат равным 0.46 (переменная carat) постройте линейную регрессию, 
# где в качестве зависимой переменной выступает price, в качестве предиктора - переменная  depth. 
# Сохраните коэффициенты регрессии в переменную fit_coef.
df <- diamonds
df_ideal <- subset(df, df$cut == 'Ideal' & df$carat == 0.46)
fit <- lm(price ~ depth, df_ideal)
fit_coef <- fit$coefficients

# Напишите функцию regr.calc, которая на вход получает dataframe c двумя переменными.
# Если две переменные значимо коррелируют (p - уровень значимости для коэффициента корреляции Пирсона меньше 0.05), 
# то функция строит регрессионную модель, где первая переменная - зависимая, вторая - независимая. 
# Затем создает в dataframe новую переменную с назанием fit, где сохраняет предсказанные моделью значения зависимой переменной. 
# В результате функция должна возвращать исходный dataframe с добавленной новой переменной fit.
# Если две переменные значимо не коррелируют, то функция возвращает строчку "There is no sense in prediction"
regr.calc <- function(x){
  fit_cor  <- cor.test(x[[1]], x[[2]])
  if(fit_cor$p.value <= 0.05){
    fit <- lm(x[[1]] ~ x[[2]], x)
    x$fit <- predict(fit, x[1])
    return(x)
  }
  else{
    return("There is no sense in prediction")
  }
}

my_df = iris[,c(1,4)]
my_df = iris[,1:2]
regr.calc(my_df)

my_df$fit <- NULL
my_df$fit <- predict(fit, my_df)

# Постройте scatterplot по данным iris, сохранив его в переменную my_plot : 
# Ось X - переменная Sepal.Width
# Ось Y -  переменная Petal.Width
# Цвет точек - переменная Species
# Также добавьте линейное сглаживание для каждой группы наблюдений по переменной Species.
df <- iris
ggplot(df, aes(Sepal.Width, Petal.Width, col = Species))+
  geom_point(size = 3)+
  geom_smooth(method = "lm")
