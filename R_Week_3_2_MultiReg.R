# multiple linear regression

?swiss

swiss <- data.frame(swiss)
str(swiss)
hist(swiss$Fertility, col = 'red')

# numeric predictors (РїРµСЂРµР»Р±Р°С‡Р°С”РјРѕ РЅР°СЂРѕРґР¶СѓРІР°РЅС–СЃС‚СЊ)
fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)

fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)

confint(fit2) # РґРѕРІС–СЂС‡С– С–РЅС‚РµСЂРІР°Р»Рё РґР»СЏ РјРѕРґРµР»С–

# Напишите функцию fill_na, которая принимает на вход данные с тремя переменными:
# x_1  -  числовой вектор
# x_2 - числовой вектор
# y - числовой вектор с пропущенными значениями.
# Теперь — самое интересное. На первом этапе, используя только наблюдения, в которых нет пропущенных значений, 
# мы построим регрессионную модель (без взаимодействий), где  y — зависимая переменная, x_1 и x_2 — независимые переменные. 
# Затем, используя построенную модель, мы заполним пропущенные значения предсказаниями модели.
# Функция должна возвращать dataframe c новой переменной  y_full. Сохраните в нее переменную y, 
# в которой пропущенные значения заполнены предсказанными значениями построенной модели.
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
fill_na <- function(x){
  x_known <- subset(x, y != "NA")
  fit <- lm(y ~ x_1 + x_2, data = x_known)
  x$y_full <- ifelse(!is.na(x$y), x$y, predict(fit, x))
  return(x)
}
fill_na(test_data)

# В переменной df сохранен subset данных mtcars только с переменными "wt", "mpg", "disp", "drat", "hp". 
# Воспользуйтесь множественным регрессионным анализом, чтобы предсказать вес машины (переменная "wt"). 
# Выберите такую комбинацию независимых переменных (из "mpg", "disp", "drat", "hp"), чтобы значение R^2 adjusted было наибольшим. 
# Взаимодействия факторов учитывать не надо. 
df <- mtcars[, c("wt", "mpg", "disp", "drat", "hp")]

fit <- lm(wt ~ mpg + disp + drat + hp, data = df)
summary(fit)

fit2 <- lm(wt ~ mpg + disp + hp, data = df)
summary(fit2)

#library(corrplot)
#M <- cor(df)
#corrplot(M)

library(PerformanceAnalytics)
chart.Correlation(df)

library(leaps)
x <- regsubsets(wt ~ mpg + disp + drat + hp, df)
plot(x, scale = "adjr2")                
                
# Воспользуйтесь встроенным датасетом attitude, чтобы предсказать рейтинг (rating) по переменным complaints и critical. 
# Каково t-значение для взаимодействия двух факторов?
?attitude
df2 <- attitude
fit3 <- lm(rating ~ complaints * critical, data = df2)
summary(fit3)


# categorical predictors
hist(swiss$Catholic, col = 'red')
swiss$religious <- ifelse(swiss$Catholic > 60, 'Lots', 'Few')
swiss$religious <- as.factor(swiss$religious)

fit3 <- lm(Fertility ~ Examination + religious, data = swiss)
summary(fit3)

fit4 <- lm(Fertility ~ religious*Examination, data = swiss)
summary(fit4)

library(ggplot2)
ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  #geom_smooth()
  geom_smooth(method = 'lm')

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss)
summary(fit5)

# Визуализируйте взаимодействие переменных wt и am, дополнив код, приведённый в задании:
# Ось x - переменная wt
#О сь y - переменная mpg
# Цвет регрессионных прямых - переменная am
df3 <- mtcars
df3$am <- factor(df3$am, labels = c('Automatic', 'Manual'))

fit6 <- lm(mpg ~ wt*am, data = df3)
summary(fit6)

my_plot <- ggplot(df3, aes(y = mpg, x = wt, col = am))+
  #geom_point()+
  geom_smooth(method = 'lm')

# model comparison
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss) # РІСЃС– РїСЂРµРґРёРєС‚РѕСЂРё (+)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)

# model selection
optimal_fit <-  step(fit_full, direction = 'backward')
summary(optimal_fit)

# C помощью функции step найдите оптимальную модель для предсказания rating в датасете attitude. Model_full и model_null уже созданы. 
# Сохраните команду с функцией step в переменную ideal_model.
attitude <- data.frame(attitude)

model_full <- lm(rating ~ ., data = attitude)
model_null <- lm(rating ~ 1, data = attitude)

ideal_model <- step(model_full, direction = 'backward')
summary(ideal_model)

# Сравните полную модель из предыдущего степа и оптимальную модель с помощью функции anova. Введите получившееся F-значение.
anova(model_full, ideal_model)

# Напоследок потренируемся в эффективном написании формул. В этой задаче будем работать со встроенным датасетом LifeCycleSavings. 
# Попытаемся предсказать значение sr на основе всех остальных переменных в этом датасете. 
# Вспомните способы сокращения формул и напишите команду, 
# которая создаёт линейную регрессию с главными эффектами и всеми возможными взаимодействиями второго уровня. 
# Сохраните модель в переменную model.
LifeCycleSavings <- data.frame(LifeCycleSavings)
model <- lm(sr ~ (.)^2, LifeCycleSavings)
